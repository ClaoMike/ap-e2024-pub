module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    envEmpty
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

-- a simple data type EvalM and its constructor
newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  EvalM ef <*> EvalM ex = EvalM $ \env ->
    case (ef env, ex env) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right f, Right x) -> Right (f x)

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m envEmpty

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x

eval (Add e1 e2) = evalIntHelper e1 (+) e2 "Add failed"
eval (Sub e1 e2) = evalIntHelper e1 (-) e2 "Sub failed"
eval (Mul e1 e2) = evalIntHelper e1 (*) e2 "Mul failed"
eval (Div e1 e2) = evalDivHelper e1 e2 "Div failed"
eval (Pow e1 e2) = evalPowHelper e1 e2 "Pow failed"

eval (Eql e1 e2) = evalEqlHelper e1 e2 "Eql failed"
eval (If e1 e2 e3) = evalIfHelper e1 e2 e3

eval (Var var) = do
  env <- askEnv
  case envLookup var env of
    Just x -> pure x
    _ -> failure "Var not found"

eval (Let var e1 e2) = do
  x <- eval e1
  case x of
    ValInt x' -> localEnv (envExtend var x) (eval e2)
    ValBool x' -> localEnv (envExtend var x) (eval e2)
    _ -> failure "Let failed"

eval (Lambda var e1) = do
  env <- askEnv
  pure (ValFun env var e1)

eval (Apply e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValFun env' var e3, ValInt y') -> localEnv (envExtend var y) (eval e2)
    (ValFun env' var e3, ValBool y') -> localEnv (envExtend var y) (eval e2)
    (_, _) -> failure "Incorrect apply"

eval (TryCatch e1 e2) = eval e1 `catch` eval e2

evalIntHelper :: Exp -> (Integer -> Integer -> Integer) -> Exp -> Error -> EvalM Val
evalIntHelper e1 op e2 err =  do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ op x' y'
    _ -> failure err

evalDivHelper :: Exp -> Exp -> Error -> EvalM Val
evalDivHelper e1 e2 err =  do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (_, ValInt 0) -> failure "Division by 0"
    (ValInt x', ValInt y') -> pure $ ValInt $ div x' y'
    _ -> failure err

evalPowHelper :: Exp -> Exp -> Error -> EvalM Val
evalPowHelper e1 e2 err =  do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> if y'<0 then failure "Negative power" else pure $ ValInt $ (^) x' y'
    _ -> failure err

evalEqlHelper :: Exp -> Exp -> Error -> EvalM Val
evalEqlHelper e1 e2 err =  do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    _ -> failure err

evalIfHelper :: Exp -> Exp -> Exp -> EvalM Val
evalIfHelper e1 e2 e3 = do
  x <- eval e1
  case x of
    ValBool True -> eval e2
    ValBool False -> eval e3
    _ -> failure "Non-boolean conditional."

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x