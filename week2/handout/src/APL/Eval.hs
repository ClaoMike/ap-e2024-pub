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

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

eval :: Env -> Exp -> EvalM Val
eval _ (CstInt x) = pure $ ValInt x
eval _ (CstBool x) = pure $ ValBool x

eval env (Add e1 e2) = evalIntHelper env e1 (+) e2 "Add failed"
eval env (Sub e1 e2) = evalIntHelper env e1 (-) e2 "Sub failed"
eval env (Mul e1 e2) = evalIntHelper env e1 (*) e2 "Mul failed"
eval env (Div e1 e2) = evalDivHelper env e1 e2 "Div failed"
eval env (Pow e1 e2) = evalPowHelper env e1 e2 "Pow failed"

eval env (Eql e1 e2) = evalEqlHelper env e1 e2 "Eql failed"
eval env (If e1 e2 e3) = evalIfHelper env e1 e2 e3

eval env (Var var) = do
  case envLookup var env of
    Just x -> pure x
    _ -> failure "Var not found"

eval env (Let var e1 e2) = do
  x <- eval env e1
  case x of
    ValInt x' -> eval (envExtend var x env) e2
    ValBool x' -> eval (envExtend var x env) e2
    _ -> failure "Let failed"

eval env (Lambda var e1) = pure (ValFun env var e1)
eval env (Apply e1 e2) = do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValFun env' var e3, ValInt y') -> eval (envExtend var y env') e3
    (ValFun env' var e3, ValBool y') -> eval (envExtend var y env') e3
    (_, _) -> failure "Incorrect apply"

eval env (TryCatch e1 e2) = eval env e1 `catch` eval env e2

evalIntHelper :: Env -> Exp -> (Integer -> Integer -> Integer) -> Exp -> Error -> EvalM Val
evalIntHelper env e1 op e2 err =  do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ op x' y'
    _ -> failure err

evalDivHelper :: Env -> Exp -> Exp -> Error -> EvalM Val
evalDivHelper env e1 e2 err =  do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (_, ValInt 0) -> failure "Division by 0"
    (ValInt x', ValInt y') -> pure $ ValInt $ div x' y'
    _ -> failure err

evalPowHelper :: Env -> Exp -> Exp -> Error -> EvalM Val
evalPowHelper env e1 e2 err =  do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValInt x', ValInt y') -> if y'<0 then failure "Negative power" else pure $ ValInt $ (^) x' y'
    _ -> failure err

evalEqlHelper :: Env -> Exp -> Exp -> Error -> EvalM Val
evalEqlHelper env e1 e2 err =  do
  x <- eval env e1
  y <- eval env e2
  case (x, y) of
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    _ -> failure err

evalIfHelper :: Env -> Exp -> Exp -> Exp -> EvalM Val
evalIfHelper env e1 e2 e3 = do
  x <- eval env e1
  case x of
    ValBool True -> eval env e2
    ValBool False -> eval env e3
    _ -> failure "Non-boolean conditional."

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x