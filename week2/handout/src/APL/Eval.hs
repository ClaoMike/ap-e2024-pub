module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
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

newtype EvalM a = EvalM (Env -> Either Error a)

instance Functor EvalM where
  fmap f (EvalM x) = EvalM $ \env ->
    case x env of
      Right x' -> Right $ f x'
      Left err -> Left err

instance Applicative EvalM where
  pure x = EvalM (\env -> Right x)
  EvalM f <*> EvalM x = EvalM $ \env ->
     case (f env, x env) of 
      (Right f, Right x) -> Right $ f x
      (Left err, _) -> Left err
      (_, Left err) -> Left err

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
          in y env

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

failure :: String -> EvalM a
failure error = EvalM $ \_env -> Left error

evalHelper :: Exp -> (Integer -> Integer -> Integer) -> Exp -> Error -> EvalM Val
evalHelper e1 f e2 err = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt a, ValInt b) -> pure $ ValInt $ f a b
    (_, _) -> failure err

eval :: Exp -> EvalM Val
eval (CstInt e) = pure $ ValInt e
eval (CstBool e) = pure $ ValBool e
eval (Add e1 e2) = evalHelper e1 (+) e2 "failed addition"
eval (Sub e1 e2) = evalHelper e1 (-) e2 "failed substraction"
eval (Mul e1 e2) = evalHelper e1 (*) e2 "failed multiplication"
eval (Div e1 e2) = evalHelper e1 div e2 "failed division"
eval (Pow e1 e2) = evalHelper e1 (^) e2 "failed power"
eval (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (_, _) -> failure "not compatible"
eval (If e1 e2 e3) = do
  x <- eval e1
  case x of
    ValBool True -> eval e2
    ValBool False -> eval e3
    _ -> failure "poor condition"
eval (Var var) = do
  env <- askEnv
  case envLookup var env of
    Just x -> pure x
    Nothing -> failure "not found"
eval (Let var e1 e2) = do
  x <- eval e1
  localEnv (envExtend var x) $ eval e2

eval (Lambda var e1) = do
  env <- askEnv
  pure  (ValFun env var e1)
      
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"

eval (TryCatch e1 e2) = catch (eval e1) (eval e2)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM x) (EvalM y) = EvalM $ \env ->
  case x env of
    Right x -> Right x
    _ -> y env