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
type KVP = (Val, Val)
type State = ([String], [KVP])

newtype EvalM a = EvalM (Env -> State -> (Either Error a, State))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env state -> (Right x, state)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state ->
    case x env state of
      (Left err, state') -> (Left err, state')
      (Right x', state') ->
        let EvalM y = f x'
         in y env state'

askEnv :: EvalM Env
askEnv = EvalM $ \env state -> (Right env, state)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env state-> (Left s, state )

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state->
  case m1 env state of
    (Left _, state') -> m2 env state'
    (Right x, state') -> (Right x, state')

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) = do
  case m envEmpty ([], [])  of
    (Right x, state) -> (fst state, Right x)
    (Left err, state) -> (fst state, Left err)

evalPrint :: String -> EvalM()
evalPrint str = EvalM $ \env (state, kvp) -> (Right(), (state ++ [str], kvp))

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2

eval (Print str e) = do
  v <- eval e
  case v of
    ValInt x -> evalPrint (str ++ ": " ++ show x) -- if v is integer
    ValBool x -> evalPrint (str ++ ": " ++ show x) -- if v is bool
    ValFun _ _ _ -> evalPrint (str ++ ": " ++ "#<fun>") -- if v is function
  return v

-- TODO
eval (KvPut e1 e2) = do
  k <- eval e1
  v <- eval e2
  -- record (p, v) in store
  -- if there is an association of k, replace it with the new one
  return v

-- TODO
eval (KvGet e) = do
  k <- eval e
  -- search the store and return the value
  -- no value -> return Left
  undefined

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = undefined

evalKvGet :: Val -> EvalM Val
evalKvGet = undefined

