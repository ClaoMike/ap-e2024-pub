module APL.Eval
  (
    Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)

type Error = String
type Env = [(VName, Val)]

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving(Eq, Show)

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend var val env = (var, val):env 

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup var env = lookup var env

evalHelper :: Env -> Exp -> (Integer -> Integer -> Integer) -> Exp -> Either Error Val
evalHelper env a f b = do
  case (eval env a, eval env b) of
    (Right (ValInt e1), Right (ValInt e2)) -> Right( ValInt(e1 `f` e2) )
    (Left error1, _) -> Left error1
    (_, Left error2) -> Left error2

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt a) = Right $ ValInt a
eval _ (CstBool a) = Right $ ValBool a
eval env (Add a b) = evalHelper env a (+) b
eval env (Sub a b) = evalHelper env a (-) b
eval env (Mul a b) = evalHelper env a (*) b
eval env (Div a b) = do
  case (eval env a, eval env b) of
    (_, Right (ValInt 0)) -> Left "division by 0"
    (Right (ValInt e1), Right (ValInt e2)) -> Right( ValInt(e1 `div` e2) )
    (Left error1, _) -> Left error1
    (_, Left error2) -> Left error2
eval env (Pow a b) = do
  case (eval env a, eval env b) of
    (Right (ValInt e1), Right (ValInt e2)) -> do
      if e2 < 0 then Left "negative power" else Right( ValInt(e1 ^ e2) )
    (Left error1, _) -> Left error1
    (_, Left error2) -> Left error2
eval env (Eql a b) = do
  case (eval env a, eval env b) of
    (Right (ValInt e1), Right (ValInt e2)) -> if e1 ==e2 then Right( ValBool True ) else Right( ValBool( False ) )
    (Right (ValInt _), Right (ValBool _)) -> Left "unmatching types"
    (Right (ValBool _), Right (ValInt _)) -> Left "unmatching types"
    (Left error1, _) -> Left error1
    (_, Left error2) -> Left error2
eval env (If a b c) = do
  case eval env a of
    Right(ValBool e1) -> if e1 then eval env b else eval env c
    Right(ValInt _) -> Left "condition is non-boolean"
    Left error -> Left error

eval env (Var var) = do
  case envLookup var env of
    Nothing -> Left "not found"
    Just x -> Right x

eval env (Let var e1 e2) = do
  case eval env e1 of
    Left error -> Left error
    Right a -> do
      let env' = envExtend var a env
      case eval env' e2 of
        Right a -> Right a
        Left error -> Left error