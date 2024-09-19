module APL.Eval
  (
    eval, Val(..), Error, envEmpty
  )
where

import APL.AST (Exp(..), VName)
data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String
type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend var val env = (var, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup var env = lookup var env

eval :: Env -> Exp -> Either Error Val
eval _ (CstInt x) = Right $ ValInt x
eval _ (CstBool x) = Right $ ValBool x

eval env (Eql x y) = do
  case (eval env x, eval env y) of
    (Right(ValBool i1), Right(ValBool i2)) -> if i1==i2 then Right (ValBool True) else Right (ValBool False)
    (_, _) -> Left "Operation has failed"

eval env (If x y z) = do
  case eval env x of
    Right(ValBool i1) -> eval env y
    Right _ -> Left "Operation has failed"
    _ -> eval env z

eval env (Add x y) = evalHelper (+) x y env
eval env (Sub x y) = evalHelper (-) x y env
eval env (Mul x y) = evalHelper (*) x y env
eval env (Div x y) = evalSafeDiv x y env
eval env (Pow x y) = evalSafePow x y env

evalHelper :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Env -> Either Error Val
evalHelper op x y env = do
  case (eval env x, eval env y) of
    (Right(ValInt i1), Right(ValInt i2)) -> Right $ ValInt $ op i1 i2
    (_, _) -> Left "Operation has failed"

evalSafeDiv :: Exp -> Exp -> Env -> Either Error Val
evalSafeDiv x y env = do
  case (eval env x, eval env y) of
    (Right(ValInt i1), Right(ValInt i2)) -> if i2 == 0 then Left "Division by 0" else Right $ ValInt $ div i1 i2
    (_, _) -> Left "Operation has failed"

evalSafePow :: Exp -> Exp -> Env -> Either Error Val
evalSafePow x y env = do
  case (eval env x, eval env y) of
    (Right(ValInt i1), Right(ValInt i2)) -> if i2 < 0 then Left "Negative power" else Right $ ValInt $ (^) i1 i2
    (_, _) -> Left "Operation has failed"