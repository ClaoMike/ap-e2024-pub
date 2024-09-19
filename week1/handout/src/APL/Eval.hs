module APL.Eval
  (
    eval, Val(..), Error
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

eval :: Exp -> Either Error Val
eval (CstInt x) = Right $ ValInt x
eval (CstBool x) = Right $ ValBool x

eval (Eql x y) = do
  case (eval x, eval y) of
    (Right(ValBool i1), Right(ValBool i2)) -> if i1==i2 then Right (ValBool True) else Right (ValBool False)
    (_, _) -> Left "Operation has failed"

eval (If x y z) = do
  case eval x of
    Right(ValBool i1) -> eval y
    Right _ -> Left "Operation has failed"
    _ -> eval z

eval (Add x y) = evalHelper (+) x y
eval (Sub x y) = evalHelper (-) x y
eval (Mul x y) = evalHelper (*) x y
eval (Div x y) = evalSafeDiv x y
eval (Pow x y) = evalSafePow x y

evalHelper :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Either Error Val
evalHelper op x y = do
  case (eval x, eval y) of
    (Right(ValInt i1), Right(ValInt i2)) -> Right $ ValInt $ op i1 i2
    (_, _) -> Left "Operation has failed"

evalSafeDiv :: Exp -> Exp -> Either Error Val
evalSafeDiv x y = do
  case (eval x, eval y) of
    (Right(ValInt i1), Right(ValInt i2)) -> if i2 == 0 then Left "Division by 0" else Right $ ValInt $ div i1 i2
    (_, _) -> Left "Operation has failed"

evalSafePow :: Exp -> Exp -> Either Error Val
evalSafePow x y = do
  case (eval x, eval y) of
    (Right(ValInt i1), Right(ValInt i2)) -> if i2 < 0 then Left "Negative power" else Right $ ValInt $ (^) i1 i2
    (_, _) -> Left "Operation has failed"