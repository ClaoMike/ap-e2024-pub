module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String
type Env = [VName]

newtype CheckM a = CheckM (Env -> Either Error a)

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_ -> Right x
    (<*>) = ap

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      Left err -> Left err
      Right x' -> let CheckM y = f x' in y env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

check :: Exp -> CheckM ()
check (CstInt _) = return ()
check (CstBool _) = return ()
check (Add e1 e2) = do
  check e1
  check e2
check (Sub e1 e2) = do
  check e1
  check e2
check (Mul e1 e2) = do
  check e1
  check e2
check (Div e1 e2) = do
  check e1
  check e2
check (Pow e1 e2) = do
  check e1
  check e2
check (Eql e1 e2) = do
  check e1
  check e2
check (If cond e1 e2) = do
  check cond
  check e1
  check e2
check (Apply e1 e2) = do
  check e1
  check e2
check (TryCatch e1 e2) = do
  check e1
  check e2

checkExp :: Exp -> Maybe Error
checkExp = undefined
