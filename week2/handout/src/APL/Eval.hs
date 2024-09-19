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

-- a simple data type EvalM and its constructor
newtype EvalM a = EvalM (Either Error a) 

instance Functor EvalM where
  fmap f (EvalM(Right x)) = EvalM (Right (f x))
  fmap _ (EvalM(Left err)) = EvalM(Left err)

instance Applicative EvalM where
  pure x = EvalM(Right x)
  -- (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b 
  EvalM(Left err) <*> _ = EvalM(Left err)
  _ <*> EvalM(Left err) = EvalM(Left err)
  EvalM(Right f) <*> x = fmap f x

instance Monad EvalM where
  EvalM(Left err) >>= _ = EvalM(Left err)
  EvalM(Right x) >>= f = f x

runEval :: EvalM a -> Either Error a
runEval x = case x of
  EvalM(Right x') -> Right x'
  EvalM(Left err) -> Left err

failure :: String -> EvalM a
failure s = EvalM(Left s)

eval :: Env -> Exp -> EvalM Val
eval = undefined -- TODO