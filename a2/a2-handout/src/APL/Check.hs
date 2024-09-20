module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String
type Env = [VName]

newtype CheckM a = CheckM (Env -> Either Error a)

-- instance Functor CheckM where

-- instance Applicative CheckM where

-- instance Monad CheckM where

check :: Exp -> CheckM ()
check = undefined

checkExp :: Exp -> Maybe Error
checkExp = undefined
