module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExpHelper :: Exp -> String -> Exp -> String
printExpHelper a f b = addParantheses $ printExp a ++ f ++ printExp b

addParantheses:: String -> String
addParantheses s = "(" ++ s ++ ")"

printExp :: Exp -> String
printExp (CstInt v) = addParantheses $ show v
printExp (CstBool v) = addParantheses $ show v
printExp (Add a b) = printExpHelper a " + " b
printExp (Sub a b) = printExpHelper a " - " b
printExp (Mul a b) = printExpHelper a " * " b
printExp (Div a b) = printExpHelper a " / " b
printExp (Pow a b) = printExpHelper a " ** " b
printExp (Eql a b) = printExpHelper a " == " b
printExp (If a b c) = addParantheses $ "if " ++ printExp a ++ " then " ++ printExp b ++ " else " ++ printExp c
printExp (Let var a b) = addParantheses $ "let " ++ var ++ " = " ++ printExp a ++ " in " ++ printExp b
printExp (Apply a b) = addParantheses $ printExp a ++ " " ++ printExp b
printExp (TryCatch a b) = addParantheses $ "try " ++ printExp a ++ " catch " ++ printExp b
