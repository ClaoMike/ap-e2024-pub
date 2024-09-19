-- module APL.AST
--   ( VName,
--     Exp (..),
--     printExp,
--   )
module AST
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
  -- TODO: add cases
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt x) = show x
printExp (CstBool b) = if b then "True" else "False"
printExp (Var v) = v

printExp (Add e1 e2) = "(" ++ printExp e1 ++ " + " ++ printExp e2 ++ ")"
printExp (Sub e1 e2) = "(" ++ printExp e1 ++ " - " ++ printExp e2 ++ ")"
printExp (Mul e1 e2) = "(" ++ printExp e1 ++ " * " ++ printExp e2 ++ ")"
printExp (Div e1 e2) = "(" ++ printExp e1 ++ " / " ++ printExp e2 ++ ")"
printExp (Pow e1 e2) = "(" ++ printExp e1 ++ " ^ " ++ printExp e2 ++ ")"
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ " == " ++ printExp e2 ++ ")"

printExp (If cond e1 e2) = "if " ++ printExp cond ++ " then " ++ printExp e1 ++ " else " ++ printExp e2

printExp (Let v e1 e2) = "let " ++ v ++ " = " ++ printExp e1 ++ " in " ++ printExp e2

printExp (Lambda v body) = "\\" ++ v ++ " -> " ++ printExp body

printExp (Apply e1 e2) = "((" ++ printExp e1 ++ ") (" ++ printExp e2 ++ "))"

printExp (TryCatch e1 e2) = "try " ++ printExp e1 ++  " catch " ++ printExp e2
