module APL.Tests where

import APL.AST (Exp (..), VName)
import Test.QuickCheck (Arbitrary(..), Gen, elements, oneof, listOf, sample, sized, arbitrary, quickCheck, shrink)
import APL.Eval ( eval, runEval)

genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Int -> Gen Exp

genExp size =
  if size <= 1
  then Var <$> genVar
  else
    let half = (size - 1) `div` 2
    in oneof
      [ Var <$> genVar
      , Lambda <$> genVar <*> genExp (size - 1)
      , Apply <$> genExp half <*> genExp half
      , CstInt <$> arbitrary
      , CstBool <$> arbitrary
      , Add <$> genExp (size - 1) <*> genExp (size - 1)
      , Sub <$> genExp (size - 1) <*> genExp (size - 1)
      , Mul <$> genExp (size - 1) <*> genExp (size - 1)
      , Div <$> genExp (size - 1) <*> genExp (size - 1)
      , Pow <$> genExp (size - 1) <*> genExp (size - 1)
      , Eql <$> genExp (size - 1) <*> genExp (size - 1)
      , If <$> genExp (size - 1) <*> genExp (size - 1) <*> genExp (size - 1)
      , Let <$> genVar <*> genExp (size - 1) <*> genExp (size - 1)
      , TryCatch <$> genExp (size - 1) <*> genExp (size - 1)
      ]

instance Arbitrary Exp where
    arbitrary = sized genExp
    shrink (Add e1 e2) = e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
    shrink (If cond e1 e2) = e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
    shrink (Var x) = [Var x' | x' <- shrink x, not (null x')]
    shrink (Let x e1 e2) = e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
    shrink (Lambda x e) = e : [Lambda x e' | e' <- shrink e]

prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval ( Add e1 (Add e2 e3)))