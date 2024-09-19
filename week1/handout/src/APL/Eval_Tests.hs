module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, Error, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
-- import Distribution.Compat.Prelude (Integer)
import Data.Either (Either)

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      evalConstantTest,
      evalAddTest,
      evalSubTest,
      evalMulTest,
      evalDivTest,
      evalPowTest,
      evalExp1Test,
      evalExp2Test,
      evalExp3Test,
      evalSafeDivTest,
      evalSafePowTest,
      evalEqlTest,
      evalIfTest
    ]

intToRight :: Integer -> Either Error Val
intToRight x = Right $ ValInt x

evalConstantTest :: TestTree
evalConstantTest = testCase "CstInt to ValInt" $ eval envEmpty (CstInt 4) @?= intToRight 4

evalAddTest :: TestTree
evalAddTest = testCase "Add 2 constants" $ eval envEmpty (Add (CstInt 2) (CstInt 3)) @?= intToRight 5

evalSubTest :: TestTree
evalSubTest = testCase "Sub 2 constants" $ eval envEmpty (Sub (CstInt 2) (CstInt 3)) @?= intToRight (-1)

evalMulTest :: TestTree
evalMulTest = testCase "Mul 2 constants" $ eval envEmpty (Mul (CstInt 3) (CstInt 4)) @?= intToRight 12

evalDivTest :: TestTree
evalDivTest = testCase "Div 2 constants" $ eval envEmpty (Div (CstInt 12) (CstInt 3)) @?= intToRight 4

evalPowTest :: TestTree
evalPowTest = testCase "Pow 2 constants" $ eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= intToRight 8

evalExp1Test :: TestTree
evalExp1Test = testCase "Pow 1 constant and an expression" $ eval envEmpty (Pow (CstInt 2) (Add (CstInt 3) (CstInt 4))) @?= intToRight 128

evalExp2Test :: TestTree
evalExp2Test = testCase "Add 1 constant and an expression" $ eval envEmpty (Add (CstInt 2) (Mul (CstInt 3) (CstInt 4))) @?= intToRight 14

evalExp3Test :: TestTree
evalExp3Test = testCase "Mul 1 expression and 1 constant" $ eval envEmpty (Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4)) @?= intToRight 20

evalSafeDivTest :: TestTree
evalSafeDivTest = testCase "Div 2 constants" $ eval envEmpty (Div (CstInt 12) (CstInt 0)) @?= Left "Division by 0"

evalSafePowTest :: TestTree
evalSafePowTest = testCase "Pow 2 constants" $ eval envEmpty (Pow (CstInt 2) (CstInt (-1))) @?= Left "Negative power"

evalEqlTest :: TestTree
evalEqlTest = testCase "Eql int and bool" $ eval envEmpty (Eql (CstBool True) (CstInt 10)) @?= Left "Operation has failed"

evalIfTest :: TestTree
evalIfTest = testCase "If int bool bool" $ eval envEmpty (If (CstInt 10) (CstBool True) (CstBool False)) @?= Left "Operation has failed"
