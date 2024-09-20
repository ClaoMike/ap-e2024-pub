module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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

evalConstantTest :: TestTree
evalConstantTest = testCase "CstInt to ValInt" $ runEval (eval (CstInt 4)) @?= Right(ValInt 4)

evalAddTest :: TestTree
evalAddTest = testCase "Add 2 constants" $ runEval (eval (Add (CstInt 2) (CstInt 3))) @?= Right(ValInt 5)

evalSubTest :: TestTree
evalSubTest = testCase "Sub 2 constants" $ runEval (eval (Sub (CstInt 2) (CstInt 3))) @?= Right(ValInt (-1))

evalMulTest :: TestTree
evalMulTest = testCase "Mul 2 constants" $ runEval (eval (Mul (CstInt 3) (CstInt 4))) @?= Right(ValInt 12)

evalDivTest :: TestTree
evalDivTest = testCase "Div 2 constants" $ runEval (eval (Div (CstInt 12) (CstInt 3))) @?= Right(ValInt 4)

evalPowTest :: TestTree
evalPowTest = testCase "Pow 2 constants" $ runEval (eval (Pow (CstInt 2) (CstInt 3))) @?= Right(ValInt 8)

evalExp1Test :: TestTree
evalExp1Test = testCase "Pow 1 constant and an expression" $ runEval (eval (Pow (CstInt 2) (Add (CstInt 3) (CstInt 4)))) @?= Right(ValInt 128)

evalExp2Test :: TestTree
evalExp2Test = testCase "Add 1 constant and an expression" $ runEval (eval (Add (CstInt 2) (Mul (CstInt 3) (CstInt 4)))) @?= Right(ValInt 14)

evalExp3Test :: TestTree
evalExp3Test = testCase "Mul 1 expression and 1 constant" $ runEval (eval (Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4))) @?= Right(ValInt 20)

evalSafeDivTest :: TestTree
evalSafeDivTest = testCase "Div 2 constants" $ runEval (eval (Div (CstInt 12) (CstInt 0))) @?= Left "Division by 0"

evalSafePowTest :: TestTree
evalSafePowTest = testCase "Pow 2 constants" $ runEval (eval (Pow (CstInt 2) (CstInt (-1)))) @?= Left "Negative power"

evalEqlTest :: TestTree
evalEqlTest = testCase "Eql int and bool" $ runEval (eval (Eql (CstBool True) (CstInt 10))) @?= Left "Eql failed"

evalIfTest :: TestTree
evalIfTest = testCase "If int bool bool" $ runEval (eval (If (CstInt 10) (CstBool True) (CstBool False))) @?= Left "Non-boolean conditional."

