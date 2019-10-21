import Test.Hspec
import Test.QuickCheck

import Chapter8.TypeCheck
import Chapter8.Syntax

tc = typeCheckExpression
int = Type "Integer"
bool = Type "Boolean"

uk s = TypeError ("ERROR: Undefined variable '" ++ s ++ "'")
fne = TypeError "ERROR: Expected a function type"
pe a b = TypeError ("ERROR: Expected parameter of type " ++ 
    show a ++ ", received type " ++ show b)

main :: IO ()
main = hspec $ do
    describe "Chapter8" $ do
        describe "type check" $ do
            describe "positive" $ do
                it "1 should be integer" $ do
                    tc "1" `shouldBe` int
                it "True should be boolean" $ do
                    tc "True" `shouldBe` bool
                it "False should be boolean" $ do
                    tc "False" `shouldBe` bool
                it "int identity should be function" $ do
                    tc "\\(x:Integer).x" `shouldBe` Function int int
                it "int identity apply should be int" $ do
                    tc "(\\(x:Integer).x) 1" `shouldBe` int
            describe "negative" $ do
                it "untyped var" $ do
                    tc "x" `shouldBe` uk "x"
                it "apply to a non-function" $ do
                    tc "1 1" `shouldBe` fne
                it "apply of wrong type" $ do
                    tc "(\\(x:Integer).x) True" `shouldBe` pe int bool

