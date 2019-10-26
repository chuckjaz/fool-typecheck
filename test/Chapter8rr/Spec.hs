module Chapter8rr.Spec where

import Test.Hspec
import Test.QuickCheck

import Chapter8rr.TypeCheck
import Chapter8rr.Syntax

tc = typeCheck
int = TType "Integer"
bool = TType "Boolean"
field s t = (s, t)

uk s = TError ("Undefined variable '" ++ s ++ "'")
fne = TError "Expected a function type"
pe a b = TError ("Expected parameter of type " ++ 
    show a ++ ", received type " ++ show b)

spec :: IO ()
spec = hspec $ do
    describe "Chapter8rr" $ do
        describe "type check" $ do
            describe "positive" $ do
                it "1 should be integer" $ do
                    tc "1" `shouldBe` int
                it "true should be boolean" $ do
                    tc "true" `shouldBe` bool
                it "false should be boolean" $ do
                    tc "false" `shouldBe` bool
                it "int identity should be function" $ do
                    tc "λ(x:Integer).x" `shouldBe` TFunction int int
                it "int identity apply should be int" $ do
                    tc "(λ(x:Integer).x) 1" `shouldBe` int
                it "record" $ do
                    tc "{x: Integer := 1, y: Boolean := true}" `shouldBe`
                        TRecord [field "x" int, field "y" bool]
                it "void" $ do
                    tc "<>" `shouldBe` TVoid
                it "tuple" $ do
                    tc "<1, true>" `shouldBe` TTuple [int, bool]
                it "projection" $ do
                    tc "proj[0](<1, true>)" `shouldBe` int
                it "sum type" $ do
                    tc "in Integer+Boolean(1)" `shouldBe` TSum [int, bool]
                it "case" $ do
                    tc "case in Integer+Boolean(1) of x: Integer then x"
                        `shouldBe` int
                it "selection" $ do
                    tc "{x: Integer := 1, y: Boolean := true}.x" `shouldBe` int
                it "reference" $ do
                    tc "ref 1" `shouldBe` TRef int
                it "simple null" $ do
                    tc "null" `shouldBe` TRef TOpen
                it "unify null" $ do
                    tc "(λ(x: ref Integer).x) null" `shouldBe` TRef int
                it "val" $ do
                    tc "val (ref 1)" `shouldBe` int
                it "nop" $ do
                    tc "nop" `shouldBe` TCommand
                it "conditional" $ do
                    tc "if true then 0 else 0" `shouldBe` int
                it "sequence" $ do
                    tc "1;2" `shouldBe` int
