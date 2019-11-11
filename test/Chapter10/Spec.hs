module Chapter10.Spec where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck

import Chapter10.Parse
import Chapter10.Syntax

spec :: IO ()
spec = hspec $ do
    describe "Chapter 10" $ do
        describe "Parse" $ do
            describe "types" types

int = TVariable "Integer"
bool = TVariable "Boolean"
string = TVariable "String"
void = TVariable "Void"

t = typeExpr 

types = do
    it "Integer" $ do
        t "Integer" `shouldBe` int
    it "Boolean" $ do
        t "Boolean" `shouldBe` bool
    it "String" $ do
        t "String" `shouldBe` string
    it "Void" $ do
        t "Void" `shouldBe` void
    it "variable" $ do
        t "x" `shouldBe` x
    it "ref" $ do
        t "ref x" `shouldBe` TRef x
    it "function" $ do
        t "x*x->x" `shouldBe` TFunction [x,x] x
    it "ObjectType" $ do
        t "ObjectType {| x: Integer; y: Integer |}" `shouldBe`
            TObject (TRecord [("x", int), ("y", int)])
    it "VisObjectType" $ do
        t "VisObjectType({| x: ref Integer |}, {| getX: Void -> Integer |})" `shouldBe`
            TVisObject (TRecord [("x", TRef int)]) (TRecord [("getX", TFunction [void] int)])
    it "ClassType" $ do
        t "ClassType({| x: ref Integer |}, {| getX: Void -> Integer |})" `shouldBe`
            TClass (TRecord [("x", TRef int)]) (TRecord [("getX", TFunction [void] int)])
    where
        x = TVariable "x"
