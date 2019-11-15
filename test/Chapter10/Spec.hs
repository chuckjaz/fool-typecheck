module Chapter10.Spec where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck

import Common.Parser
import Chapter10.Parse
import Chapter10.Syntax

spec :: IO ()
spec = hspec $ do
    describe "Chapter 10" $ do
        describe "Parse" $ do
            describe "types" types
            describe "expressions" expressions
            describe "statements" statements
            describe "blocks" blocks
            describe "programs" programs

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

expressions = do
    it "variable" $ do
        e "x" `shouldBe` x
    it "numbers" $ do
        e "1" `shouldBe` one
        e "0" `shouldBe` zero
    it "boolean" $ do
        e "true" `shouldBe` true
        e "false" `shouldBe` false
    it "nil" $ do
        e "nil" `shouldBe` ENil
    it "void" $ do
        e "()" `shouldBe` EVoid
    it "ref" $ do
        e "ref 1" `shouldBe` ERef one
    it "val" $ do
        e "val (ref 0)" `shouldBe` EVal (ERef zero)
    where
        e s = runParser expression $ lexer s
        x = EVariable "x"
        zero = EConstant $ LInteger 0
        one = EConstant $ LInteger 1
        true = EConstant $ LBoolean True
        false = EConstant $ LBoolean False

statements = do
    it "nop" $ do
        s "nop" `shouldBe` SNop
    it "assign" $ do
        s "a := x" `shouldBe` SAssign "a" x
    it "if" $ do
        s "if true then { nop } else { nop }" `shouldBe` SIf true SNop SNop
    it "while" $ do
        s "while (x) do { nop }" `shouldBe` SWhile x SNop
    it "sequence" $ do
        s "nop; nop" `shouldBe` SSequence SNop SNop
    where
        s s = runParser statement $ lexer s
        x = EVariable "x"
        zero = EConstant $ LInteger 0
        true = EConstant $ LBoolean True

blocks = do
    it "simple block" $ do
        b "{ nop return 1 }" `shouldBe` Block empty empty SNop one
    it "const then block" $ do
        b "const x: Integer = 1 { nop return x }" `shouldBe`
            Block empty (Map.fromList [("x", (int, one))]) SNop x
    it "multiple const block" $ do
        b "const x: Integer = 1; y: Integer = 1 { nop return x }" `shouldBe`
            Block empty (Map.fromList [("x", (int, one)), ("y", (int, one))]) SNop x
    it "type then block" $ do
        b "type tx = Integer { nop return 1 }" `shouldBe`
            Block (Map.fromList [("tx", int)]) empty SNop one
    it "two types then block" $ do
        b "type tx = Integer; ty = Integer { nop return 1 }" `shouldBe`
            Block (Map.fromList [("tx", int), ("ty", int)]) empty SNop one
    it "both const and types" $ do
        b "type tx = Integer const x: tx = 1 { nop return 1 }" `shouldBe`
            Block (Map.fromList [("tx", int)]) (Map.fromList [("x", (tx, one))]) SNop one
    where
        b s = runParser block $ lexer s
        empty = Map.empty
        one = EConstant $ LInteger 1
        x = EVariable "x"
        tx = TVariable "tx"
        ty = TVariable "ty"

programs = do
    it "simple program" $ do
        p "Program main; { nop return 1 }" `shouldBe` 
            Program "main" (Block empty empty SNop one)
    where
        p s = runParser program $ lexer s
        empty = Map.empty
        one = EConstant $ LInteger 1
