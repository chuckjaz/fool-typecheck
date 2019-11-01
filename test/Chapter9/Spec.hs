module Chapter9.Spec where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck

import Chapter9.Parse
import Chapter9.Syntax
import Chapter9.KindCheck (checkKind)
import Chapter9.SubType
import Chapter9.TypeCheck (checkType)

t = typeExpr
e = expr

int = TType "int" KStar
bool = TType "bool" KStar
string = TType "string" KStar
none = TNone

spec :: IO ()
spec = hspec $ do
    describe "Chapter 9" $ do
        describe "Parse" $ do
            describe "types" types
            describe "expressions" expressions
        describe "Kind Checks" kindChecks
        describe "Subtyping" subtyping
        describe "Type Checks" typeChecks

types = do
    it "int" $ do
        t "int" `shouldBe` int
    it "bool" $ do
        t "bool" `shouldBe` bool
    it "string" $ do
        t "string" `shouldBe` string
    it "variable" $ do
        t "x" `shouldBe` x
    it "void" $ do
        t "void" `shouldBe` TVoid
    it "a -> b" $ do
        t "a -> b" `shouldBe` TFunction a b
    it "command" $ do
        t "command" `shouldBe` TCommand
    it "ref int" $ do
        t "ref int" `shouldBe` TRef int
    it "tuple" $ do
        t "int*bool*string" `shouldBe` TTuple [int, bool, string]
    it "sum" $ do
        t "int+bool+string" `shouldBe` TSum [int, bool, string]
    it "record" $ do
        t "{x: int, y: int}" `shouldBe` TRecord [("x", int), ("y", int)]
    it "lambda (no kind)" $ do
        t "λx.y" `shouldBe` TLambda "x" KStar y
    it "lambda (kinds)" $ do
        t "λx:*=>*.y" `shouldBe` TLambda "x" hk y
    it "apply" $ do
        t "x int" `shouldBe` TApply x int
    it "all (unconstrained)" $ do
        t "∀x.y" `shouldBe` TAll "x" KStar TNone y
    it "all (constrained)" $ do
        t "∀(x <: y).a" `shouldBe` TAll "x" KStar y a
    it "exists (unconstrained)" $ do
        t "∃x.y" `shouldBe` TExists "x" KStar TNone y
    it "exists (constrains)" $ do
        t "∃(x <: y).a" `shouldBe` TExists "x" KStar y a
    it "fix" $ do
        t "fix(x)" `shouldBe` TFix x
    where
        a = TVariable "a" KStar
        b = TVariable "b" KStar
        x = TVariable "x" KStar
        y = TVariable "y" KStar
        hk = KFunction KStar KStar
    
expressions = do
    it "variable" $ do
        e "x" `shouldBe` x
    it "1" $ do
        e "1" `shouldBe` one
    it "true" $ do
        e "true" `shouldBe` true
    it "false" $ do
        e "false" `shouldBe` false
    it "void" $ do
        e "<>" `shouldBe` EVoid
    it "lambda" $ do
        e "λ(x: int).x" `shouldBe` ELambda "x" int x
    it "apply" $ do
        e "x y" `shouldBe` EApply x y
    it "tuple" $ do
        e "<x, y>" `shouldBe` ETuple [x,y]
    it "projection" $ do
        e "proj[1](x)" `shouldBe` EProjection 1 x
    it "case" $ do 
        e "case x of a: int then y" `shouldBe` ECase x [("a", int, y)]
    it "in" $ do
        e "in <int+bool>(x)" `shouldBe` EIn (TSum [int,bool]) x
    it "record" $ do
        e "{x: int := 1, y: int := 2}" `shouldBe` ERecord 
            [   ("x", int, one)
            ,   ("y", int, two)
            ]
    it "select" $ do
        e "x.l" `shouldBe` ESelect x "l"
    it "ref" $ do
        e "ref 1" `shouldBe` ERef one
    it "null" $ do
        e "null" `shouldBe` ENull
    it "val" $ do
        e "val x" `shouldBe` EVal x
    it "nop" $ do
        e "nop" `shouldBe` ENop
    it "assign" $ do
        e  "x := y" `shouldBe` EAssign x y
    it "if" $ do
        e "if true then {1} else {2}" `shouldBe` EIf true one two
    it "sequence" $ do
        e "x; y" `shouldBe` ESequence x y
    it "polymorphic lambda (simple)" $ do
        e "Λx.y" `shouldBe` EPolyLambda "x" KStar TNone y
    it "polymorphic lambda (explicit)" $ do
        e "Λ(x:* => * <: int).y" `shouldBe` EPolyLambda "x" (KFunction KStar KStar) int y
    it "polymorphic apply" $ do
        e "x[int]" `shouldBe` EPolyApply x int
    where
        one = EConstant $ LNumber 1
        two = EConstant $ LNumber 2
        true = EConstant $ LBoolean True
        false = EConstant $ LBoolean False
        x = EVariable "x"
        y = EVariable "y"

kindChecks = do
    it "pre-defined" $ do
        ck "int" `shouldBe` KStar
    it "void" $ do
        ck "void" `shouldBe` KStar
    it "command" $ do
        ck "command" `shouldBe` KStar
    it "function" $ do
        ck "int -> int" `shouldBe` KStar
    it "tuple" $ do
        ck "int*int*bool" `shouldBe` KStar
    it "sum" $ do
        ck "int+bool+string" `shouldBe` KStar
    it "record" $ do
        ck "{x: int, y: int}" `shouldBe` KStar
    it "ref" $ do
        ck "ref int" `shouldBe` KStar
    it "lambda" $ do
        ck "λk.k" `shouldBe` twoStar
    it "apply + variable" $ do
        ck "(λk:*=>*.k:*=>*) (λk.k)" `shouldBe` twoStar
    it "all (simple)" $ do
        ck "∀v.v" `shouldBe` KStar
    it "all (explicit)" $ do
        ck "∀(x:* <: int).x" `shouldBe` KStar
    it "exists (simple)" $ do
        ck "∃x.x" `shouldBe` KStar
    it "exists (explicit)" $ do
        ck "∃(x: * <: int).x" `shouldBe` KStar
    it "fix" $ do
        ck "fix(λk.k)" `shouldBe` KStar
    where
        ck s = checkKind $ t s
        twoStar = KFunction KStar KStar

subtyping = do
    it "reflexive" $ do
        st TVoid TVoid `shouldBe` True
        st TVoid TCommand `shouldBe` False
        st TCommand TCommand `shouldBe` True
        st x x `shouldBe` True
        st y y `shouldBe` True
        st z z `shouldBe` True
    it "defined" $ do
        st x y `shouldBe` True
        st y z `shouldBe` True
    describe "function" $ do
        it "simple" $ do
            st (TFunction y y) (TFunction y y) `shouldBe` True
        it "specialized result" $ do
            st (TFunction y x) (TFunction y y) `shouldBe` True
        it "widen parameter" $ do
            st (TFunction z y) (TFunction y y) `shouldBe` True
        it "both" $ do
            st (TFunction z x) (TFunction y y) `shouldBe` True
        describe "reject" $ do
            it "co/contra" $ do    
                st (TFunction x x) (TFunction y y) `shouldBe` False
    describe "product" $ do
        it "simple" $ do
            st (TTuple [x, y]) (TTuple [x, y]) `shouldBe` True
        it "longer" $ do
            st (TTuple [x, y, z]) (TTuple [x, y]) `shouldBe` True
        it "one subtype" $ do
            st (TTuple [x, x]) (TTuple [x, y]) `shouldBe` True
    describe "sum" $ do
        it "simple" $ do
            st (TSum [x, y]) (TSum [x, y]) `shouldBe` True
        it "shorter" $ do
            st (TSum [x, y]) (TSum [x, y, z]) `shouldBe` True
        it "one subtype" $ do
            st (TSum [x, x]) (TSum [x, y]) `shouldBe` True
    describe "record" $ do
        it "simple" $ do
            sts "{x: int, y: int}" "{x: int, y: int}" `shouldBe` True
        it "longer" $ do
            sts "{x: int, y: int, z: int}" "{x: int, y: int}" `shouldBe` True
        it "subtype" $ do
            sts "{a: x}" "{a: z}" `shouldBe` True
    describe "poly" $ do
        it "simple" $ do
            sts "∀a.x" "∀b.x" `shouldBe` True
        it "alpha" $ do
            sts "∀a.x" "∀b.x" `shouldBe` True
        it "substitute (simple)" $ do
            sts "∀(a <: x).a" "∀(b <: x).b"
    describe "exists" $ do
        it "simple" $ do
            sts "∃a.x" "∃a.x" `shouldBe` True
        it "alpha" $ do
            sts "∃a.x" "∃b.x" `shouldBe` True
        it "substitute (simple)" $ do
            sts "∃(a <: x).a" "∃(b <: x).b"
    describe "transitive" $ do
        it "direct" $ do
            st x z `shouldBe` True
    where
        x = TVariable "x" KStar
        y = TVariable "y" KStar
        z = TVariable "z" KStar
        c = buildConstraints [(x,y), (y,z)] Map.empty
        st a b = subTypeOf a b c
        sts a b = st (t a) (t b)

typeChecks = do
    describe "constants" $ do
        it "bool" $ do
            tc "true" `shouldBe` bool
        it "int" $ do
            tc "1" `shouldBe` int
    it "void" $ do
        tc "<>" `shouldBe` TVoid
    it "function" $ do
        tc "λ(x:int).x" `shouldBe` TFunction int int
    it "apply" $ do
        tc "(λ(x:int).x) 1" `shouldBe` int
    it "product" $ do
        tc "<1,true>" `shouldBe` TTuple [int, bool]
    it "proj" $ do
        tc "proj[1](<1,true,1>)" `shouldBe` bool
    it "sum" $ do
        tc "in<int+bool>(0)" `shouldBe` TSum [int, bool]
    it "case" $ do
        tc "case in<int+bool>(0) of x:int then true x:bool then x" `shouldBe` bool
    it "record" $ do
        tc "{x: int := 1, y: int := 2}" `shouldBe` TRecord [("x", int), ("y", int)]
    it "selection" $ do
        tc "({a: int := 1, b: bool := true}).b" `shouldBe` bool
    it "reference" $ do
        tc "ref 0" `shouldBe` TRef int
    it "value" $ do
        tc "val (ref true)" `shouldBe` bool
    it "null" $ do
        tc "null" `shouldBe` TRef TNone
    it "nop" $ do
        tc "nop" `shouldBe` TCommand
    it "assignment" $ do
        tc "(ref 0) := 1" `shouldBe` TCommand
    it "conditional" $ do
        tc "if true then { 0 } else { 1 }" `shouldBe` int
    it "sequence" $ do
        tc "1;true" `shouldBe` bool
    it "poly-lambda" $ do
        tc "Λt.λ(x:t).λ(y:t).<x,y>" `shouldBe` 
            TAll "t" KStar TNone (TFunction t (TFunction t (TTuple [t, t])))
    it "poly-apply" $ do
        tc "(Λt.λ(x:t).λ(y:t).<x,y>)[int] 1 2" `shouldBe` TTuple [int, int]
    where
        tc s = checkType (expr s) Map.empty
        t = TVariable "t" KStar
