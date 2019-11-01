{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments             #-}

module Chapter9.SubType where

import Chapter9.Alpha
import Chapter9.Syntax
import Chapter9.KindCheck (checkKind)

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

type Constraints = Map.Map Type (Set.Set Type)

type SubTypeState = (Integer, Constraints)

type SubType = State SubTypeState

subTypeOf :: Type -> Type -> Constraints -> Bool
subTypeOf a b c = evalState (subtype a b) (0, c)

-- Figure 9.5 & 9.6
subtype :: Type -> Type -> SubType Bool

-- Reflex <:
subtype a b | a == b = return True

-- Function <:
subtype a@(TFunction s' r') b@(TFunction s r) = do
    sr <- subtype s s'
    rr <- subtype r' r
    result (sr && rr) a b

-- Product <:
subtype a@(TTuple as) b@(TTuple bs) = do
    r <- subTypeLonger as bs
    result r a b

-- Sum <:
subtype a@(TSum as) b@(TSum bs) = do
    r <- subTypeShorter as bs
    result r a b

-- Record <:
subtype a@(TRecord as) b@(TRecord bs) = do
    r <- fields as bs
    result r a b
    where
        fields :: [TFieldClause] -> [TFieldClause] -> SubType Bool
        fields ((an, at):as) ((bn, bt):bs) = do
            r <- subtype at bt
            rs <- fields as bs
            return (r && an == bn)
        fields [] _ = return True

-- Poly <: && BdPoly <:
subtype a@(TAll an ak ac at) b@(TAll bn bk bc bt) | ac == bc && ak == bk = do
    r <- scope $ do
        t <- unique ak
        let at' = alpha an t at
            bt' = alpha bn t bt in do
                assert t ac
                r <- subtype at' bt'
                return r
    result r a b

-- Exists <: && BdExists <:
subtype a@(TExists an ak ac at) b@(TExists bn bk bc bt) | ac == bc && ak == bk = do
    r <- scope $ do
        t <- unique ak
        let at' = alpha an t at
            bt' = alpha bn t bt in do
                assert t ac
                r <- subtype at' bt'
                return r
    result r a b

-- ConstructorFcns <:
subtype a@(TLambda an ak at) b@(TLambda bn bk bt) | ak == bk = do
    r <- scope $ do
        t <- unique ak
        let at' = alpha an t at
            bt' = alpha bn t bt in do
                r <- subtype at' bt'
                return r
    result r a b
        
-- ConstructorApp <:
subtype a@(TApply at ap) b@(TApply bt bp) | ap == bp = do
    r <- subtype at bt
    result r a b

-- Fix <:
subtype a@(TFix am) b@(TFix bm) =
    let ak = checkKind am
        bk = checkKind bm in do
            r <- if ak == bk
                then do
                    av <- unique ak
                    bv <- unique ak
                    r <- scope $ do
                        assert av bv
                        r <- subtype am bm
                        return r
                    return r
                else return False
            result r a b

-- Transitive <:
subtype a b = do
    c <- constraints
    let p = Map.lookup a c in
        case p of
            Just as -> subtype'' as
            Nothing -> return False
    where
        subtype'' :: (Set.Set Type) -> SubType Bool
        subtype'' as = do
            r <- foldM try False as
            return r
        try :: Bool -> Type -> SubType Bool
        try p t = if p then (return p) else (subtype t b)

-- helpers
assert :: Type -> Type -> SubType ()
assert a b = do
    (n, c) <- get
    put (n, Map.insert a (getput a b c) c)
    return ()

buildConstraints :: [(Type, Type)] -> Constraints -> Constraints
buildConstraints ((a, b):ts) c =
    Map.insert a (getput a b c) (buildConstraints ts c)
buildConstraints [] c = c

getput :: Type -> Type -> Constraints -> Set.Set Type
getput a b c = case Map.lookup a c of
    Just s -> Set.insert b s
    Nothing -> Set.insert b Set.empty

result :: Bool -> Type -> Type -> SubType Bool
result True a b = do 
    assert a b
    return True
result False _ _ = return False

subTypeLonger :: [Type] -> [Type] -> SubType Bool
subTypeLonger (a:as) (b:bs) = do
    r <- subtype a b
    rs <- subTypeLonger as bs
    return $ r && rs
subTypeLonger _ [] = return True
subTypeLonger _ _ = return False

subTypeShorter :: [Type] -> [Type] -> SubType Bool
subTypeShorter (a:as) (b:bs) = do
    r <- subtype a b
    rs <- subTypeShorter as bs
    return $ r && rs
subTypeShorter [] _ = return True
subTypeShorter _ _ = return False

scope :: SubType Bool -> SubType Bool
scope p = do
    s <- get
    r <- p
    put s
    return r

fresh :: SubType String
fresh = do
    (n, c) <- get
    let r  = "`" ++ show n in do
        put (n+1, c)
        return r

unique :: Kind -> SubType Type
unique k = do
    f <- fresh
    return $ TVariable f k

constraints :: SubType Constraints
constraints = do
    s <- get
    return $ snd s

traceSM :: String -> SubType ()
traceSM s = do
    st <- get
    trace (s ++ ": " ++ show st) return ()