{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter9.TypeCheck where

import Chapter9.Alpha
import Chapter9.KindCheck (checkKind)
import Chapter9.Syntax
import Chapter9.SubType (Constraints, subTypeOf)

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map

import Debug.Trace

type Env = Map.Map String Type

type CheckState = (Env, Constraints, Integer)

newtype TypeCheck a = TypeCheck (StateT CheckState (Except String) a)
  deriving 
    (   Functor
    ,   Applicative
    ,   Monad
    ,   MonadState CheckState
    ,   MonadError String
    )

getState (TypeCheck p) = p

runTypeCheck p e c = case (runExcept $ evalStateT (getState (p c)) e) of
    Right k -> k
    Left s -> TError s

checkType e c = runTypeCheck check (Map.empty, c, 0) e

check :: Expression -> TypeCheck Type

-- Build-in types
bool = TType "bool" KStar
int = TType "int" KStar

-- Identifier
check (EVariable name) = find name

-- Constant
check (EConstant (LBoolean _)) = return bool
check (EConstant (LNumber _)) = return int

-- Void
check EVoid = return TVoid

-- Function
check (ELambda name t body) = scope $ do
    assert name t
    r <- check body
    return $ TFunction t r

-- FuncApp
check (EApply t p) = do
    tt <- check t
    (pt', rt) <- requireFunction tt
    checkSubType p pt'
    return rt

-- Product
check (ETuple es) = do
    ts <- mapM check es
    return $ TTuple ts

-- Proj
check (EProjection index body) = do
    bt <- check body
    case bt of
        TTuple ts -> do
            t <- indexOf index ts
            return t
        _ -> err ("Expected '" ++ show bt ++ "' to be a tuple type")
    where
        indexOf 0 (t:ts) = return t
        indexOf n (t:ts) = indexOf (n-1) ts
        indexOf _ [] = err "Index out bound"

-- Sum
check (EIn t e) = do
    et <- check e
    oneOf et t
    return t

-- Case
check (ECase e cs) = do
    et <- check e
    caseTypes <- mapM (checkCase et) cs
    lowestType caseTypes
    where
        checkCase et (name, t, body) = scope $ do
            oneOf t et
            assert name t
            check body    

-- Record
check (ERecord fs) = do
    fts <- mapM field fs
    return $ TRecord fts
    where
        field (name, ft, fe) = do
            fet <- check fe
            ft' <- requireSubType fet ft
            return (name, ft')

-- Selection
check (ESelect e label) = do
    et <- check e
    case et of
        TRecord fts -> do
            sft <- select label fts
            return sft
        _ -> err ("Expected a record type, received " ++ show et)
    where
        select label ((name, t):_) | name == label = return t
        select label (_:ts) = select label ts
        select label [] = err ("Undefined record '" ++ label ++ "'")

-- Reference
check (ERef e) = do
    et <- check e
    return $ TRef et

-- Null
check ENull = return $ TRef TNone

-- Value
check (EVal e) = do
    et <- check e
    case et of
        TRef t -> return t
        _ -> err ("Expected a reference type, received " ++ show et)

-- Nop (missing)
check ENop = return TCommand

-- Assignment
check (EAssign t e) = do
    et <- check e
    tt <- check t
    case tt of
        TRef rt -> do
            et' <- requireSubType et rt
            return TCommand
        _ -> err ("Expected reference type, received '" ++ show tt ++ "'")

-- Conditional
check (EIf c t e) = do
    ct <- check c
    requireSubType ct bool
    tt <- check t
    et <- check e
    lowestType [tt, et]

-- Sequence
check (ESequence a b) = do
    check a
    r <- check b
    return r

-- PolyFunc
check (EPolyLambda name kind t body) = do
    bt <- check body
    return $ TAll name kind t bt

-- PolyApp
check (EPolyApply t p) = do
    tt <- check t
    r <- alphaSub tt p
    return r

-- pack

alphaSub :: Type -> Type -> TypeCheck Type
alphaSub (TAll name k _ lt) p = do
    requireKindMatch k (checkKind p)
    return $ alpha name p lt
alphaSub t p = err ("Expected '" ++ show t ++ "' to be an all type")

checkSubType :: Expression -> Type -> TypeCheck Type
checkSubType e t = do
    et <- check e
    c <- constraints
    if subTypeOf et t c
        then return t
        else err ("Expected " ++ show et ++ " to be a subtype of " ++ show t)
    
requireFunction :: Type -> TypeCheck (Type, Type)
requireFunction (TFunction p b) = return (p, b)
requireFunction t = err ("Expected a function type " ++ show t)

requireSubType :: Type -> Type -> TypeCheck Type
requireSubType (TRef TNone) (TRef b) = return $ TRef b
requireSubType (TRef a) (TRef TNone) = return $ TRef a
requireSubType a b = do
    c <- constraints
    if subTypeOf a b c
        then return b
        else err ("Expected '" ++ show a ++ "' to be a subtype of '" ++ show b ++"'")

requireKindMatch a b | a == b = return ()
requireKindMatch a b = err ("Kind mismatch, '" ++ show a ++ "' and '" ++ show b ++ "'")

lowestType :: [Type] -> TypeCheck Type
lowestType (t:ts) = lowestType' t ts
        where
            lowestType' c (t:ts) = do 
                l <- lowestOf c t
                r <- lowestType' l ts
                return r
            lowestType' c [] = return c
            lowestOf a b = do
                c <- constraints
                let  aIsSubtypeOfB = subTypeOf a b c
                     bIsSubtypeOfA = subTypeOf b a c in
                    if aIsSubtypeOfB 
                        then return b
                        else if bIsSubtypeOfA
                            then return a
                            else err ("Could not unify '" ++ show a ++ "' with '" ++ show b ++ "'")

oneOf t (TSum ts) = oneOf' t ts
    where
        oneOf' a (b:ts) = do
            c <- constraints
            if subTypeOf a b c
                then return ()
                else oneOf' a ts
        oneOf' a [] = err ("Expected expression of type '" ++ show a ++ "' to be in '" ++ show t)
        
oneOf _ t = err ("Expected '" ++ show t ++ "' to be a sum type") 

find :: String -> TypeCheck Type
find name = do
    e <- env
    case Map.lookup name e of
        Just t -> return t
        Nothing -> err ("Undefined variable '" ++ name ++ "'")

scope :: (TypeCheck Type) -> TypeCheck Type
scope p = do
    s <- get
    r <- p
    put s
    return r

assert :: String -> Type -> TypeCheck ()
assert name t = do
    e <- env
    putEnv $ Map.insert name t e

env :: TypeCheck Env
env = do
    (env, _, _) <- get
    return env

putEnv :: Env -> TypeCheck ()
putEnv env = do
    (_, c, n) <- get
    put (env, c, n)

constraints :: TypeCheck Constraints
constraints = do
    (_, c, _) <- get
    return c

putConstraints :: Constraints -> TypeCheck ()
putConstraints c = do
    (env, _, n) <- get
    put (env, c, n)

err :: String -> TypeCheck a
err = throwError
