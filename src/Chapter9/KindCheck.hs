{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chapter9.KindCheck where

import Chapter9.Syntax

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map as Map

import Debug.Trace

type Env = Map.Map String Kind

newtype Check a = Check (StateT Env (Except String) a)
  deriving 
    (   Functor
    ,   Applicative
    ,   Monad
    ,   MonadState Env
    ,   MonadError String
    )

getState (Check p) = p

runCheck p e c = case (runExcept $ evalStateT (getState (p c)) e) of
    Right k -> k
    Left s -> KError s

checkKind c = runCheck check Map.empty c

err :: String -> Check a
err = throwError

find :: String -> Check (Maybe Kind)
find name = do
    env <- get
    return $ Map.lookup name env

requireStar :: Type -> Check Kind
requireStar c = do
    k <- check c
    case k of
        KStar -> return KStar
        _ -> err "Expected concrete type"

requireFunction :: Type -> Kind-> Check Kind
requireFunction c pk = do
    k <- check c
    case k of
        KFunction  pk' r -> 
            if pk == pk' then return r
            else err ("Expected parameter of kind " ++ show pk' ++ ", received " ++ show pk)
        _ -> err "Expected a function kind"

assert :: String -> Kind -> Check ()
assert name typ = do
    env <- get
    put $ Map.insert name typ env

scope :: Check Kind -> Check Kind
scope p = do
    old <- get
    s <- p
    put old
    return s

check :: Type -> Check Kind

check (TVariable name kind) = do
    mk <- find name
    case mk of
        Just k -> 
            if k == kind
                then return kind
                else err ("Expected kind of " ++ show k ++ ", found " ++ show kind)
        Nothing -> return kind
check (TType _ kind) = return kind
check TVoid = return KStar
check TCommand = return KStar

check (TFunction m v) = do
    requireStar m
    requireStar v
    return KStar

check (TTuple cs) = do
    mapM requireStar cs
    return KStar

check (TSum cs) = do
    mapM requireStar cs
    return KStar

check (TRecord fs) = do
    mapM (\(s, c) -> requireStar c) fs
    return KStar

check (TRef c) = do
    requireStar c
    return KStar

check (TLambda name k body) = do
    bodyKind <- scope $ do
        assert name k
        bodyKind <- check body
        return bodyKind
    return $ KFunction k bodyKind

check (TApply m n) = do
    ak <- check n
    r <- requireFunction m ak
    return r

check (TAll name kind _ body) = do
    scope $ do
        assert name kind
        requireStar body
    return KStar

check (TExists name kind _ body) = do
    scope $ do
        assert name kind
        requireStar body
    return KStar

check (TFix m) = do
    mk <- check m
    case mk of
        (KFunction pk rk) ->  if pk == rk
            then return rk
            else err ("Expected type of kind k => k, found " ++ show mk)
        _ -> err ("Expected function type, found " ++ show m)