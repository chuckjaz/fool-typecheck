module Chapter8rr.TypeCheck where

import Chapter8rr.Syntax
import Chapter8rr.Parse (expr)

import Control.Monad.State.Lazy
import qualified Data.Map as Map

typeCheck :: String -> Type
typeCheck text = typeCheckExpr $ expr text

typeCheckExpr :: Expression -> Type
typeCheckExpr e = fst $ runState (check e) Map.empty

type Env = Map.Map String Type

find :: String -> State Env Type
find name = do
    env <- get
    case Map.lookup name env of
        Just t -> return t
        Nothing -> return $ TError $ "Undefined reference '" ++ name ++ "'"

assert :: String -> Type -> State Env ()
assert name typ = do
    env <- get
    put $ Map.insert name typ env

scope :: Env -> State Env Type -> State Env Type
scope env p = do
    return $ fst $ runState p env

expected (TError s) _ _ = TError s
expected _ (TError s) _ = TError s
expected (TRef TOpen) (TRef t) (TRef TOpen) = TRef t
expected (TRef t) (TRef TOpen) (TRef TOpen) = TRef t
expected (TRef TOpen) (TRef _) t = t
expected (TRef _) (TRef TOpen) t = t
expected a b r | a == b = r
expected a b _ = TError ("Expected '" ++ show a ++ "', received '" ++
    show b ++ "'")

expectIn t sumType = expectIn' t sumType sumType
    where
        expectIn' t (TSum ts) r  = expectIn'' t ts r
        expectIn' _ _ _ = TError "Expected a sum type"
        expectIn'' t (t':ts) r | t == t' = r
        expectIn'' t (t':ts) r = expectIn'' t ts r
        expectIn'' t _ r = TError ("Expected '" ++ show t ++
            "' to be in sum type '" ++ show r++ "'")

select n (t:ts) = if n == 0 then t else select (n-1) ts
select _ []     = TError "Projection type index out of bound"

selectMember :: Type -> String -> (String, Type)
selectMember (TRecord fields) name = selectMember' fields name
    where
        selectMember' :: [(String, Type)] -> String -> (String, Type)
        selectMember' ((name', typ):fs) name =
            if name == name' then (name, typ) else selectMember' fs name
        selectMember' [] name = (name, TError ("No member '" ++ name ++ "'"))
selectMember t _ = ("<none>", TError ("Expected a record type, found '" ++ show t ++ "'"))

sameTypes (t:ts) = sameAs t ts
sameTypes _ = TVoid

sameAs t (t':ts) = expected t t' t
sameAs t [] = t

-- Figure 8.2
check :: Expression -> State Env Type

-- Identifier
check (EVariable name) = find name

-- Constant
check (EConstant literal) = case literal of
    LNumber _   -> return $ TType "Integer"
    LBoolean _  -> return $ TType "Boolean"

-- Void
check EVoid = return TVoid

-- Function
check (ELambda arg typ body) = do
    env <- get
    result <- scope env $ do
        assert arg typ
        check body
    return $ TFunction typ result

-- Application
check (EApply target arg) = do
    targetType <- check target
    argType <- check arg
    return $ case targetType of
        (TFunction paramType resultType) ->
            expected paramType argType resultType
        _-> TError "Expected a function type"

-- Tuple
check (ETuple es) = do
    ts <- mapM check es
    return $ TTuple ts

-- Projection
check (EProjection index expr) = do
    typ <- check expr
    return $ case typ of
        TTuple ts -> select index ts
        _ -> TError "Expected a tuple type"

-- Sum
check (EIn typ expr) = do
    exprType <- check expr
    return $ expectIn exprType typ

-- Case
check (ECase expr cs) = do
    exprType <- check expr
    caseTypes <- mapM checkCase cs
    return $ sameTypes caseTypes
    where
        checkCase :: (String, Type, Expression) -> State Env Type
        checkCase (name, typ, expr) = do
            env <- get
            scope env $ do
                assert name typ
                check expr

-- Record
check (ERecord fs) = do
    fields <- mapM checkField fs
    return $ TRecord fields
    where
        checkField :: (String, Type, Expression) -> State Env (String, Type)
        checkField (name, typ, value) = do
            valueType <- check value
            return $ (name, expected typ valueType typ)

-- Selection
check (ESelect target member) = do
    targetType <- check target
    return $ snd $ selectMember targetType member

-- Reference
check (ERef target) = do
    targetType <- check target
    return $ TRef targetType

-- Null
check ENull = return $ TRef TOpen

-- Value
check (EVal value) = do
    valueType <- check value
    return $ case valueType of
        TRef refType -> refType
        _ -> TError "Expected a reference type"

-- No op
check ENop = return TCommand

-- Assignment
check (EAssign target value) = do
    valueType <- check value
    targetType <- check target
    return $ expected targetType (TRef valueType) TCommand

-- Conditional
check (EIf test thenPart elsePart) = do
    testType <- check test
    thenType <- check thenPart
    elseType <- check elsePart
    return $ expected (TType "Boolean") testType $ expected thenType elseType thenType

-- Sequencing
check (ESequence a b) = do
    aType <- check a
    bType <- check b
    return bType
