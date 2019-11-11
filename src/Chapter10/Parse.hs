module Chapter10.Parse where

import Chapter10.Syntax

import Common.Parser
import Common.Lexer

import Control.Applicative
import Control.Monad

import qualified Data.Set as Set

-- Definition 10.3.1
-- t
typeVariable :: Parser Type
typeVariable = do
    n <- identifier
    return $ TVariable n

-- ref
refType :: Parser Type
refType = do
    expect "ref"
    t <- typeExpression
    return $ TRef t

-- ObjectType
objectType :: Parser Type
objectType = do
    expect "ObjectType"
    t <- recordType
    return $ TObject t

-- VisObjectType
visObjectType :: Parser Type
visObjectType = do
    expect "VisObjectType"
    expect "("
    i <- recordType
    expect ","
    m <- recordType
    expect ")"
    return $ TVisObject i m

-- ClassType
classType :: Parser Type
classType = do
    expect "ClassType"
    expect "("
    i <- recordType
    expect ","
    m <- recordType
    expect ")"
    return $ TClass i m

-- Record
recordType :: Parser Type
recordType = do
    expect "{|"
    fields <- sepBy field ";"
    expect "|}"
    return $ TRecord fields
    where
        field = do
            name <- identifier
            expect(":")
            t <- typeExpression
            return (name, t)

typePrimitive :: Parser Type
typePrimitive = do
        typeVariable
    <|> refType
    <|> objectType
    <|> visObjectType
    <|> classType

-- function
functionType :: Parser Type
functionType = do
    params <- sepBy typePrimitive "*"
    tail params
    where
        tail p =
                funTail p
            <|> noTail p
        funTail p = do
            expect "->"
            r <- typePrimitive
            return $ TFunction p r
        noTail [t] = return t
        noTail _ = error "Expected ->"

typeExpression :: Parser Type
typeExpression = functionType

builtInTypes = Set.fromList ["int", "bool", "string", "real"]

lexer :: String -> [Token]
lexer s = reserve ["bool", "int", "string", "real", "ref", "val",
    "ObjectType", "VisObjectType", "ClassType"] $ tokens s

typeExpr :: String -> Type
typeExpr s = runParser typeExpression $ lexer s