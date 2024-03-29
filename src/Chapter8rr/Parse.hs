module Chapter8rr.Parse where

import Control.Applicative
import Control.Monad

import Common.Parser
import Common.Lexer

import Chapter8rr.Syntax

import Debug.Trace

-- Types

namedType :: Parser Type
namedType = do
    name <- identifier
    return $ TType name

voidType :: Parser Type
voidType = do
    expect "void"
    return $ TVoid

commandType :: Parser Type
commandType = do
    expect "command"
    return $ TCommand

refType :: Parser Type
refType = do
    expect "ref"
    typ <- typeExpression
    return $ TRef typ

recordType :: Parser Type
recordType = do
    expect "{"
    fs <- sepBy field ","
    expect "}"
    return $ TRecord fs
    where
        field = do
            name <- identifier
            expect ":"
            typ <- typeExpression
            return $ (name, typ)

typePrimitive :: Parser Type
typePrimitive = do
        voidType
    <|> commandType
    <|> namedType
    <|> refType
    <|> recordType
    <|> parens typeExpression

typeFunction :: Parser Type
typeFunction = typePrimitive `chainl1` (infixOp "->" TFunction)

oneOf ts f = do
    case ts of
        [] -> return TVoid
        [t] -> return t
        _ -> return $ f ts

typeTuple :: Parser Type
typeTuple = do
    types <- (one typeFunction) `chainl1` (infixOp "*" (++))
    t <- oneOf types TTuple
    return t

typeSum :: Parser Type
typeSum = do
    types <- (one typeTuple) `chainl1` (infixOp "+" (++))
    t <- oneOf types TSum
    return t

typeExpression :: Parser Type
typeExpression = typeSum

-- Expressions

variable :: Parser Expression
variable = do
    name <- identifier
    return $ EVariable name

numberLiteral :: Parser Expression
numberLiteral = do
    value <- number
    return $ EConstant $ LNumber value

booleanLiteral :: Parser Expression
booleanLiteral = trueLiteral <|> falseLiteral
    where
        trueLiteral = do
            expect "true"
            return $ EConstant $ LBoolean True
        falseLiteral = do
            expect "false"
            return $ EConstant $ LBoolean False

literal :: Parser Expression
literal = numberLiteral <|> booleanLiteral

voidExpr :: Parser Expression
voidExpr = do
    expect "<>"
    return EVoid

lambda :: Parser Expression
lambda = do
    expect "λ"
    expect "("
    parameterName <- identifier
    expect ":"
    parameterType <- typeExpression
    expect ")"
    expect "."
    body <- expression
    return $ ELambda parameterName parameterType body

tuple :: Parser Expression
tuple = do
    expect "<"
    expressions <- sepBy expression ","
    expect ">"
    return $ ETuple expressions

projection :: Parser Expression
projection = do
    expect "proj"
    expect "["
    index <- number
    expect "]"
    expect "("
    value <- expression
    expect ")"
    return $ EProjection index value

caseExpr :: Parser Expression
caseExpr = do
    expect "case"
    value <- expression
    expect "of"
    clauses <- many caseClause
    return $ ECase value clauses

caseClause :: Parser (String, Type, Expression)
caseClause = do
    name <- identifier
    expect ":"
    typ <- typeExpression
    expect "then"
    body <- primitive
    return $ (name, typ, body)

inExpr :: Parser Expression
inExpr = do
    expect "in"
    typ <- typeExpression
    expect "("
    value <- expression
    expect ")"
    return $ EIn typ value

record :: Parser Expression
record = do
    expect "{"
    fields <- sepBy field ","
    expect "}"
    return $ ERecord fields
    where
        field = do
            name <- identifier
            expect ":"
            typ <- typeExpression
            expect ":="
            value <- expression
            return $ (name, typ, value)

ref :: Parser Expression
ref = do
    expect "ref"
    value <- expression
    return $ ERef value

nullExpr :: Parser Expression
nullExpr = do
    expect "null"
    return ENull

val :: Parser Expression
val = do
    expect "val"
    value <- expression
    return $ EVal value

ifExpr :: Parser Expression
ifExpr = do
    expect "if"
    value <- expression
    expect "then"
    thenVal <- expression
    expect "else"
    elseVal <- expression
    return $ EIf value thenVal elseVal

nop :: Parser Expression
nop = do
    expect "nop"
    return ENop

primitive :: Parser Expression
primitive =
        literal
    <|> voidExpr
    <|> tuple
    <|> lambda
    <|> tuple
    <|> projection
    <|> caseExpr
    <|> inExpr
    <|> record
    <|> ref
    <|> nullExpr
    <|> val
    <|> ifExpr
    <|> nop
    <|> variable
    <|> parens expression

select :: Parser Expression
select = do
    value <- primitive
    selector value <|> return value
    where
        selector :: Expression -> Parser Expression
        selector value = do
            expect "."
            name <- identifier
            return $ ESelect value name

apply :: Parser Expression
apply = do
    a <- select
    rest a
        where
            one a = do
                b <- select
                rest $ EApply a b
            rest a = one a <|> return a

assign :: Parser Expression
assign = do
    target <- apply
    assign' target <|> return target
    where
        assign' target = do
            expect ":"
            expect "="
            value <- apply
            return $ EAssign target value

sequenceExpr :: Parser Expression
sequenceExpr = do
    first <- assign
    next first <|> return first
    where
        next first = do
            expect ";"
            next <- expression
            return $ ESequence first next

expression :: Parser Expression
expression = sequenceExpr

lexer :: String -> [Token]
lexer s = reserve ["true", "false", "in", "λ", "null", "ref", "if",
    "then", "else", "val", "nop", "case", "of", "void", "command",
    "proj"] $ tokens s

expr :: String -> Expression
expr s = runParser expression $ lexer s