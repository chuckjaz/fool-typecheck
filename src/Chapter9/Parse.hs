module Chapter9.Parse where

import Control.Applicative
import Control.Monad

import Common.Parser
import Common.Lexer

import Chapter9.Syntax

-- Types
intType :: Parser Type
intType = do
    expect "int"
    return $ TType"int" KStar

boolType :: Parser Type
boolType = do
    expect "bool"
    return $ TType "bool" KStar

stringType :: Parser Type
stringType = do
    expect "string"
    return $ TType "string" KStar

namedType :: Parser Type
namedType = do
    name <- identifier
    kind <- kindClause
    return $ TVariable name kind

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
            return (name, typ)

kindClause :: Parser Kind
kindClause = kindClause' <|> return KStar
    where
        kindClause' = do
            expect ":"
            kindExpression
        kindExpression = kindPrimitive `chainl1` (infixOp "=>" KFunction)
        kindPrimitive = do
                star
            <|> parens kindExpression
        star = do
            expect "*"
            return KStar

typeLambda :: Parser Type
typeLambda = do
    expect "λ"
    name <- identifier
    kind <- kindClause
    expect "."
    body <- typeExpression
    return $ TLambda name kind body

typeParameter :: Parser (String, Kind, Type)
typeParameter = unconstrained <|> constrained
    where
        unconstrained = do
            name <- identifier
            kind <- kindClause
            return (name, kind, TNone)
        constrained = do
            expect "("
            name <- identifier
            kind <- kindClause
            expect "<:"
            constraint <- typeExpression
            expect ")"
            return (name, kind, constraint)

typeAll :: Parser Type
typeAll = do
    expect "∀"
    (name, kind, constraint) <- typeParameter
    expect "."
    body <- typeExpression
    return $ TAll name kind constraint body

typeExists :: Parser Type
typeExists = do
    expect "∃"
    (name, kind, constraint) <- typeParameter
    expect "."
    body <- typeExpression
    return $ TExists name kind constraint body

typeFix :: Parser Type
typeFix = do
    expect "fix"
    expect "("
    body <- typeExpression
    expect ")"
    return $ TFix body

typePrimitive :: Parser Type
typePrimitive = do
        voidType
    <|> intType
    <|> boolType
    <|> stringType
    <|> commandType
    <|> namedType
    <|> refType
    <|> recordType
    <|> parens typeExpression
    <|> typeLambda
    <|> typeAll
    <|> typeExists
    <|> typeFix

typeFunction :: Parser Type
typeFunction = typePrimitive `chainl1` (infixOp "->" TFunction)

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

oneOf ts f = do
    case ts of
        [] -> return TVoid
        [t] -> return t
        _ -> return $ f ts

typeApply :: Parser Type
typeApply = do
    a <- typeSum
    rest a
        where
            one a = do
                b <- typeSum
                rest $ TApply a b
            rest a = one a <|> return a

typeExpression :: Parser Type
typeExpression = typeApply

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

caseClause :: Parser ECaseClause
caseClause = do
    name <- identifier
    expect ":"
    typ <- typeExpression
    expect "then"
    body <- primitive
    return (name, typ, body)

inExpr :: Parser Expression
inExpr = do
    expect "in"
    expect "<"
    typ <- typeExpression
    expect ">"
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
            return (name, typ, value)

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
    expect "{"
    thenVal <- expression
    expect "}"
    expect "else"
    expect "{"
    elseVal <- expression
    expect "}"
    return $ EIf value thenVal elseVal

nop :: Parser Expression
nop = do
    expect "nop"
    return ENop

polyLambda :: Parser Expression
polyLambda = do
    expect "Λ"
    (name, kind, constraint) <- typeParameter
    expect "."
    body <- expression
    return $ EPolyLambda name kind constraint body

pack :: Parser Expression
pack = do
    expect "pack"
    expect "<"
    bodyType <- typeExpression
    expect ","
    body <- expression
    expect ">"
    expect "as"
    packType <- typeExpression
    return $ EPack bodyType body packType

open :: Parser Expression
open = do
    expect "open"
    expr <- primitive
    expect "as"
    expect "<"
    name <- identifier
    kind <- kindClause
    expect ","
    var <- identifier
    expect ">"
    expect "in"
    body <- expression
    return $ EOpen expr name kind var body

primitive :: Parser Expression
primitive =
        literal
    <|> voidExpr
    <|> tuple
    <|> lambda
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
    <|> polyLambda
    <|> pack
    <|> open
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

applyType :: Parser Expression
applyType = do
    a <- select
    rest a
        where
            one a = do
                expect "["
                b <- typeExpression
                expect "]"
                rest $ EPolyApply a b
            rest a = one a <|> return a

apply :: Parser Expression
apply = do
    a <- applyType
    rest a
        where
            one a = do
                b <- applyType
                rest $ EApply a b
            rest a = one a <|> return a

assign :: Parser Expression
assign = do
    target <- apply
    assign' target <|> return target
    where
        assign' target = do
            expect ":="
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
lexer s = reserve ["true", "false", "in", "null", "ref", "if",
    "then", "else", "val", "nop", "case", "of", "void", "command",
    "proj", "pack", "as", "open", "bool", "int", "string", "fix"] $ tokens s

expr :: String -> Expression
expr s = runParser expression $ lexer s

typeExpr :: String -> Type
typeExpr s = runParser typeExpression $ lexer s