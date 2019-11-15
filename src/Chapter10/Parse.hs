module Chapter10.Parse where

import Chapter10.Syntax

import Common.Parser
import Common.Lexer

import Control.Applicative
import Control.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

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

-- Definition 10.2.2 - Pre-expressions
program :: Parser Program
program = do
    expect "Program"
    id <- identifier
    expect ";"
    blk <- block
    return $ Program id blk

block :: Parser Block
block = do
    types <- typeSection
    constants <- constSection
    expect "{"
    stmt <- statement
    expect "return"
    result <- expression
    expect "}"
    return $ Block types constants stmt result

typeSection :: Parser TDefs
typeSection = section <|> return Map.empty
    where
        section = do
            expect "type"
            types <- sepBy typeDef ";"
            return $ Map.fromList types
        typeDef = do
            name <- identifier
            expect "="
            t <- typeExpression
            return (name, t)

constSection :: Parser CDefs
constSection = section <|> return Map.empty
    where
        section = do
            expect "const"
            constants <- sepBy constDef ";"
            return $ Map.fromList constants
        constDef = do
            name <- identifier
            expect ":"
            t <- typeExpression
            expect "="
            value <- expression
            return (name, (t, value))

variable :: Parser Exp
variable = do
    name <- identifier
    return $ EVariable name

intValue :: Parser Exp
intValue = do
    value <- number
    return $ EConstant $ LInteger value

boolValue :: Parser Exp
boolValue = do
    value <- boolLit
    return $ EConstant $ LBoolean value
    where 
        true = do
            expect "true"
            return True
        false = do
            expect "false"
            return False
        boolLit = true <|> false

nilValue :: Parser Exp
nilValue = do
    expect "nil"
    return ENil

voidValue :: Parser Exp
voidValue = do
    expect "()"
    return EVoid

valValue :: Parser Exp
valValue = do
    expect "val"
    value <- primitiveExpression
    return $ EVal value

refValue :: Parser Exp
refValue = do
    expect "ref"
    value <- primitiveExpression
    return $ ERef value

functionValue :: Parser Exp
functionValue = do
    expect "function"
    expect "("
    params <- param `sepBy` ";"
    expect ")"
    expect ":"
    t <- typeExpression
    expect "is"
    blk <- block
    return $ EFunction params t blk
    where
        param = do
            name <- identifier
            expect ":"
            t <- typeExpression
            return (name, t)

classValue :: Parser Exp
classValue = do
    expect "class"
    baseClass <- baseClause
    expect "("
    ri <- recordValue
    expect ","
    rm <- recordValue
    expect ")"
    return $ EClass baseClass ri rm
    where
        baseClause = inheritClause <|> return Nothing
        inheritClause = do
            expect "inherits"
            base <- expression
            expect "modifies"
            names <- identifier `sepBy` ","
            return $ Just (base, names)

recordValue :: Parser Exp
recordValue = do
    expect "{|"
    fields <- field `sepBy` ";"
    expect "|}"
    return $ ERecord fields
    where
        field = do
            name <- identifier
            expect ":"
            t <- typeExpression
            expect "="
            value <- expression
            return (name, t, value)

primitiveExpression :: Parser Exp
primitiveExpression = 
        intValue
    <|> boolValue
    <|> nilValue
    <|> voidValue
    <|> valValue
    <|> refValue
    <|> variable
    <|> functionValue
    <|> classValue
    <|> parens expression

callExpression :: Parser Exp
callExpression = do
    value <- primitiveExpression
    call value
    where
        call v = justExpr v <|> callExpr v <|> voidCallExpr v
        justExpr v = return v
        callExpr v = do
            expect "("
            arguments <- expression `sepBy` ","
            expect ")"
            return $ ECall v arguments
        voidCallExpr v = do
            argument <- voidValue
            return $ ECall v [argument]

expression :: Parser Exp
expression = callExpression

nopStatement :: Parser Stmt
nopStatement = do
    expect "nop"
    return SNop

assignStatement :: Parser Stmt
assignStatement = do
    name <- identifier
    expect ":="
    value <- expression
    return $ SAssign name value

ifStatement :: Parser Stmt
ifStatement = do
    expect "if"
    condition <- expression
    expect "then"
    expect "{"
    thenStatement <- statement
    expect "}"
    expect "else"
    expect "{"
    elseStatement <- statement
    expect "}"
    return $ SIf condition thenStatement elseStatement
    
whileStatement :: Parser Stmt
whileStatement = do
    expect "while"
    expect "("
    condition <- expression
    expect ")"
    expect "do"
    expect "{"
    body <- statement
    expect "}"
    return $ SWhile condition body

primitiveStatement :: Parser Stmt
primitiveStatement = 
        nopStatement
    <|> assignStatement
    <|> ifStatement
    <|> whileStatement

statement :: Parser Stmt
statement = do
    first <- primitiveStatement
    rest first
    where
        rest first = another first <|> return first
        another first = do
            expect ";"
            second <- primitiveStatement
            rest $ SSequence first second

builtInTypes = Set.fromList ["int", "bool", "string", "real"]

lexer :: String -> [Token]
lexer s = reserve ["bool", "int", "string", "real", "ref", "val",
    "return", "const", "type", "true", "false", "nil", "function", "is",
    "inherits", "modifies", "if", "then", "else", "while", "do", "nop",
    "ObjectType", "VisObjectType", "ClassType", "Program"] $ tokens s

typeExpr :: String -> Type
typeExpr s = runParser typeExpression $ lexer s