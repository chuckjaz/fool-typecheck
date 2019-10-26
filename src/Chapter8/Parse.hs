module Chapter8.Parse where

import Control.Applicative
import Control.Monad

import Common.Parser
import Common.Lexer

import Chapter8.Syntax

typeName :: Parser Type
typeName = do 
    name <- identifier
    return $ Type name

typePrim :: Parser Type
typePrim = do
    typeName <|> parens typeExpr

typeFunction :: Parser (Type -> Type -> Type)
typeFunction =  infixOp "->" Function

typeExpr :: Parser Type
typeExpr = typePrim `chainl1` typeFunction

-----

literal :: Parser Expression
literal = trueLiteral <|> falseLiteral <|> numberLiteral
    where
        numberLiteral = do
            value <- number
            return $ ELiteral (LInteger value)
        trueLiteral = do
            reserved "True"
            return $ ELiteral (LBoolean True)
        falseLiteral = do
            reserved "False"
            return $ ELiteral (LBoolean False)
            
variable :: Parser Expression
variable = do
    name <- identifier
    return $ EVariable name

lambda :: Parser Expression
lambda = do 
    reservedOp "\\"
    reservedOp "("
    parameter <- identifier
    reservedOp ":"
    parameterType <- typeExpr
    reservedOp ")"
    reservedOp "."
    body <- expression
    return $ ELambda parameter parameterType body


primitive :: Parser Expression
primitive = 
        literal
    <|> variable
    <|> lambda
    <|> parens expression

apply :: Parser Expression
apply = do
    a <- primitive
    rest a
        where
            one a = do 
                b <- primitive
                rest $ EApply a b
            rest a = one a <|> return a

expression :: Parser Expression
expression = apply

lexer s = reserve ["True", "False"] $ tokens s

expr :: String -> Expression
expr text = runParser expression $ lexer text
