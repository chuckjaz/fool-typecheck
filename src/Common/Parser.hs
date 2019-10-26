module Common.Parser where

import Control.Applicative
import Control.Monad

import Common.Lexer

newtype Parser a = Parser { parse :: [Token] -> [(a,[Token])] }

runParser :: Parser a -> [Token] -> a
runParser m s =
    case parse m s of
        [(res, [])] -> res
        [(_, t)]    -> error ("Parser did not consume entire stream. " ++ show t)
        _           -> error "Parser error."

item :: Parser Token
item = Parser $ \s ->
    case s of
        []     -> []
        (t:ts) -> [(t,ts)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
    fmap f (Parser ts) = Parser (\t -> [(f a, b) | (a, b) <- ts t])

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = 
        Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    return = unit
    (>>=)  = bind

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

instance Alternative Parser where
    empty = mzero
    (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        []     -> parse q s
        res    -> res

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = item `bind` \c ->
    if p c
    then unit c
    else failure

reserved :: String -> Parser Token
reserved s = satisfy (\t -> (TReserved s) == t)

reservedOp :: String -> Parser Token
reservedOp s = satisfy (\t -> (TOperator s) == t)

identifier :: Parser String
identifier = do
    tok <- item
    case tok of
        TIdentifier s -> return s
        _ -> failure

number :: Parser Integer
number = do
    tok <- item
    case tok of 
        TNumber n -> return n
        _ -> failure

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
        a <- p
        rest a
            where
                one a = do 
                    f <- op
                    b <- p
                    rest (f a b) 
                rest a = one a <|> return a


infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reservedOp x >> return f

parens :: Parser a -> Parser a
parens m = do
    reservedOp "("
    r <- m
    reservedOp ")"
    return r

sepBy :: Parser a -> String -> Parser [a]
sepBy p sep = do
    a <- p
    as <- many one
    return (a:as)
    where
        one = do
            reservedOp sep
            a <- p
            return a
