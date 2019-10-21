module Common.Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Token
    =   TIdentifier String
    |   TOperator String
    |   TNumber Integer
    |   TError String
    deriving (Show, Eq)

tokens :: String -> [Token]
tokens text = 
    case text of
        (c:cs) | isSpace c      -> tokens cs
        (c:_)  | isDigit c      -> tokenOf isDigit stringToNumber text
        (c:_)  | isAlpha c      -> tokenOf isAlphaNum TIdentifier text
        (c:cs) | isSingleOp c   -> [TOperator [c]] ++ tokens cs
        (c:_)  | isOperator c   -> tokenOf isOperator TOperator text
        []                      -> [] 
        (c:cs)                  -> [TError "Invalid character"] ++ tokens cs
    where
        takeWhile :: (Char -> Bool) -> String -> (String, String)
        takeWhile p text = takeWhile' p text []
            where
                takeWhile' p (c:cs) prefix | p c = takeWhile' p cs (prefix ++ [c])
                takeWhile' p text' prefix = (prefix, text')

        tokenOf :: (Char -> Bool) -> (String -> Token) -> String -> [Token]
        tokenOf p toToken text =
            let (value, rest) = takeWhile p text in
                (toToken value):(tokens rest)

        stringToNumber :: String -> Token
        stringToNumber s = TNumber (read s :: Integer)
        
        oneOf :: String -> Char -> Bool  
        oneOf (c:cs) ch | c == ch = True
        oneOf (_:cs) ch = oneOf cs ch
        oneOf [] _ = False

        isSingleOp = oneOf "():[]{}"
        isOperator = oneOf "+-*/&^|\\<>=."
