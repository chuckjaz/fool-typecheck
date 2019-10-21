module Chapter8.Syntax where

data Type
    = Type String
    | Function Type Type
    | TypeError String
    deriving (Show, Eq)

data Literal
    = LInteger Integer
    | LBoolean Bool
    deriving (Show, Eq)

-- M N in
data Expression
    = ELiteral Literal                -- c
    | EVariable String                -- x
    | ELambda String Type Expression  -- \(x:T).M
    | EApply Expression Expression    -- M N
    | ExpressionError String          -- !
    deriving (Show, Eq)
