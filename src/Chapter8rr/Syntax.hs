module Chapter8rr.Syntax where

-- 8.2.1
data Type
    = TType String                          -- C
    | TVoid                                 -- Void
    | TFunction Type Type                   -- T -> T
    | TTuple [Type]                         -- T x ... x T
    | TSum [Type]                           -- T + ... + T
    | TRecord [(String, Type)]              -- {l: T; ...; l: T}
    | TRef Type                             -- Ref T
    | TCommand                              -- Command
    | TOpen
    | TError String
    deriving (Show, Eq)

data Literal
    = LNumber Integer
    | LBoolean Bool
    deriving (Show, Eq)

data Expression
    = EVariable String                      -- x
    | EConstant Literal                     -- c
    | EVoid                                 -- <>
    | ELambda String Type Expression        -- λ(x:T).M
    | EApply Expression Expression          -- M N
    | ETuple [Expression]                   -- <M, ..., M>
    | EProjection Integer Expression        -- Proj[i](M)
    | ECase Expression                      -- case M of x: T then E || ... || x: T then E
        [(String, Type, Expression)]
    | EIn Type Expression                   -- in T...T(M)
    | ERecord [(String, Type, Expression)]  -- {l: T := M, ..., l: T := M}
    | ESelect Expression String             -- M.l
    | ERef Expression                       -- ref M
    | ENull                                 -- null
    | EVal Expression                       -- val M
    | EIf Expression Expression Expression  -- if B then { M } else { N }
    | ENop                                  -- nop
    | EAssign Expression Expression         -- M := N
    | ESequence Expression Expression       -- M ; N
    deriving (Show, Eq)
