module Chapter9.Syntax where

data Kind 
    = KStar                       -- *
    | KFunction Kind Kind         -- k => k'
    | KError String
    deriving (Eq, Ord, Show)

-- Definition 9.1.1
type TFieldClause = (String, Type)

data Type
    = TVariable String Kind          -- v
    | TType String Kind              -- c
    | TVoid                          -- Void
    | TFunction Type Type            -- m -> n
    | TTuple [Type]                  -- m*m*...*m*m
    | TSum [Type]                    -- m+m+...+m+m
    | TRecord [TFieldClause]         -- {l:m, ..., l:m}
    | TRef Type                      -- Ref m
    | TCommand                       -- Command
    | TLambda String Kind Type       -- λv^k.m
    | TApply Type Type               -- m v
    | TAll String Kind Type Type     -- ∀v^k.T | ∀(v^k: m).T
    | TExists String Kind Type Type  -- ∃v^k.T | ∃(v^k: m).T
    | TFix Type                      -- Fix(m)
    | TError String
    | TNone
    deriving (Eq, Ord, Show)

data Literal
    = LNumber Integer
    | LBoolean Bool
    deriving (Show, Eq)

type ECaseClause = (String, Type, Expression)
type EFieldClause = (String, Type, Expression)

data Expression
    = EVariable String                               -- x
    | EConstant Literal                              -- c
    | EVoid                                          -- <>
    | ELambda String Type Expression                 -- λ(x:T).M
    | EApply Expression Expression                   -- M N
    | ETuple [Expression]                            -- <M, ..., M>
    | EProjection Integer Expression                 -- Proj[i](M)
    | ECase Expression [ECaseClause]                 -- case M of x: T then E || ... || x: T then E
    | EIn Type Expression                            -- in T...T(M)
    | ERecord [EFieldClause]                         -- {l: T := M, ..., l: T := M}
    | ESelect Expression String                      -- M.l
    | ERef Expression                                -- ref M
    | ENull                                          -- null
    | EVal Expression                                -- val M
    | ENop                                           -- nop
    | EAssign Expression Expression                  -- M := N
    | EIf Expression Expression Expression           -- if B then { M } else { N }
    | ESequence Expression Expression                -- M ; N
    | EPolyLambda String Kind Type Expression        -- Λv^k.M | Λ(v^k <: m).M
    | EPolyApply Expression Type                     -- M[m]
    | EPack Type Expression Type                     -- pack <m, M> as ∃v^k.T | pack <m, M> as ∃(v^k <: m).T
    | EOpen Expression String Kind String Expression -- open M as <v^k,x> in N
    deriving (Show, Eq)
