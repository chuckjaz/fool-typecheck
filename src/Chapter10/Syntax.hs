module Chapter10.Syntax where

import qualified Data.Map as Map

data Type
    = TType String
    | TVariable String
    | TFunction [Type] Type
    | TRef Type
    | TObject Type
    | TVisObject Type Type
    | TClass Type Type
    | TRecord [TField]
    deriving (Eq, Ord, Show)

type TField = (String, Type)

data Program = Program String Block
    deriving (Eq, Ord, Show)

data Block = Block TDefs CDefs Stmt Exp
    deriving (Eq, Ord, Show)

type TDefs = Map.Map String Type

type CDef = (Type, Exp)

type CDefs = Map.Map String CDef

data Literal
    = LBoolean Bool
    | LInteger Integer
    | LString String
    deriving (Eq, Ord, Show)

data Exp
    = EVariable String
    | EConstant Literal
    | ENil
    | EVoid
    | EVal Exp
    | ERef Exp
    | ECall Exp [Exp]
    | EFunction [EParam] Type Block
    | EClass (Maybe (Exp, [String])) Exp Exp
    | ENew Exp
    | ESend Exp String
    | ESelect Exp String
    | ERecord [EField]
    deriving (Eq, Ord, Show)

type EParam = (String, Type)
type EField = (String, Type, Exp)

data Stmt
    = SNop
    | SAssign String Exp
    | SIf Exp Stmt Stmt
    | SWhile Exp Stmt
    | SSequence Stmt Stmt
    deriving (Eq, Ord, Show)
