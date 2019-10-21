module Chapter8.TypeCheck where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Chapter8.Syntax
import Chapter8.Parse (expr)

-- Definition 8.1.1 - Free identifiers

freeIdentifiers :: Expression -> Set.Set String
freeIdentifiers expr = case expr of
    ELiteral _                  -> Set.empty
    EVariable name              -> Set.singleton name
    ELambda parameter _ body    -> Set.delete parameter $ freeIdentifiers body
    EApply target parameter     -> Set.union (freeIdentifiers target) (freeIdentifiers parameter)

-- 8.1 -- Type checking rules

type TypeEnvironment = Map.Map String Type

errorType message = TypeError ("ERROR: " ++ message)
booleanType = Type "Boolean"
integerType = Type "Integer"

typeCheck :: Expression -> Type
typeCheck expression = let (typ, _) = typeCheck' expression Map.empty in
        typ
    where
        typeCheck' :: Expression -> TypeEnvironment -> (Type, TypeEnvironment)

        -- Identifier
        typeCheck' (EVariable name) env =
            case Map.lookup name env of
                Just typ    -> (typ, env)
                Nothing     -> 
                    (errorType ("Undefined variable '" ++ name ++ "'"), env)

        -- Constant
        typeCheck' (ELiteral (LInteger _)) env = 
            (integerType, env)
        typeCheck' (ELiteral (LBoolean _)) env =
            (booleanType, env)

        -- Function
        typeCheck' (ELambda parameter parameterType body) env =
            let env' = Map.insert parameter parameterType env in
            let (bodyType, _) = typeCheck' body env' in
                ((Function parameterType bodyType), env)

        -- Application
        typeCheck' (EApply target parameter) env =
            let (targetType, env') = typeCheck' target env in
            let (parameterType, env'') = typeCheck' parameter env' in
                case targetType of 
                    Function parameterType' resultType ->
                        if parameterType == parameterType' then
                            (resultType, env'')
                        else
                            (errorType ("Expected parameter of type " ++ 
                                show parameterType' ++
                                ", received type " ++ show parameterType), env)
                    _ -> (errorType "Expected a function type", env)

        -- Parens (removed by the parser)

typeCheckExpression :: String -> Type
typeCheckExpression text = typeCheck (expr text)
