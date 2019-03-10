module Value where

import Data.List
import Code

data Value
    = VThunk [(String, Value)] Code
    | VRef Int
    | VNum Int
    | VConstructor String
    | VLambda String [(String, Value)] Code
    | VBuiltin String (Value -> Maybe Value)
    | VCall Value Value

vpprint :: Value -> String
vpprint code =
    case code of
        VThunk vars c ->
            let ppvars =
                    intercalate "," $ map (\(var, val) -> var ++ "=" ++ vpprint val) vars
            in "{" ++ cpprint c ++ "}[" ++ ppvars ++ "]"
        VRef i -> "#" ++ show i
        VNum i -> show i
        VConstructor x -> "'" ++ x ++ "'"
        VLambda var bound c ->
            let ppbound =
                    intercalate "," $ map (\(var, val) -> var ++ "=" ++ vpprint val) bound
            in "(\\" ++ var ++ ". {" ++ cpprint c ++ "})[" ++ ppbound ++ "]"
        VBuiltin name _ -> "<builtin " ++ name ++ ">"
        VCall a b -> "(" ++ vpprint a ++ " " ++ vpprint b ++ ")"
