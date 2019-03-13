module Value where

import Data.List
import Code

data Value
    = VThunk [(String, Int)] Code
    | VNum Int
    | VConstructor String
    | VLambda String [(String, Int)] Code
    | VBuiltin String (Value -> Maybe Value)
    | VCall Int Int

vpprint :: Value -> String
vpprint code =
    case code of
        VThunk vars c ->
            let ppvars =
                    intercalate "," $ map (\(var, val) -> var ++ "=#" ++ show val) vars
            in "{" ++ cpprint c ++ "}[" ++ ppvars ++ "]"
        VNum i -> show i
        VConstructor x -> x
        VLambda var bound c ->
            let ppbound =
                    intercalate "," $ map (\(var, val) -> var ++ "=#" ++ show val) bound
            in "(\\" ++ var ++ ". {" ++ cpprint c ++ "})[" ++ ppbound ++ "]"
        VBuiltin name _ -> "<builtin " ++ name ++ ">"
        VCall a b -> "(#" ++ show a ++ " #" ++ show b ++ ")"
