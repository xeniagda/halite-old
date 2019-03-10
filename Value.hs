module Value where

import Data.List
import Code

data Value
    = VThunk [(String, Value)] Code
    | VRef Int
    | VConstructor String
    | VLambda String [(String, Value)] Code
    | VCall Value Value
    deriving (Show)

vpprint :: Value -> String
vpprint code =
    case code of
        VThunk vars c ->
            let ppvars =
                    intercalate "," $ map (\(var, val) -> var ++ "=" ++ vpprint val) vars
            in "{" ++ cpprint c ++ "}[" ++ ppvars ++ "]"
        VRef i -> "#" ++ show i
        VConstructor x -> "'" ++ x ++ "'"
        VLambda var bound c ->
            let ppbound =
                    intercalate "," $ map (\(var, val) -> var ++ "=" ++ vpprint val) bound
            in "(\\" ++ var ++ ". {" ++ cpprint c ++ "})[" ++ ppbound ++ "]"
        VCall a b -> "(" ++ vpprint a ++ " " ++ vpprint b ++ ")"
