module Value where

import Code

data Value
    = VThunk Code
    | VRef Int
    | VBottom
    | VVar String
    | VToken String
    | VLet String Value Value
    | VLet' String Value Value
    | VLambda String Value
    | VLambda' String Value
    | VCall Value Value
    deriving (Show)

vpprint :: Value -> String
vpprint code =
    case code of
        VThunk c -> "[" ++ cpprint c ++ "]"
        VBottom -> "!"
        VVar x -> x
        VRef i -> "#" ++ show i
        VToken x -> "'" ++ x ++ "'"
        VLet v val body -> "(let " ++ v ++ " = " ++ vpprint val ++ " in " ++ vpprint body ++ ")"
        VLet' v val body -> "(let' " ++ v ++ " = " ++ vpprint val ++ " in " ++ vpprint body ++ ")"
        VLambda v body -> "(\\" ++ v ++ ". " ++ vpprint body ++ ")"
        VLambda' v body -> "(\\'" ++ v ++ ". " ++ vpprint body ++ ")"
        VCall a b -> "(" ++ vpprint a ++ " " ++ vpprint b ++ ")"

intoExpr :: Code -> Value
intoExpr code =
    case code of
        CBottom      -> VBottom
        CVar x       -> VVar x
        CToken x     -> VToken x
        CLet x y z   -> VLet x (VThunk y) (VThunk z)
        CLet' x y z  -> VLet' x (VThunk y) (VThunk z)
        CLambda x y  -> VLambda x (VThunk y)
        CLambda' x y -> VLambda' x (VThunk y)
        CCall x y    -> VCall (VThunk x) (VThunk y)

