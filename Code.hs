module Code where

data Code
    = CBottom
    | CVar String
    | CToken String
    | CLet String Code Code
    | CLambda String Code
    | CCall Code Code
    deriving (Show)

cpprint :: Code -> String
cpprint code =
    case code of
        CBottom -> "!"
        CVar x -> x
        CToken x -> "'" ++ x ++ "'"
        CLet v val body -> "(" ++ v ++ " = " ++ cpprint val ++ " in " ++ cpprint body ++ ")"
        CLambda v body -> "(\\" ++ v ++ ". " ++ cpprint body ++ ")"
        CCall a b -> "(" ++ cpprint a ++ " " ++ cpprint b ++ ")"

letm :: [(String, Code)] -> Code -> Code
letm [] x = x
letm ((var, val):rest) code = CLet var val $ letm rest code
