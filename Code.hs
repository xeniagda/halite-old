module Code where

data Code
    = CBottom
    | CVar String
    | CToken String
    | CLet String Code Code
    | CLet' String Code Code
    -- ^ Strict let statement with no recurion. Does not allocate
    | CLambda String Code
    | CLambda' String Code -- Lambda with only one occurence of the variable in the body
    | CCall Code Code
    deriving (Show)

cpprint :: Code -> String
cpprint code =
    case code of
        CBottom -> "!"
        CVar x -> x
        CToken x -> "'" ++ x ++ "'"
        CLet v val body -> "(let " ++ v ++ " = " ++ cpprint val ++ " in " ++ cpprint body ++ ")"
        CLet' v val body -> "(let' " ++ v ++ " = " ++ cpprint val ++ " in " ++ cpprint body ++ ")"
        CLambda v body -> "(\\" ++ v ++ ". " ++ cpprint body ++ ")"
        CLambda' v body -> "(\\'" ++ v ++ ". " ++ cpprint body ++ ")"
        CCall a b -> "(" ++ cpprint a ++ " " ++ cpprint b ++ ")"

countVar :: String -> Code -> Int
countVar var code =
    go code
    where
        go code =
            case code of
                CVar x -> if x == var then 1 else 0
                CLet v val body ->
                    if v == var
                        then 0
                        else go val + go body
                CLet' v val body ->
                    if v == var
                        then 0
                        else go val + go body
                CLambda v body ->
                    if v == var
                        then 0
                        else go body
                CLambda' v body ->
                    if v == var
                        then 0
                        else go body
                CCall a b -> go a + go b
                _ -> 0

optimizeStricts :: Code -> Code
optimizeStricts code =
    case code of
        CLet v val body ->
            if countVar v val == 0 && countVar v body < 2
                then CLet' v (optimizeStricts val) (optimizeStricts body)
                else CLet v (optimizeStricts val) (optimizeStricts body)
        CLet' v val body -> CLet' v (optimizeStricts val) (optimizeStricts body)
        CLambda v body ->
            if countVar v body < 2
                then CLambda' v (optimizeStricts body)
                else CLambda v (optimizeStricts body)
        CLambda' v body -> CLambda' v (optimizeStricts body)
        CCall a b -> CCall (optimizeStricts a) (optimizeStricts b)
        _ -> code

letm :: [(String, Code)] -> Code -> Code
letm [] x = x
letm ((var, val):rest) code = CLet var val $ letm rest code
