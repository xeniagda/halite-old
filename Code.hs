module Code where

import Data.List

data Code
    = CBottom
    | CVar String
    | CNum Int
    | CConstructor String
    | CLet String Code Code
    -- | CLet' String Code Code
    -- ^ Strict let statement with no recurion. Does not allocate
    | CLambda String Code
    -- | CLambda' String Code
    -- ^ Lambda with only one occurence of the variable in the body
    | CMatch Code [([String], Code)]
    | CMatchN Code [(Maybe Int, Code)]
    | CCall Code Code
    deriving (Show)

cpprint :: Code -> String
cpprint code =
    case code of
        CBottom -> "!"
        CVar x -> x
        CNum x -> show x
        CConstructor x -> x
        CLet v val body -> "(let " ++ v ++ " = " ++ cpprint val ++ " in " ++ cpprint body ++ ")"
        CLambda v body -> "(\\" ++ v ++ ". " ++ cpprint body ++ ")"
        CMatchN x branches ->
            let ppbranches =
                    concatMap
                        (\(pat, branch) ->
                            let pppat = case pat of
                                    Just n -> show n
                                    Nothing -> "_"
                            in pppat ++ " -> " ++ cpprint branch ++ "; ")
                        branches
            in "match (" ++ cpprint x ++ ") {" ++ ppbranches ++ "}"
        CMatch x branches ->
            let ppbranches =
                    concatMap
                        (\(pat, branch) -> intercalate " " pat ++ " -> " ++ cpprint branch ++ "; ")
                        branches
            in "match (" ++ cpprint x ++ ") {" ++ ppbranches ++ "}"
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
                CLambda v body ->
                    if v == var
                        then 0
                        else go body
                CCall a b -> go a + go b
                CMatch x branches ->
                    let branchesCount = map (go . snd) branches
                    in sum branchesCount + go x
                CMatchN x branches ->
                    let branchesCount = map (go . snd) branches
                    in sum branchesCount + go x
                _ -> 0

optimizeStricts :: Code -> Code
optimizeStricts code = code
    -- case code of
    --     CLet v val body ->
    --         if countVar v val == 0 && countVar v body < 2
    --             then CLet' v (optimizeStricts val) (optimizeStricts body)
    --             else CLet v (optimizeStricts val) (optimizeStricts body)
    --     CLet' v val body -> CLet' v (optimizeStricts val) (optimizeStricts body)
    --     CLambda v body ->
    --         if countVar v body < 2
    --             then CLambda' v (optimizeStricts body)
    --             else CLambda v (optimizeStricts body)
    --     CLambda' v body -> CLambda' v (optimizeStricts body)
    --     CCall a b -> CCall (optimizeStricts a) (optimizeStricts b)
    --     _ -> code

letm :: [(String, Code)] -> Code -> Code
letm [] x = x
letm ((var, val):rest) code = CLet var val $ letm rest code
