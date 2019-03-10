module Ast where

import Data.List
import Code

data Ast =
    Ast AstPart
    deriving (Show)

data AstPart
    = ABottom
    | AVar String
    | AConstructor String
    | ALet [(String, Ast)] Ast
    | ALambda [String] Ast
    | AMatch Ast [([String], Ast)]
    | ACall [Ast]
    deriving (Show)


astToCode :: Ast -> Code
astToCode (Ast part) =
    astCompToCode part
    where
        astCompToCode part =
            case part of
                ABottom -> CBottom
                AVar x -> CVar x
                AConstructor t -> CConstructor t
                ALet [] body -> astToCode body
                ALet ((v,exp):rest) body -> CLet v (astToCode exp) $ astCompToCode $ ALet rest body
                ALambda [] body -> astToCode body
                ALambda (v:rest) body -> CLambda v $ astCompToCode $ ALambda rest body
                AMatch x branches ->
                    CMatch (astToCode x) $ map (\(pat, branch) -> (pat, astToCode branch)) branches
                ACall lst -> foldl1' CCall $ map astToCode lst

indentOf :: Int -> String
indentOf x = take (x * 4) $ cycle " "

apprint :: Int -> Ast -> String
apprint i (Ast part) =
    appprint part
    where
        indent = indentOf i
        appprint part =
            case part of
                ABottom -> "!"
                AVar x -> x
                AConstructor x -> x
                ALet binds body ->
                    let sep = "\n" ++ indentOf (i + 1)
                        ppbinds = map (\(v, e) -> v ++ " = " ++ apprint (i + 2) e ++ ";" ) binds
                        ppbinds' = intercalate sep ppbinds
                        ppbody = apprint (i + 1) body
                    in "let " ++ ppbinds' ++ "\n" ++ indent ++ "in " ++ ppbody
                ALambda vars body ->
                    let ppvars = intercalate " " vars
                        ppbody = apprint (i + 1) body
                    in "λ" ++ ppvars ++ ". " ++ ppbody
                AMatch x branches ->
                    let sep = "\n" ++ indentOf (i + 1)
                        ppbranches =
                            map (\(p, b) ->
                                intercalate " " p ++ " ->\n"
                                ++ indentOf (i+2) ++ apprint (i+2) b ++ ";"
                            )
                            branches
                        ppbranches' = intercalate sep ppbranches

                    in "match (" ++ apprint (i+1) x ++
                        ") {" ++ sep ++ ppbranches' ++ "\n" ++ indent ++ "}"
                ACall fs ->
                    let ppfs = concatMap (\v -> apprint (i + 1) v ++ " ") fs
                    in "(" ++ (take (length ppfs - 1) ppfs) ++ ")"
