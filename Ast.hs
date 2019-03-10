module Ast where

import Data.List
import Code

data Ast =
    Ast AstPart
    deriving (Show)

data AstPart
    = ABottom
    | AVar String
    | AToken String
    | ALet [(String, Ast)] Ast
    | ALambda [String] Ast
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
                AToken t -> CToken t
                ALet [] body -> astToCode body
                ALet ((v,exp):rest) body -> CLet v (astToCode exp) $ astCompToCode $ ALet rest body
                ALambda [] body -> astToCode body
                ALambda (v:rest) body -> CLambda v $ astCompToCode $ ALambda rest body
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
                AToken x -> "'" ++ x ++ "'"
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
                ACall fs ->
                    let ppfs = concatMap (\v -> apprint (i + 1) v ++ " ") fs
                    in "(" ++ (take (length ppfs - 1) ppfs) ++ ")"
