{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import Interpreter
import Code
import Ast
import Value
import Parser
import Parse

run :: Ast -> IO ()
run ast = do
    putStrLn $ "Evaluating:\n" ++ apprint 0 ast

    let code = astToCode ast

    putStrLn $ "Desugared: " ++ cpprint code
    let v = intoExpr code
        (mem, weaked) = weak [] v

    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Weak: " ++ vpprint weaked

    let (mem', evaled) = eval mem weaked

    putStrLn "Evaled memory: "
    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Evaled: " ++ vpprint evaled

main :: IO ()
main = do
    inp <- T.pack <$> readFile "in.hlt"

    case doParse parseAst inp 0 of
        Right (ast, len) ->
            if len == T.length inp
                then run ast
                else putStrLn "Not all parsed!"
        Left errs ->
            mapM_ (showErr 0 (T.lines inp)) errs

showErr i (line:rest) (err, idx) =
    if idx >= T.length line
        then showErr (i + 1) rest (err, idx - T.length line)
        else do
            let lineNr = show i ++ " | "
                indent = take (idx + length lineNr) $ cycle " "
            putStrLn $ lineNr ++ T.unpack line
            putStrLn $ indent ++ "^ " ++ show err
            putStrLn ""
