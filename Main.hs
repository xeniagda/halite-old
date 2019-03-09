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

    let (parsed, len) = doParse parseAst inp 0
    if len == T.length inp
        then case parsed of
            Right code -> run code
            Left err -> putStrLn $ show err
        else
            putStrLn "Did not parse all" >>
            putStrLn ("len = " ++ show len ++ ", parsed = " ++ show parsed)
