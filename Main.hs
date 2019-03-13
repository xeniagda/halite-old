{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import Data.Time.Clock

import Interpreter
import Code
import Ast
import Value
import Parser
import Parse

run :: Ast -> IO (UTCTime, UTCTime)
run ast = do
    putStrLn $ "Evaluating:\n" ++ apprint 0 ast

    let code = astToCode ast
        code' = optimizeStricts code

    parseDone <- code' `seq` getCurrentTime

    let v = VThunk [] code'
        initMem = [v]
        memw = weak initMem 0
        weaked = getMem memw 0
        meme = eval memw 0
        evaled = getMem meme 0

    runDone <- evaled `seq` meme `seq` getCurrentTime

    putStrLn $ "Evaluating:\n" ++ apprint 0 ast
    putStrLn $ "Desugared: " ++ cpprint code'

    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] memw
    putStrLn $ "Weak: " ++ vpprint weaked

    putStrLn "Evaled memory: "
    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] meme
    putStrLn $ "Evaled: " ++ vpprint evaled
    putStrLn $ "Evaled (with mem): " ++ vpprintAddr meme 0

    return (parseDone, runDone)

showErr i (line:rest) (err, idx) =
    if idx > T.length line
        then showErr (i + 1) rest (err, idx - T.length line)
        else do
            let lineNr = show (i+1) ++ " | "
                indent = take (idx + length lineNr) $ cycle " "
            putStrLn $ lineNr ++ T.unpack line
            putStrLn $ indent ++ "^ " ++ show err
            putStrLn ""

repl :: IO ()
repl =
    putStrLn "Halite Repl" >> loop
    where
        loop = do
            putStr "> "
            hFlush stdout
            inp <- T.pack <$> getLine

            case doParse parseAst inp 0 of
                Right (ast, len) ->
                    if len == T.length inp
                        then do
                            run ast
                            return ()
                        else putStrLn "Not all parsed!"
                Left errs ->
                    mapM_ (showErr 0 (T.lines inp)) errs

            loop

runFile :: String -> IO ()
runFile filename = do
    inp <- T.pack <$> readFile filename

    start <- getCurrentTime

    case doParse parseAst inp 0 of
        Right (ast, len) ->
            if len == T.length inp
                then do
                    (parseDone, runDone) <- run ast
                    putStrLn $ "Parsing took: " ++ show (parseDone `diffUTCTime` start)
                    putStrLn $ "Running took: " ++ show (runDone `diffUTCTime` parseDone)
                    putStrLn $ "All: " ++ show (runDone `diffUTCTime` start)
                else putStrLn "Not all parsed!"
        Left errs ->
            mapM_ (showErr 0 (T.lines inp)) errs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [file] -> runFile file
        _ -> putStrLn "Please specify one file to run or none to get a repl"
