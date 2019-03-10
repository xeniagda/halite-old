{-# LANGUAGE OverloadedStrings #-}

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

    let code = astToCode ast
        code' = optimizeStricts code

    parseDone <- code' `seq` getCurrentTime

    let v = VThunk [] code
        (mem, weaked) = weak [] v
        (mem', evaled) = eval mem weaked

    runDone <- evaled `seq` mem' `seq` getCurrentTime

    putStrLn $ "Evaluating:\n" ++ apprint 0 ast
    putStrLn $ "Desugared: " ++ cpprint code'

    -- mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Weak: " ++ vpprint weaked

    -- putStrLn "Evaled memory: "
    -- mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Evaled: " ++ vpprint evaled

    return (parseDone, runDone)

main :: IO ()
main = do
    inp <- T.pack <$> readFile "in.hlt"

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

showErr i (line:rest) (err, idx) =
    if idx >= T.length line
        then showErr (i + 1) rest (err, idx - T.length line)
        else do
            let lineNr = show i ++ " | "
                indent = take (idx + length lineNr) $ cycle " "
            putStrLn $ lineNr ++ T.unpack line
            putStrLn $ indent ++ "^ " ++ show err
            putStrLn ""
