{-# LANGUAGE OverloadedStrings #-}

import System.IO.Unsafe
import qualified Data.Text as T

import Code
import Value
import Parse
import Parser


type Memory = [Value]

alloc :: Memory -> Value -> (Memory, Int)
alloc mem val = (mem ++ [val], length mem)

getMem :: Memory -> Int -> Maybe Value
getMem mem at =
    if at < length mem
        then Just $ mem !! at
        else Nothing

update :: Memory -> Int -> Value -> Memory
update mem at val =
    (take at mem) ++ [val] ++ (drop (at + 1) mem)

passVar :: String -> Value -> Value -> Value
passVar vName vVal value =
    case value of
        VThunk x -> passVar vName vVal $ intoExpr x
        VVar y ->
            if y == vName
                then vVal
                else value
        VLet y x c ->
            if y == vName
                then value
                else VLet y (passVar vName vVal x) $ passVar vName vVal c
        VLambda y c ->
            if y == vName
                then value
                else VLambda y $ passVar vName vVal c
        VCall y c -> VCall (passVar vName vVal y) (passVar vName vVal c)
        a -> a

weak :: Memory -> Value -> (Memory, Value)
weak mem val = case val of
    VThunk c -> weak mem $ intoExpr c
    VBottom -> error "bottom weaked"
    VRef i ->
        case getMem mem i of
            Nothing -> (mem, val)
            Just x -> weak mem x
    a -> (mem, a)

whnf :: Memory -> Value -> (Memory, Value)
whnf mem val =
    case val of
        VThunk c -> whnf mem $ intoExpr c
        VBottom -> error "bottom whnfed"
        VLet var val body ->
            let (mem', ref) = alloc mem val
                val' = passVar var (VRef ref) val
                mem'' = update mem ref val'
                body' = passVar var val' body
            in whnf mem'' body'
        VCall f arg ->
            case whnf mem f of
                (mem', VLambda var body) ->
                    let (mem'', ref) = alloc mem' arg
                        body' = passVar var (VRef ref) body
                    in whnf mem'' body'
                (mem', a) -> (mem', VCall a arg)
        VRef i ->
            case getMem mem i of
                Just x -> whnf mem x
                Nothing -> (mem, val)
        a -> (mem, a)


run :: Code -> IO ()
run code = do
    putStrLn $ "Evaluating: " ++ cpprint code
    let v = intoExpr code
        (mem, whnfed) = whnf [] v

    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Whnfed: " ++ vpprint whnfed

main :: IO ()
main = do

    inp <- T.pack <$> readFile "in.hlt"

    let (parsed, len) = doParse parseCode inp 0
    if len == T.length inp
        then case parsed of
            Right code -> run code
            Left err -> putStrLn $ show err
        else
            putStrLn "Did not parse all" >>
            putStrLn ("len = " ++ show len ++ ", parsed = " ++ show parsed)
