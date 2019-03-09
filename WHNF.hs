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

passVar :: String -> Value -> Memory -> Value -> (Memory, Value)
passVar vName vVal mem value =
    case value of
        VThunk x -> passVar vName vVal mem $ intoExpr x
        VVar y ->
            if y == vName
                then (mem, vVal)
                else (mem, value)
        VLet y x c ->
            if y == vName
                then (mem, value)
                else
                    let (mem', x') = passVar vName vVal mem x
                        (mem'', c') = passVar vName vVal mem' c
                    in (mem'', VLet y x' c')
        VLambda y body ->
            if y == vName
                then (mem, value)
                else
                    let (mem', body') = passVar vName vVal mem body
                    in (mem', VLambda y body')
        VCall f x ->
            let (mem', f') = passVar vName vVal mem f
                (mem'', x') = passVar vName vVal mem' x
            in (mem'', VCall f' x')
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', x') = whnf mem x
                        mem'' = update mem' i x'
                    in (mem'', x')
                Nothing -> (mem, value)
        a -> (mem, a)

whnf :: Memory -> Value -> (Memory, Value)
whnf mem val =
    case val of
        VThunk c -> whnf mem $ intoExpr c
        VBottom -> error "bottom whnfed"
        VLet var val body ->
            let (mem1, ref) = alloc mem val
                (mem1', val') = passVar var (VRef ref) mem1 val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 body
            in whnf mem2' body'
        VCall f arg ->
            case whnf mem f of
                (mem', VLambda var body) ->
                    let (mem2, ref) = alloc mem' arg
                        (mem2', body') = passVar var (VRef ref) mem2 body
                    in whnf mem2' body'
                (mem', a) -> (mem', VCall a arg)
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', whnfed) = whnf mem x
                        mem'' = update mem' i whnfed
                    in (mem'', whnfed)
                Nothing -> (mem, val)
        a -> (mem, a)

nf :: Memory -> Value -> (Memory, Value)
nf mem val =
    case val of
        VThunk c -> nf mem $ intoExpr c
        VBottom -> error "bottom nfed"
        VLet var val body ->
            let (mem1, ref) = alloc mem val
                (mem1', val') = passVar var (VRef ref) mem1 val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 body
            in nf mem2' body'
        VCall f arg ->
            let (mem1, arg') = nf mem arg
            in case nf mem1 f of
                (mem1', VLambda var body) ->
                    let (mem2, ref) = alloc mem1' arg'
                        (mem2', body') = passVar var (VRef ref) mem2 body
                    in nf mem2' body'
                (mem1', f') -> (mem1', VCall f' arg')
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', nfed) = nf mem x
                        mem'' = update mem' i nfed
                    in (mem'', nfed)
                Nothing -> (mem, val)
        VLambda var body ->
            let (mem', body') = nf mem body
            in (mem', VLambda var body')
        a -> (mem, a)

run :: Code -> IO ()
run code = do
    putStrLn $ "Evaluating: " ++ cpprint code
    let v = intoExpr code
        (mem, whnfed) = whnf [] v

    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Whnfed: " ++ vpprint whnfed

    let (mem', nfed) = nf mem whnfed

    mapM_ (\(i, v) -> putStrLn $ (show i ++ ": " ++ vpprint v)) $ zip [0..] mem
    putStrLn $ "Nfed: " ++ vpprint nfed

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
