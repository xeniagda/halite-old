module Interpreter where

import System.IO.Unsafe

import Code
import Value


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

passVar :: String -> Value -> Memory -> [Int] -> Value -> (Memory, Value)
passVar vName vVal mem visited value =
    case value of
        VThunk x -> passVar vName vVal mem visited $ intoExpr x
        VVar y ->
            if y == vName
                then (mem, vVal)
                else (mem, value)
        VLet y x c ->
            if y == vName
                then (mem, value)
                else
                    let (mem', x') = passVar vName vVal mem visited x
                        (mem'', c') = passVar vName vVal mem' visited c
                    in (mem'', VLet y x' c')
        VLet' y x c ->
            if y == vName
                then (mem, value)
                else
                    let (mem', x') = passVar vName vVal mem visited x
                        (mem'', c') = passVar vName vVal mem' visited c
                    in (mem'', VLet' y x' c')
        VLambda y body ->
            if y == vName
                then (mem, value)
                else
                    let (mem', body') = passVar vName vVal mem visited body
                    in (mem', VLambda y body')
        VLambda' y body ->
            if y == vName
                then (mem, value)
                else
                    let (mem', body') = passVar vName vVal mem visited body
                    in (mem', VLambda' y body')
        VCall f x ->
            let (mem', f') = passVar vName vVal mem visited f
                (mem'', x') = passVar vName vVal mem' visited x
            in (mem'', VCall f' x')
        VRef i ->
            if i `elem` visited
                then (mem, VRef i)
                else case getMem mem i of
                    Just x ->
                        let (mem', passed) = passVar vName vVal mem (i: visited) x
                            mem'' = update mem' i passed
                        in (mem'', passed)
                    Nothing -> (mem, value)
        a -> (mem, a)

weak :: Memory -> Value -> (Memory, Value)
weak mem val =
    case val of
        VThunk c -> weak mem $ intoExpr c
        VBottom -> error "bottom weaked"
        VLet var val body ->
            let (mem1, ref) = alloc mem val
                (mem1', val') = passVar var (VRef ref) mem1 [] val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 [] body
            in weak mem2' body'
        VLet' var val body ->
            let (mem', body') = passVar var val mem [] body
            in weak mem' body'
        VCall f arg ->
            case weak mem f of
                (mem', VLambda var body) ->
                    let (mem2, ref) = alloc mem' arg
                        (mem2', body') = passVar var (VRef ref) mem2 [] body
                    in weak mem2' body'
                (mem1', VLambda' var body) ->
                    let (mem2, body') = passVar var arg mem1' [] body
                    in weak mem2 body'
                (mem', a) -> (mem', VCall a arg)
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', weaked) = weak mem x
                        mem'' = update mem' i weaked
                    in (mem'', weaked)
                Nothing -> (mem, val)
        a -> (mem, a)

eval :: Memory -> Value -> (Memory, Value)
eval mem val =
    case val of
        VThunk c -> eval mem $ intoExpr c
        VBottom -> error "bottom evaled"
        VLet var val body ->
            let (mem1, ref) = alloc mem val
                (mem1', val') = passVar var (VRef ref) mem1 [] val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 [] body
            in eval mem2' body'
        VLet' var val body ->
            let (mem', body') = passVar var val mem [] body
            in eval mem' body'
        VCall f arg ->
            case eval mem f of
                (mem1', VLambda var body) ->
                    let (mem2, ref) = alloc mem1' arg
                        (mem2', body') = passVar var (VRef ref) mem2 [] body
                    in eval mem2' body'
                (mem1', VLambda' var body) ->
                    let (mem2, body') = passVar var arg mem1' [] body
                    in eval mem2 body'
                (mem1', f') ->
                    let (mem1, arg') = eval mem arg
                    in (mem1', VCall f' arg')
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', evaled) = eval mem x
                        mem'' = update mem' i evaled
                    in (mem'', evaled)
                Nothing -> (mem, val)
        a -> (mem, a)

