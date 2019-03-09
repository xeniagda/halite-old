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
                    let (mem', x') = weak mem x
                        mem'' = update mem' i x'
                    in (mem'', x')
                Nothing -> (mem, value)
        a -> (mem, a)

weak :: Memory -> Value -> (Memory, Value)
weak mem val =
    case val of
        VThunk c -> weak mem $ intoExpr c
        VBottom -> error "bottom weaked"
        VLet var val body ->
            let (mem1, ref) = alloc mem val
                (mem1', val') = passVar var (VRef ref) mem1 val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 body
            in weak mem2' body'
        VCall f arg ->
            case weak mem f of
                (mem', VLambda var body) ->
                    let (mem2, ref) = alloc mem' arg
                        (mem2', body') = passVar var (VRef ref) mem2 body
                    in weak mem2' body'
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
                (mem1', val') = passVar var (VRef ref) mem1 val
                mem2 = update mem1' ref val'
                (mem2', body') = passVar var val' mem2 body
            in eval mem2' body'
        VCall f arg ->
            let (mem1, arg') = eval mem arg
            in case eval mem1 f of
                (mem1', VLambda var body) ->
                    let (mem2, ref) = alloc mem1' arg'
                        (mem2', body') = passVar var (VRef ref) mem2 body
                    in eval mem2' body'
                (mem1', f') -> (mem1', VCall f' arg')
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', evaled) = eval mem x
                        mem'' = update mem' i evaled
                    in (mem'', evaled)
                Nothing -> (mem, val)
        VLambda var body ->
            let (mem', body') = eval mem body
            in (mem', VLambda var body')
        a -> (mem, a)

