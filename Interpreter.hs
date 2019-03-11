module Interpreter where

import Data.Maybe
import Data.List
import System.IO.Unsafe

import Code
import Value

numBuiltin :: String -> (Int -> Int -> Int) -> Value
numBuiltin name f =
    VBuiltin name $ \v1 ->
        case v1 of
            VNum x ->
                Just $
                    VBuiltin (name ++ show x)
                        $ \v2 -> case v2 of
                            VNum y -> Just $ VNum $ f x y
                            _ -> Nothing
            _ -> Nothing

builtins :: [(String, Value)]
builtins =
    [ ("add", numBuiltin "add" (+))
    , ("sub", numBuiltin "sub" (-))
    , ("mul", numBuiltin "mul" (*))
    ]

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


unthunk :: Memory -> [(String, Value)] -> Code -> (Memory, Value)
unthunk mem vars code =
    case code of
        CBottom -> (mem, error "Bottom unthunked")
        CVar x ->
            case lookup x builtins of
                Just builtin -> (mem, builtin)
                Nothing ->
                    case lookup x vars of
                        Just val -> (mem, val)
                        Nothing -> error $ "Undefined variable " ++ x
        CNum x -> (mem, VNum x)
        CConstructor x -> (mem, VConstructor x)
        CLet var cval cbody ->
            let (mem1, ref) = alloc mem (error "dummy")
                (mem1', val') = unthunk mem1 ((var,(VRef ref)):vars) cval
                mem2 = update mem1' ref val'
            in unthunk mem2 ((var,(VRef ref)):vars) cbody

        CLet' var cval cbody ->
            let (mem1, val) = unthunk mem vars cval
                (mem1', ref) = alloc mem1 val
            in unthunk mem1' ((var,(VRef ref)):vars) cbody

        CLambda x y  -> (mem, VLambda x vars y)
        CLambda' x y -> (mem, VLambda x vars y) -- TODO: Add VLambda'
        CMatch x branches ->
            let (mem', x') = uncurry weak $ unthunk mem vars x
                bindTo pat =
                    if pat == ["_"]
                        then Just []
                        else
                            let res = foldr
                                    (\tok curr ->
                                        case curr of
                                            Just (Just at, bound) ->
                                                case at of
                                                    VConstructor x ->
                                                        if x == tok
                                                            then Just (Nothing, bound)
                                                            else Nothing
                                                    VCall x y ->
                                                        Just (Just x, (tok,y):bound)
                                                    _ -> Nothing
                                            _ -> Nothing
                                    ) (Just (Just x', [])) pat
                        in case res of
                            Just (Nothing, bound) -> Just bound
                            _ -> Nothing
                matches = mapMaybe
                    (\(pat, branch) ->
                        case bindTo pat of
                            Just bound -> Just (bound, branch)
                            Nothing -> Nothing
                    ) branches
                (bound, branch) = case matches of
                    a:_ -> a
                    [] -> error "No patterns matching"
            in unthunk mem' (bound ++ vars) branch
        CMatchN x branches ->
            let (mem', x') = uncurry weak $ unthunk mem vars x
                bindTo pat =
                    case x' of
                        VNum n ->
                            case pat of
                                Left v -> Just [(v, x')]
                                Right n' ->
                                    if n == n'
                                        then Just []
                                        else Nothing
                        _ -> Nothing -- TODO: Optimize if x is not a number
                matches = mapMaybe
                    (\(pat, branch) ->
                        case bindTo pat of
                            Just bound -> Just (bound, branch)
                            Nothing -> Nothing
                    ) branches
                (bound, branch) = case matches of
                    a:_ -> a
                    [] -> error "No patterns matching"
            in unthunk mem' (bound ++ vars) branch
        CCall x y    -> (mem, VCall (VThunk vars x) (VThunk vars y))

weak :: Memory -> Value -> (Memory, Value)
weak mem val =
    case val of
        VThunk vars code -> uncurry weak $ unthunk mem vars code
        VCall f arg ->
            case weak mem f of
                (mem', VLambda var vars code) ->
                    uncurry weak $ unthunk mem ((var,arg):vars) code
                (mem', f'@(VBuiltin _ bf)) ->
                    let (mem1', arg') = weak mem' arg
                    in case bf arg' of
                        Just x -> weak mem1' x
                        Nothing -> (mem1', VCall f' arg')
                (mem', f') -> (mem', VCall f' arg)
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
        VThunk vars code -> uncurry eval $ unthunk mem vars code
        VCall f arg ->
            case eval mem f of
                (mem1, VLambda var vars code) ->
                    uncurry eval $ unthunk mem1 ((var,arg):vars) code
                (mem', f'@(VBuiltin _ bf)) ->
                    let (mem1', arg') = eval mem arg
                    in case bf arg' of
                        Just x -> eval mem1' x
                        Nothing -> (mem1', VCall f' arg')
                (mem1, f') ->
                    let (mem1', arg') = eval mem arg
                    in (mem1', VCall f' arg')
        VRef i ->
            case getMem mem i of
                Just x ->
                    let (mem', evaled) = eval mem x
                        mem'' = update mem' i evaled
                    in (mem'', evaled)
                Nothing -> (mem, val)
        a -> (mem, a)
