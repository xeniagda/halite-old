module Interpreter where

import Data.List
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

-- passVar :: String -> Value -> Memory -> [Int] -> Value -> (Memory, Value)
-- passVar vName vVal mem visited value =
--     case value of
--         VThunk vars x -> (mem, VThunk ((vName,vVal):vars) x)
--         VVar y ->
--             if y == vName
--                 then (mem, vVal)
--                 else (mem, value)
--         VLambda y body ->
--             if y == vName
--                 then (mem, value)
--                 else
--                     let (mem', body') = passVar vName vVal mem visited body
--                     in (mem', VLambda y body')
--         VLambda' y body ->
--             if y == vName
--                 then (mem, value)
--                 else
--                     let (mem', body') = passVar vName vVal mem visited body
--                     in (mem', VLambda' y body')
--         VCall f x ->
--             let (mem', f') = passVar vName vVal mem visited f
--                 (mem'', x') = passVar vName vVal mem' visited x
--             in (mem'', VCall f' x')
--         VRef i ->
--             if i `elem` visited
--                 then (mem, VRef i)
--                 else case getMem mem i of
--                     Just x ->
--                         let (mem', passed) = passVar vName vVal mem (i: visited) x
--                             mem'' = update mem' i passed
--                         in (mem'', passed)
--                     Nothing -> (mem, value)
--         a -> (mem, a)

-- passVars :: Memory -> [(String, Value)] -> Value -> (Memory, Value)
-- passVars vars val =
--     foldl'
--         (\(mem1, curr) (var,val) -> passVar var val mem1 [] curr)
--         (mem', unthunked) vars

unthunk :: Memory -> [(String, Value)] -> Code -> (Memory, Value)
unthunk mem vars code =
    case code of
        CBottom -> (mem, error "Bottom unthunked")
        CVar x ->
            case vars of
                [] -> error $ "Undefined variable " ++ x
                (var,val):rest | var == x -> (mem, val)
                _:rest -> unthunk mem rest code
        CToken x -> (mem, VToken x)

        CLet var cval cbody ->
            let (mem1, ref) = alloc mem (error "dummy")
                (mem1', val') = unthunk mem1 ((var,(VRef ref)):vars) cval
                mem2 = update mem1' ref val'
            in unthunk mem2 ((var,(VRef ref)):vars) cbody

        CLet' var cval cbody ->
            let val = VThunk vars cval
            in unthunk mem ((var,val):vars) cbody

        CLambda x y  -> (mem, VLambda x vars y)
        CLambda' x y -> (mem, VLambda x vars y) -- TODO: Add VLambda'
        CCall x y    -> (mem, VCall (VThunk vars x) (VThunk vars y))

weak :: Memory -> Value -> (Memory, Value)
weak mem val =
    case val of
        VThunk vars code -> uncurry weak $ unthunk mem vars code
        VCall f arg ->
            case weak mem f of
                (mem', VLambda var vars code) ->
                    uncurry weak $ unthunk mem ((var,arg):vars) code
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
