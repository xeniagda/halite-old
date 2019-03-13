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

getMem :: Memory -> Int -> Value
getMem mem at =
    mem !! at

update :: Memory -> Int -> Value -> Memory
update mem at val =
    (take at mem) ++ [val] ++ (drop (at + 1) mem)


unthunk :: Memory -> [(String, Int)] -> Code -> (Memory, Int)
unthunk mem vars code =
    case code of
        CBottom -> (mem, error "Bottom unthunked")
        CVar x ->
            case lookup x builtins of
                Just builtin -> alloc mem builtin
                Nothing ->
                    case lookup x vars of
                        Just addr -> (mem, addr)
                        Nothing -> error $ "Undefined variable " ++ x
        CNum x -> alloc mem $ VNum x
        CConstructor x -> alloc mem $ VConstructor x
        CLet var cval cbody ->
            let (mem1, addr) = alloc mem (error "dummy")
                (mem1', addr2) = unthunk mem1 ((var,addr):vars) cval
                mem2 = update mem1' addr (getMem mem1' addr2)
            in unthunk mem2 ((var,addr):vars) cbody
        CLambda x y  -> alloc mem $ VLambda x vars y
        CCall f x    ->
            let (memf, f') = unthunk mem vars f
                (memx, x') = unthunk memf vars x
            in alloc memx $ VCall f' x'

-- Weaks the memory stored in addr
weak :: Memory -> Int -> Memory
weak mem addr =
    case getMem mem addr of
        VThunk vars code ->
            let (mem', addr') = unthunk mem vars code
                mem'' = update mem' addr (getMem mem' addr')
            in weak mem'' addr
        VCall f arg ->
            let memf = weak mem f
            in case getMem memf f of
                VLambda var vars code ->
                    let (mem1, resaddr) = unthunk mem ((var,arg):vars) code
                    in weak mem1 resaddr
                VBuiltin _ bf ->
                    let mema = eval memf arg
                        varg = getMem mema arg
                    in case bf varg of
                        Just x -> update mem addr x
                        Nothing -> mema
                _ -> memf
        a -> mem


eval :: Memory -> Int -> Memory
eval mem addr =
    case getMem mem addr of
        VThunk vars code ->
            let (mem', addr') = unthunk mem vars code
                mem'' = update mem' addr (getMem mem' addr')
            in eval mem'' addr
        VCall f arg ->
            let memf = eval mem f
            in case getMem memf f of
                VLambda var vars code ->
                    let (mem1, resaddr) = unthunk mem ((var,arg):vars) code
                    in eval mem1 resaddr
                VBuiltin _ bf ->
                    let mema = eval memf arg
                        varg = getMem mema arg
                    in case bf varg of
                        Just x -> update mem addr x
                        Nothing -> mema
                _ ->
                    let mema = eval memf arg
                    in mema
        a -> mem
