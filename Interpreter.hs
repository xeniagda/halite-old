module Interpreter where

import Data.Maybe
import Data.List
import System.IO.Unsafe

import Code
import Value

numBuiltin :: String -> (Int -> Int -> Value) -> Value
numBuiltin name f =
    VBuiltin name $ \v1 ->
        case v1 of
            VNum x ->
                Just $
                    VBuiltin (name ++ show x)
                        $ \v2 -> case v2 of
                            VNum y -> Just $ f x y
                            _ -> Nothing
            _ -> Nothing


builtins :: [(String, Value)]
builtins =
    [ ("add", numBuiltin "add" (\x y -> VNum $ x + y))
    , ("sub", numBuiltin "sub" (\x y -> VNum $ x - y))
    , ("mul", numBuiltin "mul" (\x y -> VNum $ x * y))
    , ("mod", numBuiltin "mod" (\x y -> VNum $ mod x y))
    , ("eq", numBuiltin "eq" (\x y -> if x == y then VConstructor "True" else VConstructor "False"))
    , ("neq", numBuiltin "neq" (\x y -> if x /= y then VConstructor "True" else VConstructor "False"))
    , ("lt", numBuiltin "lt" (\x y -> if x < y then VConstructor "True" else VConstructor "False"))
    , ("gt", numBuiltin "gt" (\x y -> if x > y then VConstructor "True" else VConstructor "False"))
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
        CMatch c branches ->
            let (mem', addr) = unthunk mem vars c
                mem1 = weak mem' addr
                bindBranch (labels, code) =
                    if labels == ["_"]
                        then Just (code, [])
                        else
                            let init = Just (addr, [])
                                step = \label curr ->
                                    case curr of
                                        Nothing -> Nothing
                                        Just (ref, binds) ->
                                            case getMem mem1 ref of
                                                VCall f x ->
                                                    Just (f, (label,x):binds)
                                                _ -> Nothing
                            in case foldr step init (tail labels) of
                                Just (const, binds) ->
                                    case getMem mem1 const of
                                        VConstructor x | x == head labels ->
                                            Just (code, binds)
                                        _ -> Nothing
                                Nothing -> Nothing
                binds = mapMaybe bindBranch branches
            in case binds of
                [] -> error "No match pattern!"
                (code, binds):_ -> unthunk mem1 (binds++vars) code

        CMatchN c branches ->
            let (mem', addr) = unthunk mem vars c
                mem1 = weak mem' addr
                val = getMem mem1 addr
                bindBranch (number, code) =
                    case number of
                        Nothing -> Just code
                        Just n ->
                            case val of
                                VNum x | x == n -> Just code
                                _ -> Nothing

                binds = mapMaybe bindBranch branches
            in case binds of
                [] -> error "No match pattern!"
                code:_ -> unthunk mem1 vars code



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
                        mem1' = update mem1 addr (getMem mem1 resaddr)
                    in weak mem1' addr
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
                        mem1' = update mem1 addr (getMem mem1 resaddr)
                    in eval mem1' addr
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
