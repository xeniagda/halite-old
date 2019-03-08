import System.IO.Unsafe

data Code
    = CBottom
    | CVar String
    | CToken String
    | CLet String Code Code
    | CLambda String Code
    | CCall Code Code
    deriving (Show)

letm :: [(String, Code)] -> Code -> Code
letm [] x = x
letm ((var, val):rest) code = CLet var val $ letm rest code

data Value
    = VThunk Code
    | VRef Int
    | VBottom
    | VVar String
    | VToken String
    | VLet String Value Value
    | VLambda String Value
    | VCall Value Value
    deriving (Show)

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

intoExpr :: Code -> Value
intoExpr code =
    case code of
        CBottom     -> VBottom
        CVar x      -> VVar x
        CToken x    -> VToken x
        CLet x y z  -> VLet x (VThunk y) (VThunk z)
        CLambda x y -> VLambda x (VThunk y)
        CCall x y   -> VCall (VThunk x) (VThunk y)

passVar :: String -> Value -> Value -> Value
passVar vName vVal value =
    case value of
        VThunk x -> passVar vName vVal $ intoExpr x
        VVar y -> if y == vName then vVal else value
        VLet y x c -> if y == vName then value else VLet y x $ passVar vName vVal c
        VLambda y c -> if y == vName then value else VLambda y $ passVar vName vVal c
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
