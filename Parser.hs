module Parser where

import Control.Applicative
import qualified Data.Text as T
import Data.Char
import Data.List

class ParseError e where
    unexpectedEnd :: e
    expectedWord :: T.Text -> e
    expectedLetter :: e
    expectedCategory :: GeneralCategory -> e


data Parser e a =
    Parser { doParse :: T.Text -> Int -> Either [(e, Int)] (a, Int) }


instance Functor (Parser e) where
    fmap f x =
        Parser {
            doParse = \inp idx ->
                case doParse x inp idx of
                    Right (res, newIdx) -> Right (f res, newIdx)
                    Left errs -> Left errs
        }

emap :: (e1 -> e2) -> Parser e1 x -> Parser e2 x
emap f x =
    Parser {
        doParse = \inp idx ->
            case doParse x inp idx of
                Right x -> Right x
                Left errs -> Left $ (\(e, i) -> (f e, i)) <$> errs
    }

emap' :: (e -> e) -> Parser e x -> Parser e x
emap' f x =
    Parser {
        doParse = \inp idx ->
            case doParse x inp idx of
                Right x -> Right x
                Left errs -> Left $ (\(e, i) -> (f e, i)) <$> errs
    }

eimap' :: ((e, Int) -> (e, Int)) -> Parser e x -> Parser e x
eimap' f x =
    Parser {
        doParse = \inp idx ->
            case doParse x inp idx of
                Right x -> Right x
                Left errs -> Left $ f <$> errs
    }


instance Applicative (Parser e) where
    pure val =
        Parser { doParse = \inp idx -> Right (val, idx) }

    p1 <*> p2 =
        Parser {
            doParse = \inp idx ->
                case doParse p1 inp idx of
                    Right (f, idx') ->
                        case doParse p2 inp idx' of
                            Right (v, idx'') -> Right (f v, idx'')
                            Left errs -> Left errs
                    Left errs -> Left errs
            }

epure err =
    Parser { doParse = \inp idx -> Left [(err, idx)] }

instance Monad (Parser e) where
    return = pure
    p >>= f =
        Parser {
            doParse = \inp idx ->
                case doParse p inp idx of
                    Right (v, idx') -> doParse (f v) inp idx'
                    Left errs -> Left errs
            }

instance Alternative (Parser e) where
    empty = epure undefined
    p1 <|> p2 =
        Parser {
            doParse = \inp idx ->
                case (doParse p1 inp idx, doParse p2 inp idx) of
                    (Right v, _) ->
                        Right v
                    (_, Right v) ->
                        Right v
                    (Left errs1, Left errs2) ->
                        Left $ errs1 ++ errs2
        }


guardE :: ParseError e => e -> Bool -> Parser e ()
guardE e True = pure ()
guardE e False = epure e

separated :: Parser e a -> Parser e b -> Parser e [a]
separated par sep = do
    p1 <- par
    rest <- many $ sep >> par
    return $ p1 : rest



token :: (ParseError e) => Parser e a -> Parser e a
token p = do
    matchSpaces
    x <- p
    matchSpaces
    return x

parens :: (ParseError e) => Parser e a -> Parser e a
parens p = do
    token $ matchChar '('
    x <- p
    token $ matchChar ')'
    return x

readChar :: ParseError e => Parser e Char
readChar =
    Parser {
        doParse = \inp idx ->
            if idx < T.length inp
                then Right (inp `T.index` idx, idx + 1)
                else Left [(unexpectedEnd, idx)]
    }


matchChar :: ParseError e => Char -> Parser e Char
matchChar ch =
    Parser {
        doParse = \inp idx ->
            if idx < T.length inp
                then if inp `T.index` idx == ch
                    then Right (ch, idx + 1)
                    else Left [(expectedWord $ T.pack [ch], idx)]
                else Left [(unexpectedEnd, idx)]
    }

matchText :: ParseError e => T.Text -> Parser e T.Text
matchText t =
    emap' (const $ expectedWord t) $
        const t <$> matchString (T.unpack t)
    where
        matchString "" = pure ()
        matchString (ch:rest) = do
            matchChar ch
            matchString rest
            return ()


matchAnyChars :: ParseError e => [Char] -> Parser e Char
matchAnyChars chars = foldl1' (<|>) $ map matchChar chars

matchGCategory :: ParseError e => GeneralCategory -> Parser e Char
matchGCategory cat = do
    ch <- readChar
    guardE (expectedCategory cat) (generalCategory ch == cat)

    return ch

matchDigit :: ParseError e => Parser e Char
matchDigit = matchAnyChars ['0'..'9']

matchInt :: ParseError e => Parser e Int
matchInt = do
    negative <- (True <$ matchChar '-') <|> pure False
    num <- read <$> some matchDigit

    return $ if negative
        then -num
        else num

matchLetter :: ParseError e => Parser e Char
matchLetter =
    emap' (const expectedLetter) $
        foldl1' (<|>) $ map matchGCategory cats
    where cats = [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter]

matchName :: ParseError e => Parser e String
matchName = do
    fst <- matchLetter <|> matchChar '_'
    rest <- many $ (matchLetter <|> matchChar '_' <|> matchDigit)
    return $ fst : rest

matchSpaces :: ParseError e => Parser e T.Text
matchSpaces = fmap T.pack $ many $ matchAnyChars " \n\t"
