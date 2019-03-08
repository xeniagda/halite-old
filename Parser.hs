module Parser where

import Control.Applicative
import qualified Data.Text as T
import Data.Char
import Data.List

class ParseError e where
    unexpectedEnd :: e
    expectedAnyOf :: [T.Text] -> e
    expectedCategory :: GeneralCategory -> e


data Parser e a =
    Parser { doParse :: T.Text -> Int -> (Either e a, Int) }


instance Functor (Parser e) where
    fmap f x =
        Parser {
            doParse = \inp idx ->
                case doParse x inp idx of
                    (Right res, newIdx) -> (Right $ f res, newIdx)
                    (Left err, newIdx) -> (Left err, newIdx)
        }

emap f x =
    Parser {
        doParse = \inp idx ->
            case doParse x inp idx of
                (Right res, newIdx) -> (Right res, newIdx)
                (Left err, newIdx) -> (Left $ f err, newIdx)
    }


instance Applicative (Parser e) where
    pure val =
        Parser { doParse = \inp idx -> (Right val, idx) }

    p1 <*> p2 =
        Parser {
            doParse = \inp idx ->
                case doParse p1 inp idx of
                    (Right f, idx') ->
                        case doParse p2 inp idx' of
                            (Right v, idx'') -> (Right $ f v, idx'')
                            (Left err, newIdx) -> (Left err, newIdx)
                    (Left err, newIdx) -> (Left err, newIdx)
            }

epure err =
    Parser { doParse = \inp idx -> (Left err, idx) }

instance Monad (Parser e) where
    return = pure
    p >>= f =
        Parser {
            doParse = \inp idx ->
                case doParse p inp idx of
                    (Right v, idx') -> doParse (f v) inp idx'
                    (Left e, idx') -> (Left e, idx')
            }

instance Alternative (Parser e) where
    empty = epure undefined
    p1 <|> p2 =
        Parser {
            doParse = \inp idx ->
                case doParse p1 inp idx of
                    (Right val, idx') -> (Right val, idx')
                    (Left _, _) ->
                        case doParse p2 inp idx of
                            (Right val, idx') -> (Right val, idx')
                            (Left e, idx') -> (Left e, idx')
        }

guardE :: ParseError e => e -> Bool -> Parser e ()
guardE e True = pure ()
guardE e False = epure e

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
                then (Right $ inp `T.index` idx, idx + 1)
                else (Left unexpectedEnd, idx)
    }


matchChar :: ParseError e => Char -> Parser e Char
matchChar ch = do
    ch' <- readChar
    guardE (expectedAnyOf [T.pack [ch]]) (ch' == ch)

    return ch'

matchText :: ParseError e => T.Text -> Parser e T.Text
matchText t = const t <$> matchString (T.unpack t)
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
    foldl1' (<|>) $ map matchGCategory cats
    where cats = [UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter]


matchSpaces :: ParseError e => Parser e T.Text
matchSpaces = fmap T.pack $ many $ matchAnyChars " \n\t"
