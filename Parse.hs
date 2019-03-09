{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Parser
import Code

import qualified Data.Text as T
import Data.Char
import Data.List
import Data.Traversable
import Control.Applicative

reserved = ["let", "in"]

data HParseError
    = UnexpectedEnd
    | ExpectedAny [T.Text]
    | ExpectCagetory GeneralCategory
    | ReservedWord String
    deriving (Show)

instance ParseError HParseError where
    unexpectedEnd = UnexpectedEnd
    expectedAnyOf = ExpectedAny
    expectedCategory = ExpectCagetory

-- Parses as litte code as possible
-- eg. a b -> CVar a, rest = "b"
parseLazy :: Parser HParseError Code
parseLazy =
    token $ foldl1' (<|>)
        [ parseBottom
        , parseToken
        , parseVar
        , parseLet
        , parens parseCode
        ]

-- Parses as much code as possible
-- eg. a b -> CCall (CVar a) (CVar b), rest = ""
parseCode :: Parser HParseError Code
parseCode =
    token $ foldl1' (<|>)
        [ parens parseCode
        , parseCall
        , parseLet
        , parseLambda
        , parseVar
        , parseToken
        , parseBottom
        ]

parseBottom :: Parser HParseError Code
parseBottom = CBottom <$ (token $ matchChar '!')

parseVar :: Parser HParseError Code
parseVar = do
    name <- token $ some matchLetter
    guardE (ReservedWord name) (not $ name `elem` reserved)
    return $ CVar name

parseToken :: Parser HParseError Code
parseToken = do
    token $ matchChar '\''
    t <- some matchLetter
    token $ matchChar '\''
    return $ CToken t

parseLet :: Parser HParseError Code
parseLet = do

    token $ matchText "let"
    binds <- separated parseBind $ token $ matchChar ';'

    matchChar ';' <|> pure ' '

    token $ matchText "in"

    body <- parseCode

    return $ foldr (\(var, exp) body -> CLet var exp body) body binds

    where parseBind = do
              var <- some matchLetter
              token $ matchChar '='
              exp <- parseCode
              return (var, exp)



parseLambda :: Parser HParseError Code
parseLambda = do
    token $ matchChar '\\'
    var <- token $ some matchLetter
    token $ matchChar '.'
    body <- parseCode

    return $ CLambda var body

parseCall :: Parser HParseError Code
parseCall = do
    fs <- some $ parseLazy

    return $ foldl1' CCall fs
