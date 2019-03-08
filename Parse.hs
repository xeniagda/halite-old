{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Parser
import Code

import qualified Data.Text as T
import Data.Char
import Data.List
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
        , parseBottom
        , parseToken
        , parseLet
        , parseLambda
        , parseCall
        , parseVar
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
    var <- token $ some matchLetter
    token $ matchChar '='
    exp <- parseCode
    token $ matchText "in"
    body <- parseCode

    return $ CLet var exp body


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
