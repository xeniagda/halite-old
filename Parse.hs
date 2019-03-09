{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Parser
import Ast

import qualified Data.Text as T
import Data.Char
import Data.List
import Data.Traversable
import Control.Applicative

reserved = ["let", "in"]

data HParseError
    = UnexpectedEnd
    | ExpectedAny T.Text
    | ExpectedLetter
    | ExpectCagetory GeneralCategory
    | ReservedWord String
    deriving (Show)

instance ParseError HParseError where
    unexpectedEnd = UnexpectedEnd
    expectedWord = ExpectedAny
    expectedLetter = ExpectedLetter
    expectedCategory = ExpectCagetory

-- Parses as litte code as possible
-- eg. a b -> AVar a, rest = "b"
parseLazy :: Parser HParseError Ast
parseLazy =
    token $ foldl1' (<|>)
        [ parseBottom
        , parseToken
        , parseVar
        , parseLet
        , parens parseAst
        ]

-- Parses as much code as possible
-- eg. a b -> ACall (AVar a) (AVar b), rest = ""
parseAst :: Parser HParseError Ast
parseAst =
    token $ foldl1' (<|>)
        [ parens parseAst
        , parseCall
        , parseLet
        , parseLambda
        , parseVar
        , parseToken
        , parseBottom
        ]

parseBottom :: Parser HParseError Ast
parseBottom = (Ast ABottom) <$ (token $ matchChar '!')

parseVar :: Parser HParseError Ast
parseVar = do
    name <- token $ some matchLetter
    guardE (ReservedWord name) (not $ name `elem` reserved)
    return $ Ast $ AVar name

parseToken :: Parser HParseError Ast
parseToken = do
    token $ matchChar '\''
    t <- some matchLetter
    token $ matchChar '\''
    return $ Ast $ AToken t

parseLet :: Parser HParseError Ast
parseLet = do

    token $ matchText "let"
    binds <- separated parseBind $ token $ matchChar ';'

    matchChar ';' <|> pure ' '

    token $ matchText "in"

    body <- parseAst

    return $ Ast $ ALet binds body

    where parseBind = do
              var <- some matchLetter
              token $ matchChar '='
              exp <- parseAst
              return (var, exp)



parseLambda :: Parser HParseError Ast
parseLambda = do
    token $ matchChar '\\'
    vars <- some $ token $ some matchLetter
    token $ matchChar '.'
    body <- parseAst

    return $ Ast $ ALambda vars body

parseCall :: Parser HParseError Ast
parseCall = do
    fs <- some $ parseLazy

    guardE UnexpectedEnd $ length fs > 1
    return $ Ast $ ACall fs
