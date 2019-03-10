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
    | ExpectedAnyOf [T.Text]
    | ExpectedLetter
    | ExpectCagetory GeneralCategory
    | ReservedWord String
    deriving (Show, Eq)

instance ParseError HParseError where
    unexpectedEnd = UnexpectedEnd
    expectedWord x = ExpectedAnyOf [x]
    expectedLetter = ExpectedLetter
    expectedCategory = ExpectCagetory

    groupe (e1, idx1) (e2, idx2) =
        if idx1 == idx2
            then case (e1, e2) of
                (ExpectedAnyOf as, ExpectedAnyOf bs) -> Just (ExpectedAnyOf $ nub $ as ++ bs, idx1)
                (a, b) | a == b -> Just (a, idx1)
                _ -> Nothing
            else Nothing


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
        [ parseCall
        , parseLet
        , parseLambda
        , parseVar
        , parseToken
        , parseBottom
        , parens parseAst
        ]

parseBottom :: Parser HParseError Ast
parseBottom = (Ast ABottom) <$ (token $ matchChar '!')

parseVar :: Parser HParseError Ast
parseVar = do
    (Ast . AVar) <$>
        guardE
            (\name -> if name `elem` reserved then Just $ ReservedWord name else Nothing)
            matchName

parseToken :: Parser HParseError Ast
parseToken = do
    token $ matchChar '\''
    t <- matchName
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
              var <- matchName
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
parseCall =
    (Ast . ACall) <$>
        guardE (\fs ->
            if length fs < 2
                then Just UnexpectedEnd
                else Nothing
        ) (some parseLazy)
