{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Parser 
  ( parse ) 
where


import Data.Char 
  ( isUpper )
import Control.Monad 
  ( when )

import Errors 
  ( 
      SyntaxError
    , throwSyntaxError
    , throwSyntaxErrorByLack
  )
import Tokenizer 
  ( Token (..) )
import RestrictedEBNF
  ( 
      Terminal (..)
    , Variable (..)
    , ElementOpt (..)
    , Element (..)
    , Body (..)
    , Head (..)
    , EBNFL (..)
    , EBNF (..) 
  )


parse :: [Token] -> Either SyntaxError EBNF
parse tokens = do
  (ebnf, _) <- parseEBNF tokens
  return ebnf


parseEBNF :: [Token] -> Either SyntaxError (EBNF, [Token])
parseEBNF tokens = do
  (ebnfl, rest) <- parseEBNFL tokens
  when (null rest) throwSyntaxErrorByLack
  let (Token token lineNumber):rest2 = rest
  when (token /= ".") (throwSyntaxError lineNumber)
  (ebnflList, rest3) <- parseEBNFLList rest2
  return (EBNF ebnfl ebnflList, rest3)


parseEBNFL :: [Token] -> Either SyntaxError (EBNFL, [Token])
parseEBNFL tokens = do
  (head_, rest) <- parseHead tokens
  when (null rest) throwSyntaxErrorByLack
  let (Token token lineNumber):rest2 = rest
  when (token /= "::=") (throwSyntaxError lineNumber)
  (body, rest3) <- parseBody rest2
  (bodyList, rest4) <- parseBodyList rest3
  return (EBNFL head_ body bodyList, rest4)


parseEBNFLList :: [Token] -> Either SyntaxError ([EBNFL], [Token])
parseEBNFLList [] = return ([], [])
parseEBNFLList tokens = do
  (ebnfl, rest) <- parseEBNFL tokens
  when (null rest) throwSyntaxErrorByLack
  let (Token token lineNumber):rest2 = rest
  when (token /= ".") (throwSyntaxError lineNumber)
  (ebnflList, rest3) <- parseEBNFLList rest2
  return (ebnfl : ebnflList, rest3)


parseHead :: [Token] -> Either SyntaxError (Head, [Token])
parseHead tokens = do
  (variable, rest) <- parseVariable tokens
  return (Head variable, rest)


parseBody :: [Token] -> Either SyntaxError (Body, [Token])
parseBody tokens = do
  (element, rest) <- parseElement tokens
  (elements, rest2) <- parseElementList rest
  return (Body element elements, rest2)


parseBodyList :: [Token] -> Either SyntaxError ([Body], [Token])
parseBodyList [] = return ([], [])
parseBodyList tokens@((Token token _):rest)
  | token == "|" = do
    (body, rest2) <- parseBody rest
    (bodyList, rest3) <- parseBodyList rest2
    return (body : bodyList, rest3)
  | otherwise = return ([], tokens) 


parseElement :: [Token] -> Either SyntaxError (Element, [Token])
parseElement tokens@((Token token lineNumber):rest)
  | isUpper tokenHead = do
    (variable, rest2) <- parseVariable tokens
    return (AtomV variable, rest2)
  | tokenHead == '"' = do
    (terminal, rest2) <- parseTerminal tokens
    return (AtomT terminal, rest2)
  | token == "{" = do
    (elementOpt, rest2) <- parseElementOpt rest
    when (null rest2) (throwSyntaxError lineNumber)
    let (Token token2 lineNumber2):rest3 = rest2
    when (token2 /= "}") (throwSyntaxError lineNumber2)
    return (Repeat elementOpt, rest3) 
  | token == "[" = do
    (elementOpt, rest2) <- parseElementOpt rest
    when (null rest2) (throwSyntaxError lineNumber)
    let (Token token2 lineNumber2):rest3 = rest2 
    when (token2 /= "]") (throwSyntaxError lineNumber2)
    return (Option elementOpt, rest3) 
  | otherwise = throwSyntaxError lineNumber
  where
    tokenHead = head token
parseElement [] = throwSyntaxErrorByLack


parseElementList :: [Token] -> Either SyntaxError ([Element], [Token])
parseElementList [] = return ([], [])
parseElementList tokens@((Token token _):_)
  | isUpper tokenHead || tokenHead == '"' || token `elem` ["{", "["] = do
    (element, rest2) <- parseElement tokens
    (elementList, rest3) <- parseElementList rest2
    return (element : elementList, rest3)
  | otherwise = return ([], tokens)
  where 
    tokenHead = head token


parseElementOpt :: [Token] -> Either SyntaxError (ElementOpt, [Token])
parseElementOpt tokens@((Token token lineNumber):_)
  | tokenHead == '"' = do
    (terminal, rest) <- parseTerminal tokens
    (elementOpt, rest2) <- parseElementOpt rest
    return (PrevV terminal elementOpt, rest2)
  | isUpper tokenHead = do
    (variable, rest) <- parseVariable tokens
    (terminalList, rest2) <- parseTerminalList rest
    return (PostV variable terminalList, rest2)
  | otherwise = throwSyntaxError lineNumber
  where
    tokenHead = head token
parseElementOpt [] = throwSyntaxErrorByLack


parseTerminalList :: [Token] -> Either SyntaxError ([Terminal], [Token])
parseTerminalList [] = return ([], [])
parseTerminalList tokens@(Token token lineNumber:_)
  | tokenHead == '"' = do
    (terminal, rest) <- parseTerminal tokens
    (terminals, rest2) <- parseTerminalList rest
    return (terminal : terminals, rest2)
  | otherwise = return ([], tokens)
  where
    tokenHead = head token


parseVariable :: [Token] -> Either SyntaxError (Variable, [Token])
parseVariable ((Token token lineNumber):rest)
  | isUpper tokenHead = return (Variable token, rest)
  | otherwise = throwSyntaxError lineNumber
  where
    tokenHead = head token
parseVariable [] = throwSyntaxErrorByLack


parseTerminal :: [Token] -> Either SyntaxError (Terminal, [Token])
parseTerminal ((Token token lineNumber):rest)
  | tokenHead == '"' = return (Terminal token, rest)
  | otherwise = throwSyntaxError lineNumber
  where
    tokenHead = head token
parseTerminal [] = throwSyntaxErrorByLack