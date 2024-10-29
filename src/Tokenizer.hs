module Tokenizer ( tokenize, Token (..) ) where

import Errors ( TokenizeError (..) )
import Data.Char ( isAlpha, isUpper, isAscii, isAlphaNum ) 

data Token = Token { tkToken :: String, tkLineNumber :: Int } deriving ( Eq, Show )

tokenize :: String -> Either TokenizeError [Token]
tokenize ebnf = do
  tokensLines <- mapM tokenizeLine $ zip (lines ebnf) [1,2 ..]
  return $ concat tokensLines

data DFAState = 
  Init     |
  Term     |  -- reading a terminal
  Var      |  -- reading a variable
  MetaEq1  |  -- reading "::" of "::="
  MetaEq2  |  -- reading "::=" of "::=" 
  OverRead |  -- over reading for identifying the end of a variable
  Accept   |
  Err    
  deriving ( Show, Eq )


tokenizeLine :: (String, Int) -> Either TokenizeError [Token]
tokenizeLine ("", _) = return []
tokenizeLine (line, lineNumber) = do
  (firstToken, _, rest) <- extractFirstToken ([], Init, line)
  restTokens <- tokenizeLine (rest, lineNumber)
  return $ Token firstToken lineNumber : restTokens
  where
    extractFirstToken :: (String, DFAState, String) -> Either TokenizeError (String, DFAState, String)
    extractFirstToken (extracted,     Init, ' ':t) = extractFirstToken (extracted, Init, t)
    extractFirstToken (extracted,   Accept,  rest) = return (extracted, Init, rest)
    extractFirstToken (extracted, OverRead,  rest) = return (init extracted, Init, last extracted : rest)
    extractFirstToken (        _,      Err,     _) = Left $ TokenizeError lineNumber
    extractFirstToken (extracted,        _,    []) = return (extracted, Init, [])
    extractFirstToken (extracted,        q,   h:t) = extractFirstToken (extracted ++ [h], dfaTrans q h, t)

dfaTrans :: DFAState -> Char -> DFAState
dfaTrans Init  '.' = Accept
dfaTrans Init  '|' = Accept
dfaTrans Init  '{' = Accept
dfaTrans Init  '}' = Accept
dfaTrans Init  '[' = Accept
dfaTrans Init  ']' = Accept
dfaTrans Init  ':' = MetaEq1
dfaTrans Init '\"' = Term
dfaTrans Init    a    
  | isAlpha a && isUpper a = Var
  |              otherwise = Err
    
dfaTrans Term '\"' = Accept
dfaTrans Term    a
  | isAscii a = Term
  | otherwise = Err

dfaTrans Var a
  | isAlphaNum a = Var
  |    otherwise = OverRead

dfaTrans MetaEq1 a
  | a == ':' = MetaEq2
  |otherwise = Err
dfaTrans MetaEq2 a
  |  a == '=' = Accept
  | otherwise = Err

dfaTrans _ _ = Err
  