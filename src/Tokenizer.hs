module Tokenizer ( tokenize, Token (..) ) where

import Errors ( TokenizeError (..) )
import Data.Char ( isAlpha, isUpper, isAscii, isAlphaNum ) 

data Token = Token { token :: String, lineNumber :: Int }

tokenize :: String -> Either TokenizeError [Token]
tokenize ebnf = do
    tokenss <- mapM tokenizeLine $ zip (lines ebnf) [1..]
    return $ concat tokenss

data DFAState = 
  Init     |
  Terminal |  -- reading a terminal
  Variable |  -- reading a variable
  MetaEq1  |  -- reading "::" of "::="
  MetaEq2  |  -- reading "::=" of "::=" 
  Accept   |
  Fin      |  -- redundancy to identify the end of reading a variable
  Err    

tokenizeLine :: (String, Int) -> Either TokenizeError [Token]
tokenizeLine ("", _) = return []
tokenizeLine (line, number) = do
  (firstToken, _, rest) <- extractFirstToken (line, Init, [])
  restTokens <- tokenizeLine (rest, number)
  return $ Token firstToken number : restTokens
  where
    extractFirstToken :: (String, DFAState, String) -> Either TokenizeError (String, DFAState, String)
    extractFirstToken (extracted, Init, ' ':t) = extractFirstToken (extracted, Init, t)
    extractFirstToken (       _ , Init,    []) = Left $ TokenizeError number
    extractFirstToken (extracted,  Fin,  rest) = return (init extracted, Init, last extracted : rest)
    extractFirstToken (       _ ,  Err,     _) = Left $ TokenizeError number
    extractFirstToken (extracted,    _,    []) = return (extracted, Init, [])
    extractFirstToken (extracted,    q,   h:t) = extractFirstToken (extracted ++ [h], dfaTrans q h, t)

dfaTrans :: DFAState -> Char -> DFAState
dfaTrans Init  '.' = Accept
dfaTrans Init  '|' = Accept
dfaTrans Init  '{' = Accept
dfaTrans Init  '}' = Accept
dfaTrans Init  '[' = Accept
dfaTrans Init  ']' = Accept
dfaTrans Init  ':' = MetaEq1
dfaTrans Init '\"' = Terminal
dfaTrans Init    a    
  | isAlpha a && isUpper a = Variable
  |              otherwise = Err
    
dfaTrans Terminal '\"' = Accept
dfaTrans Terminal a
  | isAscii a = Terminal
  | otherwise = Err

dfaTrans Variable a
  | isAlphaNum a = Variable
  |    otherwise = Accept

dfaTrans MetaEq1 a
  | a == ':' = MetaEq2
  |otherwise = Err
dfaTrans MetaEq2 a
  |  a == '=' = Accept
  | otherwise = Err

dfaTrans Accept _ = Fin
dfaTrans _ _ = Err
  