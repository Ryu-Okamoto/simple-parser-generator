{-# LANGUAGE InstanceSigs #-}

module Tokenizer 
  ( 
      tokenize
    , Token (..)
  ) 
where


import Errors 
  ( 
      LexicalError
    , throwLexicalError
  )
import Data.Char 
  ( 
      isAlpha
    , isNumber
    , isUpper
    , isAlphaNum
    , isPrint
  ) 


data Token = Token 
  {
    tkToken :: String,
    tkLineNumber :: Int
  } 
  deriving ( Eq )

instance Show Token where
  show :: Token -> String
  show token = "(" ++ tkToken token ++ ", " ++ show (tkLineNumber token) ++ ")"


tokenize :: String -> Either LexicalError [Token]
tokenize ebnf = do
  tokensLines <- mapM tokenizeLine $ zip (lines ebnf) [1,2 ..]
  return $ concat tokensLines


data DFAState = 
    Init
  | Term               -- reading a terminal
  | Escape             -- reading a escape sequence
  | Var                -- reading a variable
  | MetaEq1            -- reading "::" of "::="
  | MetaEq2            -- reading "::=" of "::=" 
  | OverRead           -- over reading for identifying the end of a variable or macro
  | Macro              -- reading a macro
  | Accept    
  | Err    
  deriving ( Show, Eq )


tokenizeLine :: (String, Int) -> Either LexicalError [Token]
tokenizeLine (  "",          _) = return []
tokenizeLine (line, lineNumber) = do
  (firstToken, _, rest) <- extractFirstToken ([], Init, line)
  validateMacro firstToken
  restTokens <- tokenizeLine (rest, lineNumber)
  return $ Token firstToken lineNumber : restTokens
  where
    extractFirstToken :: (String, DFAState, String) -> Either LexicalError (String, DFAState, String)
    extractFirstToken (extracted,     Init, ' ':t) = extractFirstToken (extracted, Init, t)
    extractFirstToken (extracted,   Accept,  rest) = return (extracted, Init, rest)
    extractFirstToken (extracted, OverRead,  rest) = return (init extracted, Init, last extracted : rest)
    extractFirstToken (        _,      Err,     _) = throwLexicalError lineNumber
    extractFirstToken (        _,     Term,    []) = throwLexicalError lineNumber
    extractFirstToken (extracted,        _,    []) = return (extracted, Init, [])
    extractFirstToken (extracted,        q,   h:t) = extractFirstToken (extracted ++ [h], dfaTrans q h, t)

    validateMacro :: String -> Either LexicalError ()
    validateMacro token@('@':_)
      | token `elem` availableMacros = return ()
      | otherwise = throwLexicalError lineNumber
      where
        availableMacros = 
          [
              "@ALPHANUM"
            , "@ALPHA"
            , "@UPPERCASE"
            , "@LOWERCASE"
            , "@NUMBER"
            , "@PRINTABLE"
          ]
    validateMacro _ = return ()


dfaTrans :: DFAState -> Char -> DFAState
dfaTrans Init ':' = MetaEq1
dfaTrans Init '\"' = Term
dfaTrans Init '@' = Macro
dfaTrans Init a
  | a `elem` ['.', '|', '{', '}', '[', ']'] = Accept
  | isAlpha a && isUpper a = Var
  | otherwise = Err

dfaTrans Term '\"' = Accept
dfaTrans Term a
  | a == '\\' = Escape
  | isPrint a = Term
  | otherwise = Err

dfaTrans Escape a
  | a `elem` ['n', 't', '"', '\\'] = Term
  | otherwise = Err

dfaTrans Var a
  | isAlphaNum a = Var
  | otherwise = OverRead

dfaTrans MetaEq1 a
  | a == ':' = MetaEq2
  |otherwise = Err
dfaTrans MetaEq2 a
  | a == '=' = Accept
  | otherwise = Err

dfaTrans Macro a
  | isAlpha a && isUpper a = Macro
  | isNumber a = Macro
  | otherwise = OverRead

dfaTrans _ _ = Err
  