{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Errors where


import Data.Data 
  ( 
      Data 
        ( toConstr )
    , showConstr 
  )


class Error e where
    getName :: e -> String
    getLineNumber :: e -> Int


newtype LexicalError = LexicalError Int deriving ( Data )

instance Error LexicalError where
    getName :: LexicalError -> String
    getName = showConstr . toConstr
    getLineNumber :: LexicalError -> Int
    getLineNumber (LexicalError lineNumber) = lineNumber

instance Show LexicalError where
    show :: LexicalError -> String
    show = getMessage

throwLexicalError :: Int -> Either LexicalError a
throwLexicalError lineNumber = Left $ LexicalError lineNumber


newtype SyntaxError = SyntaxError Int deriving ( Data )

instance Error SyntaxError where
    getName :: SyntaxError -> String
    getName = showConstr . toConstr
    getLineNumber :: SyntaxError -> Int
    getLineNumber (SyntaxError lineNumber) = lineNumber

instance Show SyntaxError where
    show :: SyntaxError -> String
    show = getMessage

throwSyntaxError :: Int -> Either SyntaxError a
throwSyntaxError lineNumber = Left $ SyntaxError lineNumber

throwSyntaxErrorByLack :: Either SyntaxError a
throwSyntaxErrorByLack = Left $ SyntaxError 1


getMessage :: Error e => e -> String
getMessage e = getName e ++ ": at line " ++ show (getLineNumber e)