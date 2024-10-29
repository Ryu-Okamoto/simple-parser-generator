{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Errors where

import Data.Data ( Data ( toConstr ), showConstr )

class Error e where
    getName :: e -> String
    getLineNumber :: e -> Int

newtype TokenizeError = TokenizeError Int deriving ( Data )
newtype GenerateError = GenerateError Int deriving ( Data )

instance Error TokenizeError where
    getName :: TokenizeError -> String
    getName = showConstr . toConstr
    getLineNumber :: TokenizeError -> Int
    getLineNumber (TokenizeError lineNumber) = lineNumber

instance Error GenerateError where
    getName :: GenerateError -> String
    getName = showConstr . toConstr
    getLineNumber :: GenerateError -> Int
    getLineNumber (GenerateError lineNumber) = lineNumber

getMessage :: Error e => e -> String
getMessage e = getName e ++ ": at line " ++ show (getLineNumber e)

instance Show TokenizeError where
    show :: TokenizeError -> String
    show = getMessage
    
instance Show GenerateError where
    show :: GenerateError -> String
    show = getMessage