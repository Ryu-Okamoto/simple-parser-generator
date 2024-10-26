{-# LANGUAGE StandaloneDeriving #-}

module RestrictedEBNF where

data     EBNF        =  EBNF EBNFL [EBNFL] 
data     EBNFL       =  EBNFL Head Body [Body] 
newtype  Head        =  Head Variable 
data     Body        =  Body Element [Element]
data     Element     =  AtomV Variable | AtomT Terminal | Option ElementOpt | Repeat ElementOpt
data     ElementOpt  =  PrevV Terminal ElementOpt | PostV Variable [Terminal]
newtype  Variable    =  Variable String
newtype  Terminal    =  Terminal String

deriving instance Show EBNF
deriving instance Show EBNFL
deriving instance Show Head
deriving instance Show Body
deriving instance Show Element
deriving instance Show ElementOpt
deriving instance Show Variable
deriving instance Show Terminal

deriving instance Eq Head
deriving instance Eq Element
deriving instance Eq ElementOpt
deriving instance Eq Variable
deriving instance Eq Terminal