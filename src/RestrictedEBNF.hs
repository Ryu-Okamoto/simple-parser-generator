{-# LANGUAGE StandaloneDeriving #-}

module RestrictedEBNF where


data     EBNF        =  EBNF EBNFL [EBNFL] 
data     EBNFL       =  EBNFL Head Body [Body] 
newtype  Head        =  Head Variable 
data     Body        =  Body Element [Element]
data     Element     =  AtomV Variable | AtomT Terminal | Repeat ElementOpt | Option ElementOpt
data     ElementOpt  =  PrevV Terminal (Maybe ElementOpt) | PostV Variable [Terminal]
newtype  Variable    =  Variable String
data     Terminal    =  Terminal String | Macro Macro
data     Macro       =  ALPHANUM | ALPHA | UPPERCASE | LOWERCASE | NUMBER | PRINTABLE

deriving instance Show EBNF
deriving instance Show EBNFL
deriving instance Show Head
deriving instance Show Body
deriving instance Show Element
deriving instance Show ElementOpt
deriving instance Show Variable
deriving instance Show Terminal
deriving instance Show Macro

deriving instance Eq EBNF
deriving instance Eq EBNFL
deriving instance Eq Head
deriving instance Eq Body
deriving instance Eq Element
deriving instance Eq ElementOpt
deriving instance Eq Variable
deriving instance Eq Terminal
deriving instance Eq Macro