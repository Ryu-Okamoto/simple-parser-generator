# What is this?
This is a parser generator for grammers written in *restricted*-EBNF.   
The input is EBNF file, and the output is parser programs written in Haskell.

Restricted-EBNF is EBNF **winthout** optional part, which more or less than one variables are in.   
For example, in restricted-EBNF, `V ::= V1 [ V2 ]` and `V ::= t1 { V1 t2 } V2` are allowed,   
but `V ::= [ V1 V2 ]` and `V ::= t1 { V1 t2 V2 }` are **NOT**, where `V`and `Vn` are variables and `tn` are terminals.

The definition of restricted-EBNF are described in *restricted*-EBNF as follows:
```ebnf
  EBNF        ::=  EBNFL "." { EBNFL "." }
  EBNFL       ::=  Head "::=" Body { "|" Body }
  Head        ::=  Variable
  Body        ::=  Element { "|" Element }
  Element     ::=  Terminal | Variable | "{" ElementOpt "}" | "[" ElementOpt "]"
  ElementOpt  ::=  Terminal ElementOpt | Variable { Element }
  Variable    ::=  CAPS [ String ]
  Terminal    ::=  "\"" String "\""
  CAPS        ::=  "A" | "B" | ... | "Z"
  String      ::=  ASCII { ASCII }
```  

## Usage
[Stack](https://docs.haskellstack.org/en/stable/) (a Haskell build tool) is required.  

```
$ stack build
$ stack test
```
