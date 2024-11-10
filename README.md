[![Haskell CI](https://github.com/Ryu-Okamoto/simple-parser-generator/actions/workflows/haskell.yml/badge.svg)](https://github.com/Ryu-Okamoto/simple-parser-generator/actions/workflows/haskell.yml)

# What is this?
This is a parser generator for grammers written in *restricted*-EBNF. 
The input is an EBNF file, and the outputs are lexer and parser programs written in Haskell.

Restricted-EBNF is EBNF **winthout** optional part (`{ }` or `[ ]`), which more or less than one variables are in. 
For example, in restricted-EBNF, `V ::= V1 [ V2 ]` and `V ::= t1 { V1 t2 } V2` are allowed, 
but `V ::= [ V1 V2 ]` and `V ::= t1 { V1 t2 V2 }` are **NOT**, where `V`and `Vn` are variables and `tn` are terminals.

The definition of restricted-EBNF are described in *restricted*-EBNF as follows:
```
  EBNF       ::=  EBNFL "." { EBNFL "." }.
  EBNFL      ::=  Head "::=" Body { "|" Body }.
  Head       ::=  Variable.
  Body       ::=  Element { Element }.
  Element    ::=  Terminal | Variable | "{" ElementOpt "}" | "[" ElementOpt "]".
  ElementOpt ::=  Terminal [ ElementOpt ] | Variable { Element }.
  Variable   ::=  @UPPERCASE [ ALPHANUMS ].
  Terminal   ::=  "\"" STRING "\"".
  ALPHANUMS  ::=  @ALPHANUM { @ALPHANUM }.
  STRING     ::=  @PRINTABLE { @PRINTABLE }.
```
Variables stating with `@` are treated as macros.
Macros are symbols used to only represent the set of characters available lexically 
and does **NOT** correspond to nodes in the abstract syntax tree (AST).

All defined macros and their expansions are as follows:
```
  @ALPHANUM  ::=  @ALPHA | @NUMBER.
  @ALPHA     ::=  @UPPERCASE | @LOWERCASE.
  @UPPERCASE ::=  "A" | "B" | ... | "Z".
  @LOWERCASE ::=  "a" | "b" | ... | "z".
  @NUMBER    ::=  "0" | "1" | ... | "9".
  @PRINTABLE ::=
     // a printable character including spaces (\n, \t or ' '), 
     // but double quotation MUST be written as "\""
     // and back slash as "\\"
```

## Usage
[Stack](https://docs.haskellstack.org/en/stable/) (a Haskell build tool) is required.  

```
$ stack build
$ stack test            // build and run tests
$ stack run  -- <args>  // build and run app with args
$ stack install         // build and locate app in local bin
```
