{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ParserSpec 
  ( spec ) 
where


import Test.Hspec
  (
      it
    , Spec
    , shouldBe
    , SpecWith
    , context
    , runIO
    , expectationFailure
  )

import Errors 
  ( 
      LexicalError (..)
    , SyntaxError (..)
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
import Tokenizer
  ( tokenize )
import Parser 
  ( parse )


spec :: Spec
spec = do
  context "[normal] simple syntax" $ do
    testNormal "A ::= B." astVariable
    testNormal "A ::= \"t\"." astTerminal
    testNormal "A ::= B C." astProduct
    testNormal "A ::= B | C." astPlus
  context "[normal] optional syntax" $ do
    testNormal "A ::= { B }." astRepeat
    testNormal "A ::= [ B ]." astOption
  context "[normal] combinational syntax" $ do
    testNormal "A ::= B { C }." astBRepeatC
    testNormal "A ::= [ B ] C." astOptionBC
    testNormal "A ::= B { C } D [ E ]." astBRepeatCDOptionE
    testNormal "A ::= { B } | \"t\"." astRepeatBort
    testNormal "A ::= [ B ] | C." astOptionBorC
    testNormal "A ::= { \"t\" B \"t\" \"t\" }." astRepeattBtt
    testNormal "A ::= [ \"t\" \"t\" B \"t\" ]." astOptionttBt
  context "[normal] multiple ebnf lines" $ do
    testNormal "A ::= B. B ::= \"t\"."  astABBt

    where
      astVariable = EBNF (EBNFL headA bodyB []) []
      astTerminal = EBNF (EBNFL headA bodyt []) []
      astProduct = EBNF (EBNFL headA bodyBC []) []
      astPlus = EBNF (EBNFL headA bodyB [bodyC]) []
      astRepeat = EBNF (EBNFL headA (Body repeatB []) []) []
      astOption = EBNF (EBNFL headA (Body optionB []) []) []
      astBRepeatC = EBNF (EBNFL headA bodyBRepeatC []) []
      astOptionBC = EBNF (EBNFL headA bodyOptionBC []) []
      astBRepeatCDOptionE = EBNF (EBNFL headA bodyBRepeatCDOptionE []) []
      astRepeatBort = EBNF (EBNFL headA (Body repeatB []) [bodyt]) []
      astOptionBorC = EBNF (EBNFL headA (Body optionB []) [bodyC]) []
      astRepeattBtt = EBNF (EBNFL headA bodyRepeattBtt []) []
      astOptionttBt = EBNF (EBNFL headA bodyOptionttBt []) []
      astABBt = EBNF (EBNFL headA bodyB []) [EBNFL headB bodyt []]

      headA = Head (Variable "A")
      headB = Head variableB

      bodyBRepeatCDOptionE =  Body (AtomV variableB) [Repeat (PostV (Variable "C") []), AtomV (Variable "D"), Option (PostV (Variable "E") [])]
      bodyBRepeatC = Body (AtomV variableB) [Repeat (PostV (Variable "C") [])]
      bodyOptionBC = Body (Option (PostV variableB [])) [AtomV (Variable "C")]
      bodyRepeattBtt = Body (Repeat (PrevV terminalt (PostV variableB [terminalt, terminalt]))) []
      bodyOptionttBt = Body (Option (PrevV terminalt (PrevV terminalt (PostV variableB [terminalt])))) []
      bodyB = Body (AtomV variableB) []
      bodyC = Body (AtomV (Variable "C")) []
      bodyBC = Body (AtomV variableB) [(AtomV (Variable "C"))]
      bodyt = Body (AtomT terminalt) []

      repeatB = Repeat (PostV variableB [])
      optionB = Option (PostV variableB [])

      variableB = Variable "B"
      terminalt = Terminal "\"t\""
      

testNormal :: String -> EBNF -> SpecWith()
testNormal ebnf expected = it ebnf $
  case tokenizeResult of 
    (Left _) -> expectationFailure $ "failed to tokenize " ++ ebnf
    (Right tokens) -> 
      case parseResult of
        (Left _) -> expectationFailure $ "failed to parse " ++ ebnf
        (Right actual) -> actual `shouldBe` expected
      where
        parseResult = parse tokens
  where
    tokenizeResult = tokenize ebnf