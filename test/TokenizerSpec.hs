module TokenizerSpec ( spec ) where

import Test.Hspec ( describe, it, Spec, shouldBe, SpecWith ) 

import Tokenizer ( tokenize, Token (..) )

spec :: Spec
spec = do
  describe "[normal] only variable and basics" $ do
    testNormal "A ::= B."             $ createTokens ["A", "::=", "B", "."]           
    testNormal "A ::= Var."           $ createTokens ["A", "::=", "Var", "."]           
    testNormal "A ::= B C."           $ createTokens ["A", "::=", "B", "C", "."]
    testNormal "A ::= B | C."         $ createTokens ["A", "::=", "B", "|", "C", "."]
    testNormal "A ::= { B }."         $ createTokens ["A", "::=", "{", "B", "}", "."] 
  describe "[normal] with some terminals" $ do
    testNormal "A ::= \"a\"."         $ createTokens ["A", "::=", "\"a\"", "."]           
    testNormal "A ::= \"Hello\"."     $ createTokens ["A", "::=", "\"Hello\"", "."]           
    testNormal "A ::= \"   \"."       $ createTokens ["A", "::=", "\"   \"", "."]           
    testNormal "A ::= B \"c\"."       $ createTokens ["A", "::=", "B", "\"c\"", "."]
    testNormal "A ::= B | \"c\"."     $ createTokens ["A", "::=", "B", "|", "\"c\"", "."]
    testNormal "A ::= \"w\" { B }."   $ createTokens ["A", "::=", "\"w\"", "{", "B", "}", "."] 
  describe "[normal] complex line" $ do
    testNormal "A ::= \"if\" Expr Stmts [ \"else\" Stmts ]"   $ createTokens ["A", "::=", "\"if\"", "Expr", "Stmts", "[", "\"else\"", "Stmts", "]"]
    testNormal "A ::= Var { Var } \":\" \"int\" | \"char\""   $ createTokens ["A", "::=", "Var", "{", "Var", "}", "\":\"", "\"int\"", "|", "\"char\""]
  describe "[normal] multiple lines" $ do
    testNormal "A ::= B.\n B ::= C." 
      [Token "A" 1, Token "::=" 1, Token "B" 1, Token "." 1, 
       Token "B" 2, Token "::=" 2, Token "C" 2, Token "." 2]

  where
    createTokens :: [String] -> [Token]
    createTokens = map (\s -> Token s 1)

testNormal :: [Char] -> [Token] -> SpecWith ()
testNormal ebnf expected = it ebnf $ actual `shouldBe` expected
  where
    actual = case tokenize ebnf of
      (Right tokens) -> tokens
      _              -> []

