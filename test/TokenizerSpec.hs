{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TokenizerSpec 
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
  ( LexicalError (..) )
import Tokenizer 
  ( 
      tokenize
    , Token (..)
  )


spec :: Spec
spec = do
  context "[normal] only variables" $ do
    testNormal "A ::= Var." $ createTokensFromList ["A", "::=", "Var", "."]
    testNormal "A ::= B C." $ createTokensFromList ["A", "::=", "B", "C", "."]
    testNormal "A ::= B | C." $ createTokensFromList ["A", "::=", "B", "|", "C", "."]
    testNormal "A ::= { B }." $ createTokensFromList ["A", "::=", "{", "B", "}", "."]

  context "[normal] basic combination" $ do
    testNormal "A ::= \"a\"." $ createTokensFromList ["A", "::=", "\"a\"", "."]
    testNormal "A ::= \"He llo\"." $ createTokensFromList ["A", "::=", "\"He llo\"", "."]
    testNormal "A ::= B \"c\"." $ createTokensFromList ["A", "::=", "B", "\"c\"", "."]
    testNormal "A ::= B|\"c\"." $ createTokensFromList ["A", "::=", "B", "|", "\"c\"", "."]
    testNormal "A ::= \"w\"{B}." $ createTokensFromList ["A", "::=", "\"w\"", "{", "B", "}", "."]

  context "[normal] terminals containing escape sequences" $ do
    testNormal "A ::= \"\\\"\"." $ createTokensFromList ["A", "::=", "\"\\\"\"", "."]
    testNormal "A ::= \"\\\\\"." $ createTokensFromList ["A", "::=", "\"\\\\\"", "."]
    testNormal "A ::= \"\\n\"." $ createTokensFromList ["A", "::=", "\"\\n\"", "."]
    testNormal "A ::= \"\\t\"." $ createTokensFromList ["A", "::=", "\"\\t\"", "."]
    testNormal "A ::= \"\\\"hi\\\"\"." $ createTokensFromList ["A", "::=", "\"\\\"hi\\\"\"", "."]

  context "[normal] macros" $ do
    testNormal "A ::= @UPPERCASE." $ createTokensFromList ["A", "::=", "@UPPERCASE", "."]
    testNormal "A ::= @ALPHANUM." $ createTokensFromList ["A", "::=", "@ALPHANUM", "."]
    testNormal "A ::= @ALPHA." $ createTokensFromList ["A", "::=", "@ALPHA", "."]
    testNormal "A ::= @NUMBER." $ createTokensFromList ["A", "::=", "@NUMBER", "."]
    testNormal "A ::= @PRINTABLE." $ createTokensFromList ["A", "::=", "@PRINTABLE", "."]
    testNormal "A ::= @ALPHA | @ALPHA { A }." $ createTokensFromList ["A", "::=", "@ALPHA", "|", "@ALPHA", "{", "A", "}", "."]
    
  context "[normal] apply to example files" $ do
    testNormalWithFile "test/example/expression/definition.ebnf" "test/example/expression/token.tsv"
    testNormalWithFile "test/example/itself/definition.ebnf" "test/example/itself/token.tsv"

  context "[abnormal] some errors in variables and terminals" $ do
    testAbnormal "A ::= WithUnderScore__"
    testAbnormal "A ::= \"not closed string"
    testAbnormal "A ::= \"\" in terminal without escape\""
  
  context "[abnormal] use undefined macros" $ do
    testAbnormal "A ::= @Number"
    testAbnormal "A ::= @ORIGINAL"

  where
    createTokensFromList :: [String] -> [Token]
    createTokensFromList = map (\s -> Token s 1)


testNormal :: String -> [Token] -> SpecWith ()
testNormal ebnf expected = it ebnf $
  case tokenize ebnf of
    (Left _) -> expectationFailure $ "failed to tokenize " ++ ebnf
    (Right actual) -> actual `shouldBe` expected


testNormalWithFile :: FilePath -> FilePath -> SpecWith ()
testNormalWithFile ebnfFile tokensFile = do
  ebnf <- runIO $ readFile ebnfFile
  expected <- runIO $ createTokensFromFile tokensFile
  let actual = case tokenize ebnf of
        (Right tokens) -> tokens
        _ -> []
  it ebnfFile $ actual `shouldBe` expected
  where
    createTokensFromFile :: FilePath -> IO [Token]
    createTokensFromFile filePath = do
      tokens <- map words . lines <$> readFile filePath
      return $ map (\(t:l:_) -> Token t (read l)) tokens


testAbnormal :: String -> SpecWith ()
testAbnormal ebnf = it ebnf $
  case tokenize ebnf of
    (Left (LexicalError lineNumber)) -> lineNumber `shouldBe` 1
    _ -> expectationFailure $ "failed to identify lexical error " ++ ebnf