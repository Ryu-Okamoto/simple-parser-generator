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
  ( SyntaxError (..) )
import Tokenizer 
  ( Token (..) )
import RestrictedEBNF
  ( 
      Macro (..)
    , Terminal (..)
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
    testNormal "A ::= B." astB
    testNormal "A ::= \"t\"." astt
    testNormal "A ::= B C." astBC
    testNormal "A ::= B | C." astBorC
  context "[normal] optional syntax" $ do
    testNormal "A ::= { B }." astRepeatB
    testNormal "A ::= [ B ]." astOptionB
    testNormal "A ::= [ \"t\" ]." astOptiont
  context "[normal] combinational syntax" $ do
    testNormal "A ::= B { C }." astBRepeatC
    testNormal "A ::= [ B ] C." astOptionBC
    testNormal "A ::= B { C } D [ E ]." astBRepeatCDOptionE
    testNormal "A ::= [ B ] | C." astOptionBorC
    testNormal "A ::= { \"t\" B \"t\" }." astRepeattBt
    testNormal "A ::= [ \"t\" \"t\" B ]." astOptionttB
    testNormal "A ::= [ B \"t\" \"t\" ]." astOptionBtt
  context "[normal] multiple ebnf lines" $ do
    testNormal "A ::= B. B ::= C."  astABBC
  context "[normal] with macros" $ do
    testNormal "A ::= @ALPHA." astALPHA
    testNormal "A ::= @UPPERCASE { @ALPHA }." astCamelCase
  context "[normal] apply to example files" $ do
    testNormalWithFile "test/example/expression/token.tsv"
    testNormalWithFile "test/example/expression/token.tsv"
  
  context "[abnormal] simple syntax error" $ do
    testAbnormal "A ::= B"  "no end dot" 
    testAbnormal "A ::= [ B ."  "no end bracket"
    testAbnormal "A ::= B ] ."  "no begin bracket"
    testAbnormal "A B ::= ] C."  "stupid sequence"
  context "[abnormal] in violation of the restrictions" $ do
    testAbnormal "A ::= [ B C ]."  "two variables in an optional part"
    testAbnormal "A ::= { B C }."  "two variables in an repeated part"
    testAbnormal "A ::= [ \"t\" B \"t\" C ]."  "two variables in an optional part with terminals"

  -- AST for testing
  where
    astB = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B")) []
          ) []
        ) []
    astt = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body
            (AtomT (Terminal "\"t\"")) []
          ) []
        ) []
    astBC = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B")) 
            [AtomV (Variable "C")]
          ) []
        ) [] 
    astBorC =
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B")) []
          ) [
            Body
              (AtomV (Variable "C")) []
          ]
        ) []  
    astRepeatB = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Repeat 
              (PostV (Variable "B") [])
            ) []
          ) []
        ) []
    astOptionB = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PostV (Variable "B") [])
            ) []
          ) []
        ) []
    astOptiont =
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PrevV (Terminal "\"t\"") Nothing)
            ) []
          ) []
        ) [] 
    astBRepeatC = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B")) 
            [Repeat
              (PostV (Variable "C") [])
            ]
          ) []
        ) []
    astOptionBC = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PostV (Variable "B") [])
            ) 
            [AtomV (Variable "C")]
          ) []
        ) [] 
    astBRepeatCDOptionE = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B"))
            [   
                Repeat
                  (PostV (Variable "C") [])
              , AtomV (Variable "D")
              , Option
                  (PostV (Variable "E") [])
            ]
          ) []
        ) [] 
    astOptionBorC = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PostV (Variable "B") [])
            ) []
          ) [
            Body
              (AtomV (Variable "C")) []
          ]
        ) [] 
    astRepeattBt = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Repeat 
              (PrevV (Terminal "\"t\"") (Just
              (PostV (Variable "B")
                [Terminal "\"t\""]
              )))
            ) []
          ) []
        ) []
    astOptionttB = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PrevV (Terminal "\"t\"") (Just
              (PrevV (Terminal "\"t\"") (Just
              (PostV (Variable "B") [])
              ))))
            ) []
          ) []
        ) []
    astOptionBtt = 
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (Option 
              (PostV (Variable "B")
                [  
                    Terminal "\"t\""
                  , Terminal "\"t\""
                ]
              )
            ) []
          ) []
        ) []
    astABBC =
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body 
            (AtomV (Variable "B")) []
          ) []
        ) [
          EBNFL
            (Head (Variable "B"))
            (Body 
              (AtomV (Variable "C")) []
            ) []
        ] 
    astALPHA =
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body
            (AtomT (Macro ALPHA)) []
          ) []
        ) []
    astCamelCase =
      EBNF
        (EBNFL
          (Head (Variable "A"))
          (Body
            (AtomT (Macro UPPERCASE)) 
            [Repeat
              (PrevV (Macro ALPHA) Nothing)
            ]
          ) []
        ) []
      

testNormal :: String -> EBNF -> SpecWith ()
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


testNormalWithFile :: FilePath -> SpecWith ()
testNormalWithFile tokensFile = do
  tokens <- runIO $ createTokensFromFile tokensFile
  it tokensFile $ 
    case parse tokens of
      (Left _) -> expectationFailure $ "failed to parse " ++ tokensFile
      _ -> return ()
  where
    createTokensFromFile :: FilePath -> IO [Token]
    createTokensFromFile filePath = do
      tokens <- map words . lines <$> readFile filePath
      return $ map (\(t:l:_) -> Token t (read l)) tokens


testAbnormal :: String -> String -> SpecWith ()
testAbnormal ebnf description = it outputText $
  case tokenizeResult of 
    (Left _) -> expectationFailure $ "failed to tokenize " ++ ebnf
    (Right tokens) -> 
      case parseResult of
        (Left (SyntaxError actual)) -> actual `shouldBe` 1
        (Right _) -> expectationFailure $ "failed to capture a syntax error " ++ ebnf
      where
        parseResult = parse tokens
  where
    outputText = ebnf ++ "  // " ++ description
    tokenizeResult = tokenize ebnf 
