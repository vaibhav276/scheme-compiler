{-# LANGUAGE RecordWildCards #-}
import Parser (evalStringImpl)

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "compiler" $ do
          describe "parser"      			$ for_ parserCases      $ test evalStringImpl shouldBe
          describe "parser - negative"      $ for_ parserCasesNeg   $ test evalStringImpl shouldNotBe
          describe "evaluator"              $ for_ evaluatorCases   $ test evalStringImpl shouldBe
  where
    test f g Case{..} = it description $ f input `g` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }   

parserCases :: [Case]
parserCases = 
	[ Case { description = "simple list"
           , input       = "'(1 2 2)"
           , expected    = "(1 2 2)"
           }   
    , Case { description = "simple list 1"
           , input       = "'(1 3 (\"this\" \"one\"))"
           , expected    = "(1 3 (\"this\" \"one\"))"
           }
    , Case { description = "two strings"
           , input       = "a test"
           , expected    = "a"
           }
    , Case { description = "simple list"
           , input       = "'(a test)"
           , expected    = "(a test)"
           }
    , Case { description = "nested list"
           , input       = "'(a (nested) test)"
           , expected    = "(a (nested) test)"
           }
    , Case { description = "nested quoted dotted list"
           , input       = "'(a '(quoted (dotted . list)) test)"
           , expected    = "(a (quote (quoted (dotted.list))) test)"
           }
    ]

parserCasesNeg :: [Case]
parserCasesNeg = 
    [ Case { description = "unbalanced parens"
            , input      = "'(a '(imbalanced parens)"
            , expected   = "(a (quote (imbalance parens))"
           }
    ]


evaluatorCases :: [Case]
evaluatorCases =
    [ Case { description = "add"
            , input      = "(+ 2 3)"
            , expected   = "5"
           }
    , Case { description = "add and subtract"
           , input       = "(+ 2 (- 4 1))"
           , expected    = "5"
           }
    , Case { description = "add and subtract 1"
           , input       = "(- (+ 4 6 3) 3 5 2)"
           , expected    = "3"
           }
    , Case { description = "LT"
           , input       = "(< 2 3)"
           , expected    = "#t"
           }
    , Case { description = "GT"
           , input       = "(> 2 3)"
           , expected    = "#f"
           }
    , Case { description = "GE"
           , input       = "(>= 3 3)"
           , expected    = "#t"
           }
    , Case { description = "string equals"
           , input       = "(string=? \"test\" \"test\")"
           , expected    = "#t"
           }
    , Case { description = "string not equals"
           , input       = "(string=? \"abc\" \"test\")"
           , expected    = "#f"
           }
    , Case { description = "string LE"
           , input       = "(string<=? \"abc\" \"test\")"
           , expected    = "#t"
           }
    , Case { description = "if"
           , input       = "(if (< 2 3) \"yes\" \"no\")"
           , expected    = "\"yes\""
           }
    , Case { description = "car"
           , input       = "(car '(2 3 4))"
           , expected    = "2"
           }
    , Case { description = "cdr"
           , input       = "(cdr '(2 3 4))"
           , expected    = "(3 4)"
           }
    , Case { description = "cons"
           , input       = "(cons 5 '(2 3 4))"
           , expected    = "(5 2 3 4)"
           }
    , Case { description = "eqv?"
           , input       = "(eqv? '(5) '(2 3 4))"
           , expected    = "#f"
           }
    , Case { description = "eqv?"
           , input       = "(eqv? '(2 3 4) '(2 3 4))"
           , expected    = "#t"
           }
    , Case { description = "equal?"
           , input       = "(equal? \"2\" 2)"
           , expected    = "#t"
           }
    , Case { description = "equal?"
           , input       = "(equal? \"2\" 3)"
           , expected    = "#f"
           }
    ]

