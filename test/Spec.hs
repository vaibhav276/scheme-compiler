import Parser
import Control.Monad (liftM)

main :: IO ()
main = do
    putStrLn $ parse "(1 2 2)"
    putStrLn $ parse "'(1 3 (\"this\" \"one\"))"
    putStrLn $ parse "a test"
    putStrLn $ parse "(a test)"
    putStrLn $ parse "(a (nested) test)"
    putStrLn $ parse "(a '(quoted (dotted . list)) test)"
    putStrLn $ parse "(a '(imbalanced parens)" -- must fail
    putStrLn $ parseAndEval "(+ 2 2)"
    putStrLn $ parseAndEval "(+ 2 (-4 1))" -- todo: why this is failing?
    putStrLn $ parseAndEval "(+ 2 (- 4 1))"
    putStrLn $ parseAndEval "(- (+ 4 6 3) 3 5 2)"
    putStrLn $ parseAndEval "(- (+ 4 6 3) 3 g 2)" -- must fail
    putStrLn $ parseAndEval "(< 2 3)"
    putStrLn $ parseAndEval "(> 2 3)"
    putStrLn $ parseAndEval "(>= 3 3)"
    putStrLn $ parseAndEval "(string=? \"test\" \"test\")"
    putStrLn $ parseAndEval "(string=? \"abc\" \"test\")"
    putStrLn $ parseAndEval "(string<=? \"abc\" \"test\")"
    putStrLn $ parseAndEval "(if (< 2 3) \"yes\" \"no\")"
    putStrLn $ parseAndEval "(car '(2 3 4))"
    putStrLn $ parseAndEval "(cdr '(2 3 4))"
    putStrLn $ parseAndEval "(cons 5 '(2 3 4))"
    putStrLn $ parseAndEval "(eqv '(5) '(2 3 4))"
    putStrLn $ parseAndEval "(eqv '(2 3 4) '(2 3 4))"

parse :: String -> String
parse expr = do
    evaled <- return $ liftM show $ readExpr expr
    extractValue $ trapError evaled

parseAndEval :: String -> String
parseAndEval expr = do
    evaled <- return $ liftM show $ readExpr expr >>= eval
    extractValue $ trapError evaled
