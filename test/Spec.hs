import Parser (readExpr, eval)

main :: IO ()
main = do
    putStrLn $ show . readExpr $ "(1 2 2)"
    putStrLn $ show . readExpr $ "'(1 3 (\"this\" \"one\"))"
    putStrLn $ show . readExpr $ "a test"
    putStrLn $ show . readExpr $ "(a test)"
    putStrLn $ show . readExpr $ "(a (nested) test)"
    putStrLn $ show . readExpr $ "(a '(quoted (dotted . list)) test)"
    putStrLn $ show . readExpr $ "(a '(imbalanced parens)" -- must fail
    putStrLn $ show . eval . readExpr $ "(+ 2 2)"
    putStrLn $ show . eval . readExpr $ "(+ 2 (-4 1))"
    putStrLn $ show . eval . readExpr $ "(+ 2 (- 4 1))"
    putStrLn $ show . eval . readExpr $ "(- (+ 4 6 3) 3 5 2)"
