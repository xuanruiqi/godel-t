module Main

import Core.Eval
import Core.Expr
import Core.Context
import Parse.Parse
import Parse.Grammar

printEval : Expr -> String
printEval e = if isVal e
              then case hasType e emptyCtxt of
                        Just ty => case toVal e of
                                        Just v => "Result: " ++ show v ++ 
                                                  " |- " ++ show ty
                                        _ => show e ++ " does not evaluate"
                        _ => show e ++ " does not evaluate"
              else case step e of
                        Just e' => "|--> " ++ show e' ++ "\n" ++ 
                                   printEval e'
                        Nothing => show e ++ " failed to step"

prettyPrintError : ParseError -> String
prettyPrintError err 
  = case err of
         ParseFail errStr lineInfo _ => case lineInfo of
                                             Just (line, col) => "Parse error: " ++ errStr ++ " at line " ++ show line ++ ", column" ++ show col
                                             Nothing => "Parse error: " ++ errStr
         LexFail (line, col, _) => "Lexer error at line " ++ show line ++ 
                                   ", column" ++ show col

interpret : String -> IO ()
interpret str = case parseStr str expr of
                     Left e => do putStrLn $ printEval e
                     Right err => do putStrLn $ prettyPrintError err

main : IO ()
main = do putStr "> "
          input <- getLine
          interpret input
          main
