module Core.Repl

import Core.Expr
import Core.Eval
import Parse.Parse
import Parse.Grammar

printEval : Expr -> String
printEval e = if isVal e
              then case hasType e of
                   Just t => case toVal e of
                             Just v => "=> " ++ show v ++ " |- " ++ show t
                             Nothing => "Error"
                   Nothing => "Error: " ++ show e ++ "has invalid type"
              else case step e of
                   Just e' => "=> " ++ show e' ++ "\n" ++ printEval e'
                   Nothing => "Error: can't evaluate" ++ show e

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
