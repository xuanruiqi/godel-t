module Parse.Grammar

import public Parse.Parse
import public Parse.Lex
import public Core.Expr

symbol : String -> Rule ()
symbol req
    = terminal (\x => case tok x of
                           Symbol s => if s == req then Just ()
                                                   else Nothing
                           _ => Nothing)

keyword : String -> Rule ()
keyword req
    = terminal (\x => case tok x of
                           Keyword s => if s == req then Just ()
                                                    else Nothing
                           _ => Nothing)

var : Rule String
var = terminal (\x => case tok x of
                           Var str => Just str
                           _ => Nothing)
mutual
  simpleType : Rule Ty
  simpleType = do symbol "("; commit
                  t <- type
                  symbol ")"
                  pure t
           <|> do keyword "nat"; pure N


  type : Rule Ty
  type = do t1 <- simpleType; symbol "->"; commit
            t2 <- type
            pure $ Arrow t1 t2
     <|> simpleType


mutual
  export
  expr : Rule Expr
  expr = do e1 <- simpleExpr; commit; e2 <- simpleExpr
            pure (App e1 e2)    
     <|> simpleExpr


  simpleExpr : Rule Expr
  simpleExpr = do keyword "Z"
                  commit
                  pure Zero
           <|> do keyword "z"
                  commit
                  pure Zero
           <|> do keyword "S"
                  e <- expr
                  commit
                  pure (Succ e)
           <|> do keyword "s"
                  e <- expr
                  commit
                  pure (Succ e)
           <|> do symbol "("; e <- expr; symbol ")"
                  commit
                  pure e
           <|> do x <- var
                  commit
                  pure (Var x)
           <|> do keyword "rec"; symbol "("; e0 <- expr; symbol ","
                  x <- var; symbol "."; y <- var; symbol "."
                  e1 <- expr; symbol ","; e <- expr; symbol ")"
                  commit
                  pure (Rec e0 x y e1 e)  
           <|> do symbol "\\"; x <- var; symbol "["
                  tau <- type; symbol "]"; symbol "."
                  e <- expr
                  commit
                  pure (Lambda tau x e)
         

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
