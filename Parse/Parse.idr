module Parse.Parse

import public Text.Parser
import public Parse.Lex

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData Token) True ty

public export
EmptyRule : Type -> Type
EmptyRule ty = Grammar (TokenData Token) False ty

public export
data ParseError = ParseFail String (Maybe (Int, Int)) (List Token)
                | LexFail (Int, Int, String)


public export
parseStr : String -> Rule t -> Either t ParseError
parseStr str rule
  = case lex str of
         Right lexError => Right $ LexFail lexError
         Left toks =>
           case parse toks (do res <- rule; eof; pure res) of
                Left (Error err []) => Right $ ParseFail err Nothing []
                Left (Error err (t::ts)) => Right $ ParseFail err (Just (line t, col t)) (map tok (t::ts))
                Right (val, _) => Left val

         
-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
 
