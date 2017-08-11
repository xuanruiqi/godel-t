module Parse.Lex

import public Text.Lexer

public export
data Token = Var String
           | Keyword String
           | Symbol String
           | Ignore String

export
Show Token where
    show (Var x) = "Var" ++ x
    show (Keyword s) = s
    show (Symbol s) = s
    show (Ignore _) = ""

comment : Lexer
comment = is ';' <+> is ';' <+> some (isNot '\n') <+> is '\n'

keywords : List String
keywords = ["z", "s", "Z", "S", "rec", "nat", "N"]

symbols : List String
symbols = ["(", ")", "[", "]", ",", ".", "\\", "->"]

var : Lexer
var = pred idBegin <+> many (pred idChar)
  where
    idBegin : Char -> Bool
    idBegin '_' = True
    idBegin x = isAlpha x
    
    idChar : Char -> Bool
    idChar '\'' = True
    idChar '_' = True
    idChar x = isAlphaNum x

validSymbol : Lexer
validSymbol = some (oneOf ",.\\()[]->")


rawTokens : TokenMap Token
rawTokens = 
  map (\x => (exact x, Keyword)) keywords ++
  map (\x => (exact x, Symbol)) symbols ++
  [(var, Var), 
   (comment, Ignore),
   (validSymbol, Symbol),
   (space, Ignore)]
   
export
lex : String -> Either (List (TokenData Token)) (Int, Int, String)
lex str
  = case Lexer.lex rawTokens str of
         (tok, (_, _, "")) => Left (filter notIgnore tok)
         (_, fail) => Right fail
  where
    notIgnore : TokenData Token -> Bool
    notIgnore token = case tok token of
                            Ignore _ => False
                            _ => True
    
-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
