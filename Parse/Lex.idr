---------------------------------------------------------------
||| Copyright 2017 by Xuanrui Qi <me@xuanruiqi.com> 
|||  Licensed under Mozilla Public License. 
||| Available at
||| 
|||    https://www.mozilla.org/en-US/MPL/2.0/ 
|||
||| Covered Software is provided under this License 
||| on an “as is” basis, without warranty of any kind, 
||| either expressed, implied, or statutory, including, 
||| without limitation, warranties that the Covered Software 
||| is free of defects, merchantable, fit for a particular 
||| purpose or non-infringing. No liability is assumed unless 
||| required by law or consented by writing. Refer to 
||| license for details.
|||
|||
|||
||| Based on work by Edwin Brady
||| Copyright (c) 2017 Edwin Brady
|||    School of Computer Science, University of St Andrews
||| All rights reserved.

||| This code is derived from software written by Edwin Brady
||| (ecb10@st-andrews.ac.uk).
---------------------------------------------------------------

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
