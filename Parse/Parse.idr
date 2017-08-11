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
 
