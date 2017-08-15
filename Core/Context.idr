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
---------------------------------------------------------------

module Context

import Core.Expr
import Data.SortedMap

public export
Context : Type
Context = SortedMap String Ty

public export
emptyCtxt : Context
emptyCtxt = empty

public export
lookup : String -> Context -> Maybe Ty
lookup = Data.SortedMap.lookup

public export
bind : String -> Ty  -> Context -> Context
bind = insert
