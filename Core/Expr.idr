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

module Core.Expr

public export
data Ty = N | Arrow Ty Ty

public export
data Expr = Zero
          | Succ Expr
          | Var String
          | Rec Expr String String Expr Expr
          | Lambda Ty String Expr
          | App Expr Expr

public export
data Val = NV Nat 
         | LamV Ty String Expr

public export
Show Ty where
     show N = "nat"
     show (Arrow t t') = show t ++ "->" ++  show t'

public export 
Eq Ty where
    N == N = True
    (Arrow t1 t2) == (Arrow t1' t2') = (t1 == t1') && (t2 == t2')
    N == (Arrow _ _) = False
    (Arrow _ _) == N = False

    t /= t' = not (t == t')

public export
Show Expr where
    show Zero = "Z"
    show (Succ e) = case e of
                    Zero => "S Z"
                    Var x => "S " ++ x
                    otherwise => "S (" ++ show e ++ ")"
    show (Var x) = x
    show (Rec e0 x y e1 e) = "rec(" ++ show e0 ++ ", " ++ x ++ " . "  ++
                             y ++ " . " ++ show e1 ++ ", " ++
                             show e ++ ")"
    show (Lambda tau x e) = "\\" ++  x ++ "[" ++ show tau ++ "]" ++  " . " ++
                            show e
    show (App e1 e2) = "(" ++ show e1 ++ ")" ++ show e2

public export
Show Val where
    show (NV n) = show n
    show (LamV tau x e) = show (Lambda tau x e)
