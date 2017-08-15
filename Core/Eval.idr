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

module Core.Eval

import Core.Expr
import Core.Context

||| Bind an expression (e') to a name (x) in another
||| expression (e), and return the resulting expression.
public export
subst : Expr -> String -> Expr -> Expr
subst Zero x e' = Zero
subst (Var x) y e' = if x == y
                     then e'
                     else (Var x)
subst (Succ e) x e' = Succ (subst e x e')
subst (Lambda tau x e) y e' = 
  if x == y
  then subst (subst (Lambda tau x e) (x ++ "'") (Var (x ++ "'"))) x e'
  else (Lambda tau x (subst e y e'))
subst (App e1 e2) x e' = App (subst e1 x e') (subst e2 x e')
subst (Rec e0 x y e1 e) z e' = 
  if x == z
  then subst (Rec e0 (x ++ "'") y (subst e1 x (Var (x ++ "'"))) e) z e'
  else if y == z
       then subst (Rec e0 x (y ++ "'") (subst e1 y (Var (y ++ "'"))) e) z e'
       else Rec (subst e0 z e') x y (subst e1 z e') (subst e z e')


||| Small-step semantics
public export
step : Expr -> Maybe Expr
step Zero = Just Zero
step (Var x) = Just (Var x)
step (Succ e) = case step e of
                Just e' => Just (Succ e')
                Nothing => Nothing
step (Lambda tau x e) = case step e of
                        Just e' => Just (Lambda tau x e')
                        Nothing => Nothing
step (App e1 e2) = case e1 of
                   Lambda tau x e => Just (subst e x e2)
                   otherwise => Nothing
step (Rec e0 x y e1 e) = case e of
                         Zero => Just e0
                         Succ e' => Just (subst (subst e1 x e) y (Rec e0 x y e1 e'))
                         otherwise => case step e of
                                      Just e' => Just (Rec e0 x y e1 e')
                                      Nothing => Nothing

public export
isVal : Expr -> Bool
isVal Zero = True
isVal (Succ e) = if isVal e
                 then True
                 else False
isVal (Lambda _ _ _) = True
isVal _ = False

public export
toVal : Expr -> Maybe Val
toVal Zero = Just (NV Z)
toVal (Succ e) = if isVal e
                 then case e of
                      Zero => Just (NV (S Z))
                      Succ e' => case toVal e' of
                                 Just (NV n) => Just (NV (S (S n)))
                                 otherwise => Nothing
                      otherwise => Nothing
                 else Nothing
toVal (Lambda tau x e) = Just (LamV tau x e)


||| Type checker
public export
hasType : Expr -> Context -> Maybe Ty
hasType Zero _ = Just N
hasType (Succ x) ctxt = case hasType x ctxt of
                             Just ty => Just ty
                             Nothing => Nothing
hasType (Var x) ctxt = lookup x ctxt
hasType (Rec e0 x y e1 e) ctxt = case hasType e ctxt of
                                      Just N => case hasType e0 ctxt of
                                                Just tau => let ctxt' = bind x N (bind y tau ctxt) in
                                                                case hasType e1 ctxt' of
                                                                     Just tau => Just tau
                                                                     otherwise => Nothing
                                                Nothing => Nothing
                                      otherwise => Nothing
hasType (Lambda tau x e) ctxt = let ctxt' = bind x tau ctxt in 
                                    case hasType e ctxt' of
                                         Just tau' => Just (Arrow tau tau')
                                         Nothing => Nothing
hasType (App e1 e2) ctxt = case hasType e1 ctxt of
                                Just (Arrow t t') => case hasType e2 ctxt of
                                                          Just t => Just t'
                                                          Nothing => Nothing
                                otherwise => Nothing

public export
tyVal : Val -> Context -> Maybe Ty
tyVal (NV _) _ = Just N
tyVal (LamV tau x e) ctxt = case hasType e ctxt of
                                 Just t => Just (Arrow tau t)
                                 Nothing => Nothing
