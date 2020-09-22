module Semantics where

import Syntax

value :: Tm -> Bool
value term =
  case term of Tm_True      -> True
               Tm_False     -> True
               Tm_Abs _ _ _ -> True
               _            -> False


-- [x := s] in t
subst :: String -> Tm -> Tm -> Tm
subst x s t =
  case t of
    -- t is a variable
    Tm_Var y        -> if x == y then s else t
    -- t is a lambda
    Tm_Abs y ty1 t1 -> if x == y then t -- local var shadowing
                       else Tm_Abs y ty1 (subst x s t1)
    Tm_App t1 t2    -> Tm_App (subst x s t1) (subst x s t2)
    Tm_True         -> Tm_True
    Tm_False        -> Tm_False
    Tm_if t1 t2 t3  -> Tm_if (subst x s t1) (subst x s t2) (subst x s t3)

step :: Tm -> Tm
step t = case t of
  Tm_App (Tm_Abs x ty2 t1) t2 -> if (value t2) then (subst x t2 t1) else t
  Tm_App t1 t2 -> if (value t1)
    then (if (value t2) then t else Tm_App t1 (step t2))
    else Tm_App (step t1) t2
  Tm_if Tm_True t1 t2 -> t1
  Tm_if Tm_False t1 t2 -> t2
  Tm_if t1 t2 t3 -> Tm_if (step t1) t2 t3

-- step (Tm_App (Tm_Abs "x" (Ty_Arrow Ty_Bool Ty_Bool) (Tm_Var "x")) (Tm_Abs "x" Ty_Bool (Tm_Var "x")))
