module Types where

import qualified Data.Map as Map

import Syntax
import Semantics


type Context = [(String, Ty)]

has_type :: Context -> Tm -> Ty
has_type env tm =
  case tm of
    Tm_Var x -> (lookup x env)
    Tm_Abs x typ2 t1 -> Map.insert x typ2 env
