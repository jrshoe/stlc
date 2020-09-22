module Syntax where

data Ty
  = Ty_Bool
  | Ty_Arrow Ty Ty
  deriving (Eq, Read, Show)

data Tm
  = Tm_Var String
  | Tm_App Tm Tm
  | Tm_Abs String Ty Tm
  | Tm_True
  | Tm_False
  | Tm_if Tm Tm Tm
  deriving (Show, Eq)

