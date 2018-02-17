-- | Abstract syntax for typed lambda calculus.
--
module Syntax (
  Ctx
  , Type(..)
  , Var
  , Expr(..)
  ) where

-- | Typing context (or typing environment).
--
--   TODO: change association list to function type.
--
type Ctx = [(Var, Type)]

-- | Type.
--
data Type = Atom String    -- atomic
          | Fun Type Type  -- function
          | Prod Type Type -- product
          | Sum Type Type  -- sum
  deriving (Eq, Show)

-- | Variable.
--
type Var = String

-- | Expression.
--
data Expr = App Expr Expr               -- application
          | Abs Var Type Expr           -- abstraction
          | Ref Var                     -- reference
          | Pair Expr Expr              -- pair
          | Fst Expr                    -- projection first
          | Snd Expr                    -- projection second
          | InL Expr Type               -- injection left
          | InR Type Expr               -- injection right
          | Case Expr Var Expr Var Expr -- case
  deriving (Eq, Show)
