-- | Example programs and derived forms in typed lambda calculus.
--
module Example where

import Syntax
import Semantics

-- | Derived form for let-binding.
--
bind :: Ctx -> Var -> Expr -> Expr -> Maybe Expr
bind ctx x e1 e2 = (\t -> App (Abs x t e2) e1) <$> typeof ctx e1

-- | The identity function.
--
idfun :: Var -> Type -> Expr
idfun v t = Abs v t (Ref v)

-- | Example expressions.
--
expr1, expr2, expr3, expr4 :: Expr
expr1 = idfun "x" (Atom "T")
expr2 = Abs "z" (Atom "T") (App expr1 (Ref "z"))
expr3 = App expr1 expr2
expr4 = App expr1 expr3
