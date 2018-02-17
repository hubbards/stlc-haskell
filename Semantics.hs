-- | Static and dynamic semantics for typed lambda calculus.
--
module Semantics (typeof, eval) where

import Syntax

-- -----------------------------------------------------------------------------
-- Static Semantics

-- | Type checking function for expression; returns type for well-typed
--   expressions, otherwise bottom.
--
--   Note that we use do-notation and take advantage of the fact that 'fail'
--   returns nothing for the 'Maybe' monad.
--
--   TODO: doctests
--
typeof :: Ctx -> Expr -> Maybe Type
typeof ctx (App l r)        = do Fun t1 t2 <- typeof ctx l
                                 t         <- typeof ctx r
                                 if t1 == t then return t2 else Nothing
typeof ctx (Abs v t e)      = fmap (Fun t) $ typeof ((v, t) : ctx) e
typeof ctx (Ref v)          = lookup v ctx
typeof ctx (Pair l r)       = do t1 <- typeof ctx l
                                 t2 <- typeof ctx r
                                 return $ Prod t1 t2
typeof ctx (Fst e)          = do {Prod t _ <- typeof ctx e; return t}
typeof ctx (Snd e)          = do {Prod _ t <- typeof ctx e; return t}
typeof ctx (InL e t)        = do {t' <- typeof ctx e; return $ Sum t' t}
typeof ctx (InR t e)        = do {t' <- typeof ctx e; return $ Sum t t'}
typeof ctx (Case e x l y r) = do Sum t1 t2 <- typeof ctx e
                                 t         <- typeof ((x, t1) : ctx) l
                                 t'        <- typeof ((y, t2) : ctx) r
                                 if t == t' then return t else Nothing

-- -----------------------------------------------------------------------------
-- Dynamic Semantics

-- | Evaluation function (or reduction function) for expressions; returns value
--   (or normal form) of well-typed expressions.
--
--   TODO: doctests
--
eval :: Expr -> Expr
eval t
  | normal t  = t
  | otherwise = eval (step t)

-- | Is this term in normal form?
--
--   TODO: doctests
--
normal :: Expr -> Bool
normal (Abs _ _ _) = True -- call-by-value
normal (Ref _)     = True
normal (Pair l r)  = normal l && normal r
normal (InL e _)   = normal e
normal (InR _ e)   = normal e
normal _           = False -- application, projection, and case

-- | One step of evaluation (or normal order reduction).
--
--   TODO: doctests
--
step :: Expr -> Expr
-- reduction rules
step (App (Abs x _ b) r)      = sub x r b
step (Fst (Pair l _))         = l
step (Snd (Pair _ r))         = r
step (Case (InL e _) x l _ _) = sub x e l
step (Case (InR _ e) _ _ y r) = sub y e r
-- congruence rules
step (App l r)
  | normal l  = App l (step r) -- call-by-value
  | otherwise = App (step l) r
step (Pair l r)
  | normal l  = Pair l (step r)
  | otherwise = Pair (step l) r
step (Fst e)                  = Fst (step e)
step (Snd e)                  = Snd (step e)
step (InL e t)                = InL (step e) t
step (InR t e)                = InR t (step e)
step (Case e x l y r)         = Case (step e) x l y r

-- | Variable substitution.
--
--   TODO: doctests
--
sub :: Var -> Expr -> Expr -> Expr
sub v s (App l r)        = App (sub v s l) (sub v s r)
sub v s (Abs x t b)      = Abs x t (checksub [x] v s b)
sub v s (Ref x)          = if v == x then s else Ref x
sub v s (Pair l r)       = Pair (sub v s l) (sub v s r)
sub v s (Fst e)          = Fst (sub v s e)
sub v s (Snd e)          = Snd (sub v s e)
sub v s (InL e t)        = InL (sub v s e) t
sub v s (InR t e)        = InR t (sub v s e)
sub v s (Case e x l y r) = Case (sub v s e) x (checksub [x] v s l) y (checksub [y] v s r)

-- | Perform variable substitution if variable is fresh for expression.
--
checksub :: [Var] -> Var -> Expr -> Expr -> Expr
checksub vs v s e
  | elem v vs = e
  | otherwise = sub v s e
