module Chapter9.Alpha where

import Chapter9.Syntax

-- Alpha substitute all instances of (TVariable n) with s
alpha ::  String -> Type -> Type -> Type
alpha n s (TVariable name _) | name == n = s
alpha n s (TFunction p r) = TFunction (alpha n s p) (alpha n s r)
alpha n s (TTuple ts) = TTuple (map (alpha n s) ts)
alpha n s (TSum ts) = TSum (map (alpha n s) ts)
alpha n s (TRecord fs) = TRecord (map (\(n, t) -> (n, alpha n s t)) fs)
alpha n s (TRef t) = TRef (alpha n s t)
alpha n s (TLambda ln k t) = TLambda ln k (alpha n s t)
alpha n s (TApply a b) = TApply (alpha n s a) (alpha n s b)
alpha n s (TAll an k c t) | an /= n = TAll an k (alpha n s c) (alpha n s t)
alpha n s (TExists an k c t) | an /= n = TExists an k (alpha n s c) (alpha n s t)
alpha _ _ t = t

