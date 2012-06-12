module Smart where

import Prelude hiding (pi)
import Location
import Syntax

-- * Smart constructors

-- | Smart constructor for lambda abstractions.
lam :: [(Location, Name)] -> Term -> Term
lam []            t = t
lam ((l, x) : xs) t = Lam l (x, lam xs t)

-- | Smart constructor for split.
split :: Location -> Term -> (Name, Name) -> Term -> Term
split l t1 (x, y) t2 = Split l t1 (x, (y, t2))

-- | Smart constructor for quantifiers.
q :: PiSigma -> [(Location, Name, Type)] -> Type -> Type
q _  []                b = b
q ps ((l, x, a) : xas) b = Q l ps (a, (x, q ps xas b))

-- | Smart constructor for multiple Pi applications.
pis' :: [(Location, Name, Type)] -> Type -> Type
pis'     = q Pi

-- | Smart constructor for multiple Pi applications.
pis :: [(Location, Name)] -> Type -> Type -> Type
pis ns t = pis' (map (\ (l, n) -> (l, n, t)) ns)

-- | Smart constructor for Pi.
pi :: Location -> Name -> Type -> Type -> Type
pi l n t = pis' [(l, n, t)]

-- | Smart constructor for multiple Sigma applications.
sigmas' :: [(Location,Name,Type)] -> Type -> Type
sigmas' = q Sigma

-- | Smart constructor for multiple Sigma applications.
sigmas :: [(Location,Name)] -> Type -> Type -> Type
sigmas ns t = sigmas' (map (\ (l, n) -> (l, n, t)) ns)

-- | Smart constructor for Sigma.
sigma :: Location -> Name -> Type -> Type -> Type
sigma l n t = sigmas' [(l, n, t)]

-- | Smart constructor for function space.
(->-) :: Type -> Type -> Type
(->-) t = pi (getLoc t) "" t

-- | Smart constructor for product.
(-*-) :: Type -> Type -> Type
(-*-) t = sigma (getLoc t) "" t