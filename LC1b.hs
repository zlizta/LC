module LC1b where

-- Exercise
-- Implementation of Simply Typed Lambda Calculus with finite enumeration types.
-- 4th June 2012
-- Authors: ANBERREE Thomas, MIANLAI Zhou, NUO Li
-- Done at UNNC, SEB438 during our "Reading group meeting".
-- 
-- NEW in this version : 
-- (Thomas) I used Data.Set and Data.Map to represent enumeration types as sets and mappings from labels to terms in case construct as maps.
-- (Thomas) Bug in function check corrected.


import Data.Maybe (fromJust)
import Data.Set (Set, union, delete, insert, unions,singleton,empty,member,fromList)
import qualified Data.Set as Set
import Data.Map (Map,keysSet)
import qualified Data.Map as Map

-- Utilities

elemsAll :: (a -> Bool) -> Map k a -> Bool
elemsAll p = Map.fold ((&&) . p) True 
-- 
type Name = String

type Label = String

data Term =
      Var Name
    | Lam Name Term
    | App Term Term
    | Lab Label
    | Case Term (Map Label Term)
    deriving (Eq, Ord)
   
data Type =
       Enum (Set Label) 
     | Fun Type Type
     deriving Eq
     
type Context = [(Name, Type)]

infer :: Context -> Term -> Type
infer g (Var x)       = fromJust (lookup x g)  -- lookup stops at the first occurrence of x while scanning the list from its head towards its tail
infer g (App s t)     = case infer g s of
                         Fun a b -> if check g t a then b else error "App : wrong argument type"
                         _       -> error "App : not a function type"
infer g (Lam x t)     = error "Lam : cannot infer type"
infer g (Lab l)       = error "Lab : cannot infer type"
infer g (Case t lus)  = error "Case : cannot infer type"


check :: Context -> Term -> Type -> Bool
check g (Var x) a           = infer g (Var x) == a
check g (App s t) a         = infer g (App s t) == a 
check g (Lam x t) (Fun a b) = check ((x,a):g) t b                    
check g (Lab l) (Enum ls)   = member l ls
check g (Case t lus) a      = check g t (Enum (keysSet lus)) 
                              && elemsAll (flip (check g) a) lus 


-- tests
x  = "x"
x' = Var x


id'       = Lam x x'
empty'    = Enum empty
unit      = Enum (singleton "top")
star      = Lab "*"
bool      = Enum (fromList ["true","false"])

a1        = Fun unit unit

test :: Int -> Bool
test 0     = check [] (Lam x id') (Fun unit (Fun bool unit))
-- Expected False

test 1     = check [] id' a1
-- Expected True

test 2     = check [] (App id' star) unit 
-- Expected error

-- Big step evaluation     
eval :: Term -> Term
eval (App t u)    = let t' = eval t in case t' of  Lam x r -> eval (subst r u x)                  
eval (Case t lus) = let t' = eval t in case t' of  Lab l   -> fromJust (Map.lookup l lus)
eval (Lab l)      = Lab l
eval (Lam x t)    = Lam x t
eval (Var x)      = Var x

-- Substitution
-- subst t u x = t[u/x]     
subst :: Term -> Term -> Name -> Term
subst (Var y) u x | x == y       = u
                  | otherwise    = Var y   
subst (App s t) u x              = App (subst s u x) (subst t u x) 
subst (Lam y t) u x | y == x     = Lam y t
                    | otherwise  = Lam y' (subst (rename t y' y) u x) 
                                       where 
                                       (fu,bu) = variables u
                                       (ft,bt) = variables t
                                       -- If y appears freely in u,we avoid capturing free occurrences of y in u by renaming y to y' in t
                                       -- where y' is the name y followed by a number such that y' does not appear free in t or u.
                                       -- Just to make it easier to read resulting terms, we also require that y' does not appear bounded in u or t,
                                       -- but this is not necessary. 
                                       y' | member y fu = head [ x | x <- [y++show i | i <- [0..]] , not (member x (unions [fu,bu,ft,bt]))]
                                          | otherwise   = y
subst (Case t lvs) u x           = Case (subst t u x) (Map.map (\ v -> subst v u x) lvs)                                 
subst (Lab l) u x                = Lab l  

-- variables t = (Set of names with at least one free  occurrence in t, Set of names with at least one bound occurrence in t)
variables :: Term -> (Set Name,Set Name)
variables (Var x)      = (singleton x,empty)
variables (App s t)    = (union fs ft, union bs bt) 
                          where 
                           (fs,bs) = variables s
                           (ft,bt) = variables t
variables (Lam x t)    = (delete x ft, insert x bt) where (ft,bt) = variables t  
variables (Case t lus) = (union  ft fus , union bt bus)
                          where 
                           (ft,bt)   = variables t
                           (fus,bus) =  Map.fold (\ (fs,bs) (ft,bt) -> (union fs ft, union bs bt)) (empty,empty) (Map.map variables lus)
variables (Lab l)      = (empty,empty)


 
              
-- Assumption on rename t z y: z does not occur at all in term t (neither freely, nor bounded)
rename :: Term -> Name -> Name -> Term 
rename (Var x) z y | y == x       = Var z
                   | otherwise    = Var x   
rename (App s t) z y              = App (rename s z y) (rename t z y) 
rename (Lam x t) z y | x == y     = Lam x t
                     | otherwise  = Lam x (rename t z y) 
rename (Case t lvs) z y           = Case (rename t z y) (Map.map (\v -> rename v z y) lvs)                                  
rename (Lab l) z y                = Lab l