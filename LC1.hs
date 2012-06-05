module LC1 where
-- Exercise
-- Implementation of Simply Typed Lambda Calculus with finite enumeration types.
-- 4th June 2012
-- Authors: ANBERREE Thomas, MIANLAI Zhou, NUO Li
-- Done at UNNC, SEB438 during our "Reading group meeting".
-- 
-- This version is the one we came up with together with only slight modifications
-- and comments by me (Thomas). 
--
-- I HAVE NOT CORRECTED A MISTAKE in the function check (see below).
--
-- Exercise: correct the bug.
--
-- I will also provide a new version where Enum types and Label/Term associations in the case constructs are implemented as Set and Map respectively.
 

import Data.Maybe (fromJust)


type Name = String

type Label = String

data Term =
      Var Name
    | App Term Term
    | Lam Name Term
    | Lab Label
    | Case Term [(Label,Term)]
   
data Type =
       Enum [Label]
     | Fun Type Type
        deriving Eq
     -- two enumerations form the same type 
     -- iff they are the same lists (this is not what we want really)
     
type Context = [(Name, Type)]

infer :: Context -> Term -> Type
infer g (Var x) = fromJust (lookup x g)
infer g (App s t) = case infer g s of
                         Fun a b -> if check g t a then b else error ""
                         _       -> error ""
infer g (Lam x t) = error "Cannot infer"
infer g (Lab l)   = error ""
infer g (Case t lus) = error ""


check :: Context -> Term -> Type -> Bool
check g (Var x) a           = elem (x,a) g                               -- (Thomas) Error: if the context is [(x,b),(x,a)] it should return FALSE. E.g.  \ x.(\x.x) : a->b->a types check but it should not. (Here, a and b are different concrete types, see test  0 below).
check g (App s t) a         = infer g (App s t) == a
check g (Lam x t) (Fun a b) = check ((x,a):g) t b                        -- (Thomas) A name may appear twice in a context, e.g. while checking Lam x (Lam x x) : a -> b -> b
check g (Lab l) (Enum ls)   = elem l ls
check g (Case t lus) a      = check g t (Enum (map fst lus)) 
                              && and (map (flip (check g) a . snd) lus)        
check _ _ _                 = False

-- tests
x  = "x"
x' = Var x


id'       = Lam x x'
unit      = Enum ["top"]
star      = Lab "*"
bool      = Enum ["true","false"]

a1        = Fun unit unit

test :: Int -> Bool
test 0     = check [] (Lam x id') (Fun unit (Fun bool unit))
-- Expected False but test 0 = True due to the error in the funciton check

test 1     = check [] id' a1
-- Expected True

test 2     = check [] (App id' star) unit 
-- Expected error "cannot infer"


-- Big step evaluation     
eval :: Term -> Term
eval (App t u) = let t' = eval t in case t' of 
                    Lam x r -> eval (subst r u x)
                    -- _       -> App t' u                    
eval (Case t lus) = let t' = eval t in case t' of 
                        Lab l -> fromJust (lookup l lus)
                        -- _     -> Case t' lus
eval t = t
     
subst :: Term -> Term -> Name -> Term
subst (Var y) u x | x == y    = u
                  | otherwise = Var y   
subst (App s t) u x = App (subst s u x) (subst t u x) 
subst (Lam y t) u x | y == x = Lam y t
                    | otherwise = Lam z (subst (rename t z y) u x) 
                                  where 
                                  z = newname y t u -- to avoid capture
subst (Case t lvs) u x = Case (subst t u x)[(l,subst v u x) | (l,v)<-lvs]                                  
subst t u x = t -- t = Lab l   

variables :: Term -> ([Name],[Name])
variables (Var x) = ([x],[])
variables (App s t) = (union fs ft, union bs bt) 
                      where 
                      (fs,bs) = variables s
                      (ft,bt) = variables t
variables (Lam x t) = (remove x ft, insert x bt) where (ft,bt) = variables t  
variables (Case t lus) = (unions (ft:fus) , unions (bt:bus))
                         where 
                         (ft,bt) = variables t
                         (fus,bus) = unzip $ map (variables . snd) lus
variables t = ([],[])

union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) | x < y = x : union xs (y:ys)
                    | x == y = x : union xs ys
                    | otherwise = y : union ys (x:xs)

insert x [] = [x]
insert x (y:ys) | x < y = x : y : ys
                | x == y = y:ys
                | otherwise = y : insert x ys

remove x [] = []
remove x (y:ys) | x < y = y : ys
                | x == y = ys
                | otherwise = y : remove x ys    

unions = foldr union []


newname y t u = if elem y fu 
                then head [ x | x <- [y++show i | i <- [0..]] , not (elem x (unions [fu,bu,ft,bt]))]
                else y
        
               
  where 
  (fu,bu) = variables u
  (ft,bt) = variables t  
              
-- Assumption : z does not occur at all in the term (neither freely, nor bounded)
rename :: Term -> Name-> Name -> Term 
rename (Var x) z y | y == x    = Var z
                   | otherwise = Var x   
rename (App s t) z y = App (rename s z y) (rename t z y) 
rename (Lam x t) z y | x == y = Lam x t
                     | otherwise = Lam x (rename t z y) 
rename (Case t lvs) z y = Case (rename t z y)[(l,rename v z y) | (l,v)<-lvs]                                  
rename t z y = t -- t = Lab l