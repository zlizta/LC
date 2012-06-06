module LC1c where

-- Exercise
-- Implementation of Simply Typed Lambda Calculus with finite enumeration types.
-- 4th June 2012
-- Authors: ANBERREE Thomas, MIANLAI Zhou, NUO Li
-- Done at UNNC, SEB438 during our "Reading group meeting".
-- 
-- NEW in this version : 
-- It is still about Simply Typed Lambda Calculus (STLC) with Enym types but the Syntax is the full syntax of PiSigma, in prevision of future extension.
-- We simply ignore terms that are not in STLC for the moment.
-- Location information is added to terms for future error management and the PiSigma parser is used for testing.
-- The function type constructor is replaced by Q loc Pi.


import Data.Maybe (fromJust)
import Data.Set (Set, union, delete, insert, unions,singleton,empty,member,fromList)
import qualified Data.Set as Set
import Data.Map (Map,keysSet)
import qualified Data.Map as Map

import Syntax
import Location
import Utilities


type Message = [String]
type Source  = [String]
data Error = Error Source Location Message

instance Show Error where
   show (Error s l m) = unlines [angles (sepByComma s),locMessage l,concat m]

     
type Context = [(Name, Type)]


infer :: Context -> Term -> Either Error Type
infer g (Var loc x)     = case lookup x g of 
                               Just a  -> Right a
                               Nothing -> Left $ Error ["infer","Var"] loc ["Undeclared name ",x]  
                              
infer g (App s t)       = case infer g s of
                               Right (Q loc Pi (a,(_,b))) -> case check g t a of
                                                                  Right ()  ->  Right b
                                                                  Left e    ->  Left e 
                               Left e                     -> Left e
infer g (Lam loc (x,t))     =  Left $  Error ["infer","Lambda"] loc ["Cannot infer the type of a lambda abstraction"] 
infer g (Lab loc l)         =  Left $  Error ["infer","Label"]  loc ["Cannot infer the type of label ",l] 
infer g (Case loc t lus)    =  Left $  Error ["infer","Case"]   loc ["Cannot infer the type of a case construct"] 


check :: Context -> Term -> Type -> Either Error ()
check g (Var loc x) a           = case infer g (Var loc x) of
                                     Right a' -> if a == a' then Right ()
                                                            else Left $  Error ["check","Var"] loc ["Wrong type"]
                                     Left e -> Left e
check g (App     s t) a         = case infer g (App s t) of
                                     Right a' -> if a == a' then Right ()
                                                            else Left $  Error ["check","App"] Unknown ["Wrong type"]
                                     Left e -> Left e
check g (Lam _ (x,t)) (Q _ Pi (a,(_,b)))        = check ((x,a):g) t b
check g (Lam loc (x,t)) _                       = Left $  Error ["check","Lam"] loc ["Wrong type"]                  
check g (Lab loc l) (Enum _ ls)   | member l ls = Right ()
                                  | otherwise   = Left $ Error ["check","Lab"] loc ["Label ",l," is not in ",braces (sepByComma (Set.elems ls))]
check g (Case loc t lus) a                      = case check g t (Enum Unknown (keysSet lus)) of
                                                   Right () -> sequence (flip (check g) a) (Map.elems lus)
                                                                where
                                                                sequence f []     = Right ()
                                                                sequence f (x:xs) = case f x of {Right () -> sequence f xs; Left e -> Left e}
                                                   Left e -> Left e
                                                   
                                                  
check g t a = error ("<check> " ++ show t ++ "\n<check> " ++ show a)      -- To know which cases are missing.


-- Big step evaluation     
eval :: Term -> Term
eval (App    t u)      = let t' = eval t in case t' of  Lam _ (x,r) -> eval (subst r u x)                  
eval (Case loc t lus)  = let t' = eval t in case t' of  Lab _ l   -> fromJust (Map.lookup l lus)
eval (Lab  loc l)      = Lab loc l
eval (Lam  loc (x,t))    = Lam loc (x,t)
eval (Var  loc x)      = Var loc x

-- Substitution
-- subst t u x = t[u/x]     
subst :: Term -> Term -> Name -> Term
subst (Var loc y) u x | x == y       = u
                      | otherwise    = Var loc y   
subst (App     s t) u x              = App (subst s u x) (subst t u x) 
subst (Lam loc (y,t)) u x | y == x     = Lam loc (y,t)
                          | otherwise  = Lam loc (y',subst (rename t y' y) u x) 
                                       where 
                                       (fu,bu) = variables u
                                       (ft,bt) = variables t
                                       -- If y appears freely in u,we avoid capturing free occurrences of y in u by renaming y to y' in t
                                       -- where y' is the name y followed by a number such that y' does not appear free in t or u.
                                       -- Just to make it easier to read resulting terms, we also require that y' does not appear bounded in u or t,
                                       -- but this is not necessary. 
                                       y' | member y fu = head [ x | x <- [y++show i | i <- [0..]] , not (member x (unions [fu,bu,ft,bt]))]
                                          | otherwise   = y
subst (Case loc t lvs) u x            = Case loc (subst t u x) (Map.map (\ v -> subst v u x) lvs)                                 
subst (Lab  loc l) u x                = Lab loc l  

-- variables t = (Set of names with at least one free  occurrence in t, Set of names with at least one bound occurrence in t)
variables :: Term -> (Set Name,Set Name)
variables (Var  loc x)       = (singleton x,empty)
variables (App      s t)     = (union fs ft, union bs bt) 
                                where 
                                 (fs,bs) = variables s
                                 (ft,bt) = variables t
variables (Lam  loc (x,t))     = (delete x ft, insert x bt) where (ft,bt) = variables t  
variables (Case loc t lus) = (union  ft fus , union bt bus)
                                where 
                                 (ft,bt)   = variables t
                                 (fus,bus) =  Map.fold (\ (fs,bs) (ft,bt) -> (union fs ft, union bs bt)) (empty,empty) (Map.map variables lus)
variables (Lab  loc l)       = (empty,empty)


 
              
-- Assumption on rename t z y: z does not occur at all in term t (neither freely, nor bounded)
rename :: Term -> Name -> Name -> Term 
rename (Var loc x) z y | y == x        = Var loc z
                       | otherwise     = Var loc x   
rename (App     s t) z y               = App (rename s z y) (rename t z y) 
rename (Lam loc (x,t)) z y | x == y      = Lam loc (x,t)
                         | otherwise   = Lam loc (x,rename t z y) 
rename (Case loc t lvs) z y            = Case loc (rename t z y) (Map.map (\v -> rename v z y) lvs)                                  
rename (Lab  loc l) z y                = Lab loc l