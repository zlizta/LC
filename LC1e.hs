{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LC1e where


-- 4th June 2012
-- Authors: ANBERREE Thomas, MIANLAI Zhou, NUO Li
-- Done at UNNC, SEB438 during our "Reading group meeting".



import Data.Maybe (fromJust)
import Data.Set (Set, union, delete, insert, unions,singleton,empty,member,fromList)
import qualified Data.Set as Set


import Data.Map (Map,keysSet)
import qualified Data.Map as Map


import Syntax
import Location
import Environment
import Utilities

import Control.Monad.Error
import Control.Monad.State 
import Control.Monad.Identity
import Control.Monad.Reader

data Constraints = Constraint -- Not used for the moment

type Message = [String]
type Source  = [String]
type EvalError = String

run :: Environment -> Eval a -> Either EvalError (a, Environment)
run e (Eval p) = runIdentity $ runErrorT $ runStateT (runReaderT p Constraint) e -- Constraint ignored for the moment

evalerror s l m = throwError $ unlines [angles (sepByComma s),locMessage l,concat m]

newtype Eval a = Eval { unEval :: ReaderT Constraints (StateT Environment (ErrorT EvalError Identity)) a }
    deriving ( Monad
           , MonadError EvalError
           , MonadState Environment
           , MonadReader Constraints
           , Functor)


--
checkProg :: Closure Prog -> Eval Context
checkProg gp = checkProg' Set.empty Set.empty gp

checkProg' :: Set Name -> Set Name -> Closure Prog -> Eval Context
checkProg' decls defns (g, []) = return g
checkProg' decls defns (g, (Decl _ x a) : prg) =
    do g' <- doDeclareType g x (g,a)
       if x `Set.member` decls
        then throwError $ "Variable " ++ x ++ " declared twice in the same block."
        else checkProg' (Set.insert x decls) defns (g', prg)
        
checkProg' decls defns (g, (Defn l x t) : prg) =
    do a <- infer (g, Var l x)  -- ^ Looks up the type of variable x in context g
       check (g,t) a
       doDefineTerm g x (g,t) 
       checkProg' decls (Set.insert x defns) (g,prg)
--

 
eq :: Closure Type -> Closure Type -> Eval ()
eq (g,a) (g',a') = if a == a'   -- For the moment, only to be used with types of STLC
                      then return ()
                      else evalerror ["eq"] Unknown ["Type mismatch: \n",show a,"\n",show a'] 

infer :: Closure Term -> Eval (Closure Type)
infer (g , (Var loc x))     =  do
                               env <- get
                               case lookupType env g x of 
                                Just ga  -> return ga
                                Nothing  -> evalerror ["infer","Var"] loc ["Undeclared name ",x]  
                              
infer (g , App s t)       =  do 
                               (g',c) <- infer (g,s)
                               case c of
                                (Q loc Pi (a,(_,b))) -> check (g,t) (g',a) >> return (g',b)
                                _                    -> evalerror ["infer","App"] (getLoc s) ["Pi type expected"]
                                
infer (g, (Lam loc (x,t)))    =  evalerror ["infer","Lambda"] loc ["Cannot infer the type of a lambda abstraction"] 
infer (g, (Lab loc l))        =  evalerror ["infer","Label"]  loc ["Cannot infer the type of label ",l] 
infer (g, (Case loc t lus))   =  evalerror ["infer","Case"]   loc ["Cannot infer the type of a case construct"] 

doDeclareType :: Context -> Name -> Closure Type -> Eval Context
doDeclareType g x ha =
    do
    env <- get
    let (env',g') = declareType env g x ha
     in put env' >> return g'

doDeclareTerm :: Context -> Name -> Closure Term -> Eval Context
doDeclareTerm g x ha =
    do
    env <- get
    let (env',g') = declareTerm env g x ha
     in put env' >> return g'

doDefineTerm :: Context -> Name -> Closure Term -> Eval ()
doDefineTerm g x gt =
    do
    env <- get
    put $ defineTerm env g x gt
 
     
check :: (Closure Term) -> (Closure Type) -> Eval ()
check (g, (Var loc x)) ga       = 
    do
    ga' <- infer (g,Var loc x)
    eq ga ga'

check (g, App   s t) ga       = 
    infer (g,App s t) >>= eq ga

check (g,(Lam _ (x,t))) (h,(Q _ Pi (a,(_,b)))) = 
    do
    g' <- doDeclareType g x (h,a)
    check (g',t) (h,b)
    
    
check (g, (Lam loc (x,t))) ha  =
    evalerror ["check","Lam"] loc ["unexpected type"]                  

    
check (g, Lab loc l) (h, Enum _ ls)   
    | member l ls = return ()
    | otherwise   = evalerror ["check","Lab"] loc ["Label ",l," is not in ",braces (sepByComma (Set.elems ls))]
check (g, Lab loc l)  (h,a) = evalerror ["check","Lab"] loc ["Label ",l," : unexpected type"]  



check (g, (Case loc t lus)) ha = 
    do
    -- TO DO : raise error if repeated labels in the labels-terms association.
    check (g,t) (emptyContext,Enum Unknown (keysSet lus))
    -- It would be better to do the following using a different "sequencing" operator
    -- where elements in the list are checked independently from each other (in the same environment).
    -- The errors should be propagated (possibly accummulated), but not modifications of the environment.
    sequence_ [ check (g,u) ha | u <- Map.elems lus ] 
    
            
                                             
                                                   
-- To know which cases are missing.                                                  
check (g, t) (h, a) = error ("<check> CASE MISSING (BUG)\n" ++ "<check> " ++ show t ++ "\n<check> " ++ show a)      


-- Big step evaluation  
eval :: (Closure Term) -> Eval (Closure Term)
eval (g,App    t u)      = 
    do
    (g',Lam _ (x,r)) <- eval (g,t) 
    g'' <- doDeclareTerm g' x (g,u)
    eval (g'',r)
 
eval (g,Case loc t lus)  =
    do 
    (g, Lab _ l) <- eval (g,t)
    eval (g, fromJust (Map.lookup l lus)) -- TO DO: introduce constraints here. !!!!!!!!!
    
eval ga     = return ga
 


{-
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

-}