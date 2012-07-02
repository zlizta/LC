{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LC1e where


-- 4th June 2012
-- Authors: ANBERREE Thomas, MIANLAI Zhou, NUO Li
-- Done at UNNC, SEB438 
-- We still only consider the STLC (Simply Typed Lambda Calculus)
-- fragment of PiSigma, for the moment. However, the full PiSigma syntax is defined and other features,
-- such as a reader monad for constraints, are present but not used. They will be useful when extending 
-- STLC to dependent types.



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

-- The Eval monad in which type checking and evaluation are performed.
newtype Eval a = Eval { unEval :: ReaderT Constraints (StateT Environment (ErrorT EvalError Identity)) a }
    deriving ( Monad
           , MonadError EvalError
           , MonadState Environment
           , MonadReader Constraints
           , Functor)

type EvalError  = String
evalerror s l m = throwError $ unlines [angles (sepByComma s),locMessage l,concat m]

data Constraints = Constraint -- Not used for the moment
           
run :: Environment -> Eval a -> Either EvalError (a, Environment)
run e (Eval p) = runIdentity $ runErrorT $ runStateT (runReaderT p Constraint) e -- Constraints ignored for the moment

-- Basic context operations
declareType :: Context -> Name -> Closure Type -> Eval Context
declareType g x ha =
    do
    env <- get
    let (env',g') = env_declareType env g x ha
     in put env' >> return g'

declareTerm :: Context -> Name -> Closure Term -> Eval Context
declareTerm g x ha =
    do
    env <- get
    let (env',g') = env_declareTerm env g x ha
     in put env' >> return g'

defineTerm :: Context -> Name -> Closure Term -> Eval ()
defineTerm g x gt =
    do
    env <- get
    put $ env_defineTerm env g x gt
    
-- Type checking a pogram
checkProg :: Closure Prog -> Eval Context

checkProg gp = cp Set.empty Set.empty gp
    where
    cp :: Set Name -> Set Name -> Closure Prog -> Eval Context
    cp decls defns (g, []) = return g
    cp decls defns (g, (Decl loc x a) : prg) =
        do g' <- declareType g x (g,a)
           if x `Set.member` decls
            then evalerror [] loc ["Variable ", x, " declared twice in the same block."]
            else cp (Set.insert x decls) defns (g', prg)
            
    cp decls defns (g, (Defn loc x t) : prg) =
        do if x `Set.member` defns 
            then evalerror [] loc ["Variable ", x, " defined twice in the same block."]
            else
             do
             a <- infer (g, Var loc x) 
             check (g,t) a
             defineTerm g x (g,t) 
             cp decls (Set.insert x defns) (g,prg)

-- Equality
-- For type checking STLC, equality is only used with proper types.
-- Types are equal if and only if they are syntactically equal.
eq :: Closure Type -> Closure Type -> Eval ()
eq (_,a) (_,a') = if a == a'  
                      then return ()
                      else evalerror ["eq"] (getLoc a) ["Type mismatch: \n",show a,"\n",show a'] 

eqTerm :: Closure Term -> Closure Term -> Eval ()
-- EXERCISE: what can we do? You may assume that the types of the two arguments are known to be equal if it helps.
eqTerm (g,t) (g',t') = ?


-- Type inference
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
                                
infer (g, t)    =  evalerror ["infer"] (getLoc t) ["Cannot infer the type of term ",show t] 



-- Type checking

-- Gamma |- t => a'   a = a'
-- -------------------------
-- Gamma |- t <= a 

check_by_infer_eq :: (Closure Term) -> (Closure Type) -> Eval ()
check_by_infer_eq gt ha = infer gt >>= eq ha
 
check :: (Closure Term) -> (Closure Type) -> Eval ()
check gt@(_, Var _ _) ha = check_by_infer_eq gt ha
check gt@(_, App _ _) ha = check_by_infer_eq gt ha

check (g,(Lam _ (x,t))) (h,(Q _ Pi (a,(_,b)))) = 
    do
    g' <- declareType g x (h,a)
    check (g',t) (h,b)
 
check (g, Lab loc l) (h, Enum _ ls)   
    | member l ls = return ()
    | otherwise   = evalerror ["check","Lab"] loc ["Label ",l," is not in ",braces (sepByComma (Set.elems ls))]

check (g, (Case loc t lus)) ha = 
    do
    check (g,t) (emptyContext,Enum Unknown (keysSet lus))
    sequence_ [ check (g,u) ha | u <- Map.elems lus ] 
    
check (g,t)  (_,a) = evalerror ["check"] (getLoc t) ["<check> unexpected type\n","<check> ",show t,"\n<check> ",show a]            
                                           
-- Big step evaluation  
eval :: (Closure Term) -> Eval (Closure Term)
eval (g,App    t u)      = 
    do
    (g',Lam _ (x,r)) <- eval (g,t) 
    g'' <- declareTerm g' x (g,u)
    eval (g'',r)
 
eval (g,Case loc t lus)  =
    do 
    (g, Lab _ l) <- eval (g,t)
    eval (g, fromJust (Map.lookup l lus)) 
    
eval ga     = return ga
 

