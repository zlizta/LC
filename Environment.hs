{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Environment where

import Prelude hiding (lookup)
import qualified Prelude as Prelude

import Data.Maybe (fromJust)
import Data.Map

import Location
import Syntax

{- The main entities PiSigma manipulates are closures : PiSigma type-checks and evaluates closures.
   A *closure* is an expression e of type a along with a *context* defining the names which may appear in e.
   We will mainly use the word *expression* for expressions in which *names* may appear, such as terms or types of the language PiSigma.
   A *name*, or a variable, or an identifier, is simply a string. 
-}

type    Closure a           = (a,Context)

{- A name may appear in different contexts and even several times in the same context, with a different definition for each occurrence (homonymy).
   In a given context, the most "recent" occurrence of a name overshadows the older ones. -}
   
type    Context             = [(Name, Index)]

{- In a context, names are associated to unique names called *indices*. -}

{- All definitions are actually stored in an environment which maps indices to triples (closure term, closure type, printing info). -}

type    Environment         = Map Index (Closure Term, Closure Type, PrintInfo)
data    PrintInfo           = PrintInfo { name :: Name , expand :: Bool }        deriving Show

{- In a triple (cTerm,cType,pInfo), cTerm or cType may be undefined: -}

cUndefined i = (Undefined i ,emptyContext)               


class CLOSURE a where
  getContext :: a -> Context
  putContext :: a -> Context -> a



-- * Closures
instance GetLoc a => GetLoc (Closure a) where getLoc = getLoc . fst

-- * Contexts
emptyContext :: Context
emptyContext = []

-- | Add a new association (x,i) to context s. 
extendContext :: Context -> Name -> Index -> Context                        
extendContext s x i = (x,i):s

declareType :: Environment -> Context -> Name -> Type -> (Environment, Context)
declareType e g x =  
-- * Environments
emptyEnvironment = empty

env_lookup_term :: Index -> Environment -> Closure Term
env_lookup_term i e = case lookup i e of Just (t,_,_) -> t
                      
env_lookup_printInfo :: Index -> Environment -> PrintInfo
env_lookup_printInfo i e = 
                 case lookup i e of
                      Just (t,a,p) -> p


-- | Updates an existing entry and keeps the original printing info.
env_update_term :: Index -> Closure Term -> Environment -> Environment
env_update_term i t e = adjust (\(_,a,p) -> (t,a,p)) i e

-- * CLOSURES 
instance CLOSURE (Closure a) where
  getContext (_, s)   = s
  putContext (a, _) s = (a, s)

instance CLOSURE a => CLOSURE (Bind a) where
  getContext (_, a)   = getContext a
  putContext (x, a) s = (x, putContext a s)
  

