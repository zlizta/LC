module Syntax where

import Data.Set
import Data.Map

import Location

type Type    = Term
type Name    = String    -- ^ Identifiers
data ProgramEntry = Decl Location Name Type
                  | Defn Location Name Term
                  deriving (Show, Eq, Ord)

type Prog = [ProgramEntry]

type Bind a  = (Name, a) -- ^ (x,t) indicates that t is the context of the identifier x, where t is typically a term or a term closure.
type Label   = String    -- ^ Inhabitants of enumeration types (Enum).

data PiSigma = Pi                                       -- (x : S) -> T
             | Sigma                                    -- (x : S) *  T
             deriving (Show, Eq, Ord)

data Term    = Var     Location Name                         -- variables
             | Let     Location Prog Term                    -- letrec :   let G in t ; G is a program, i.e. a sequence of delcarations x : T and possibly recursive definitions x = t
             | Type    Location                              -- type of all types
             | Q       Location PiSigma (Type, Bind Type)    -- quantifiers  : Q Pi (S,(x,T))  means  (x : S) -> T , Q Sigma (S,(x,T))  means  (x : S) *  T
             | Lam     Location (Bind Term)                  -- \x -> t
             | App              Term Term                    -- application :   t u
             | Pair    Location Term Term                    -- pairs :   (t,u)
             | Split   Location Term (Bind (Bind Term))      -- Sigma elimination :   split t with (x,y) -> u
             | Enum    Location (Set Label)                  -- enumerations (finite types)
             | Lab     Location Label                        -- labels
             | Case    Location Term (Map Label Term)        -- case t of { L1 -> u1 | ... | L1 -> un }
             | Lift    Location Term                         -- lifting :  ^T
             | Box     Location Term                         -- box :   [t]
             | Force   Location Term                         -- box opener :   !t
             | Rec     Location Term                         -- Rec T
             | Fold    Location Term                         -- fold t
             | Unfold  Location Term (Bind Term)             -- unfold t as x -> u
             deriving (Show, Eq, Ord)

instance GetLoc Term where
  getLoc (Var    l _  ) = l
  getLoc (Let    l _ _) = l
  getLoc (Type   l    ) = l
  getLoc (Q      l _ _) = l
  getLoc (Lam    l _  ) = l
  getLoc (App      t _) = getLoc t
  getLoc (Pair   l _ _) = l
  getLoc (Split  l _ _) = l
  getLoc (Enum   l _  ) = l
  getLoc (Lab    l _  ) = l
  getLoc (Case   l _ _) = l
  getLoc (Lift   l _  ) = l
  getLoc (Box    l _  ) = l
  getLoc (Force  l _  ) = l
  getLoc (Rec    l _  ) = l
  getLoc (Fold   l _  ) = l
  getLoc (Unfold l _ _) = l
