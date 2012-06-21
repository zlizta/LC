module Values where


import Syntax
import Environment


{- [alti2010flops]
Values (v), weak head normal forms (w) and neutral values (n) are defined as follows:
v ::= w | n
w ::= Type | (x : S ) -> T | \x -> t | (x : S) * T | (t , u) | {l1 ... ln} | 'l | ^T | [ t ] | Rec T | fold t
n ::= x | n t | split n with (x , y) -> t | case n of  { l1 -> t1 | ... | ln ->tn } | !n | unfold n as x -> t
-}

newtype Boxed = Boxed (Closure Term)  deriving (Show, Eq)
instance CLOSURE Boxed where
  getContext (Boxed c) = getContext c
  putContext (Boxed c) = Boxed . putContext c
  
data    Value = VNeutral Neutral
              -- Weak head normal forms 
              | VType
              | VQ PiSigma  ((Type, Bind Type),Context)
              | VLambda     (Bind (Closure Term))
              | VPair       (Closure (Term, Term))
              | VEnum       [Label]
              | VLabel       Label
              | VLift       (Closure Type)
              | VBox         Boxed
              | VRec        (Closure Type)
              | VFold       (Closure Term)
              deriving (Eq, Show)
           
data  Neutral = -- Neutral terms 
                NVar    Index
              | NUndefined Index
              | NApp    Neutral (Closure Term)
              | NCase   Neutral (Closure [(Label,Term)])
              | NSplit  Neutral (Bind (Bind (Closure Term)))
              | NForce  Neutral
              | NUnfold Neutral (Bind (Closure Term))
              deriving (Eq, Show)