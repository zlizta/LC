module Location where

-- | Locations are used to indicate the position
-- of errors in the source programme when displaying error
-- messages. They are created by the parser which produces 
-- concrete terms (:: Term) tagged with locations.  

data Location
  = Unknown
  | Location { filename :: !String
        , line     :: !Int
        , column   :: !Int }
  deriving (--Show, 
           Ord)
instance Show Location where
    show _ = ""
-- | Locations are always equal. This allows us to
-- derive equality for the abstract syntax that ignores
-- the locations.
instance Eq Location where
  _ == _ = True
  
locMessage :: Location -> String
locMessage Unknown     = "<unknown>:"
locMessage (Location f l c) = concat
                       [ f
                       , ":"
                       , show l
                       , ":"
                       , show c
                       , ":"
                       ]



class GetLoc a where
  getLoc :: a -> Location