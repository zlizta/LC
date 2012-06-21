module Test1e where

import Parser
import Syntax
import Environment
import Location
import LC1e

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Set (Set)
import qualified Data.Set as Set
-- parse :: Parser a -> SourceName -> String -> Either ParseError a


 
go = 
 do
 s <- readFile "test1.txt"
 case parse sProg "test1.txt" s of
    Left e -> putStrLn $ show e
    Right prog -> case run emptyEnvironment (checkProg2 (emptyContext, prog)) of
                                                            Left e       -> putStrLn $ e
                                                            Right ((g,ls),env) -> putStrLn $ unlines ls
 
 
 
 

--
checkProg2 :: Closure Prog -> Eval (Context,[String])
checkProg2 gp = checkProg2' Set.empty Set.empty gp
    
checkProg2' :: Set Name -> Set Name -> Closure Prog -> Eval (Context,[String])
checkProg2' decls defns (g, []) = return (g,[])
checkProg2' decls defns (g, (Decl lx x a) : (Defn ly y t) : prg) 
 | unmark x == unmark y =
    do
    env0 <- get
    case run env0 (checkProg (g, [(Decl lx (unmark x) a) , (Defn ly (unmark y) t)])) of
        Left e -> do
                  put env0
                  (g2,ls2) <- checkProg2' decls defns (g,prg)
                  return (g2,(report x False):(indent (unmarkpad x ++ " : ERROR : ") e):ls2)
        Right (g1,env1) ->  do
                            put env1
                            (g2,ls2) <- checkProg2' decls defns (g1,prg)
                            return (g2,(report x True):(unmarkpad x++" : OK"):ls2)

report ('_':cs) b | b == False = dashline
                  | b == True  = unexpected
report cs True  = dashline
report cs False = unexpected

unexpected = "!!!!!!!!! UNEXCPECTED OUTCOME WHEN TYPE CHECKING !!!!!!!!!!"

unmark ('_':cs) = cs
unmark cs = cs

unmarkpad x = pad (unmark x)

pad x = take 12 $ x ++ repeat ' '
dashline = take 160 (repeat '-') 
   
indent x y = x ++  unlines (head (lines y) : (map ((spaces (length x))++) (safetail (lines y))))   

safetail (x:xs) = xs
safetail [] = []
spaces i = take i (repeat ' ')