module Test where

import Parser
import Syntax
import LC1c

-- parse :: Parser a -> SourceName -> String -> Either ParseError a


pt s = parse sTerm "" s

pt' s = case pt s of Right x -> x

checkTest :: String -> String -> String
checkTest st sa = "\n" ++ st ++ " : " ++ sa ++ "\n  ==> "
                  ++ case pt st of
	      	     	   	 Right t -> case pt sa of
	      	     	   	                      Right a -> show (check [] t a) 

inferTest :: String -> String
inferTest st = "\n" ++ st ++ " : ?\n  ==> "
                  ++ case pt st of
	      	     	   	 Right t -> show (infer [("id",pt' "{ a } -> { a }")] t)
 
go = putStrLn $ concat tests

tests =  [ 
          checkTest "'a" "{ a b c }"
        , checkTest "'a" "{ a a a }"
        , checkTest "'a" "{ b }"
        , checkTest "λ x → x" "{ a } → { a }"
        , checkTest "λ x → (λ x → x)" "{ a } → { b } → { a }"
        , checkTest "λ x → (λ x → x)" "{ a } → { b } → { b }"
        , checkTest "λ x → (λ x → x)" "{ a } → ({ b } → { b })"
        , checkTest "λ x → (λ x → x)" "({ a } → { b }) → { b }"
        , checkTest "λ x → x" "({ a } → { b }) → { a } → { b }"
        , checkTest "λ x → x" "({ a } → { b }) → { c } → { b }"
        , checkTest "(λ x → x) 'a" "{ a }"
        ]

test1 = checkTest "(λ x → x) 'a" "{ a }"