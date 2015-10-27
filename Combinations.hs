{-
    Combinations.hs

    Created by  :   Ned Pummeroy <nedp>
            at  :   2014-09-11 Thu 16:32:07
    Updated at  :   2014-09-11 Thu 16:52:29

    Student id  :   586530
    
    Combinations
        This module defines functions for processing lists to create
        various combinations and permutations of their elements.

        The functions for basic combinations and permutations are
        based on the code found at:
        http://www.haskell.org/haskellwiki/99_questions/Solutions/26
        (H-99: Ninety-Nine Haskell Problems, Question 26 Solutions)

        This is part of a submission for Project 1 of the University 
        of Melbourne subject COMP30020 Declarative Programming in
        semester 2 of 2014.
-}
module Combinations ( combinations
                    , rCombinations
                    , permutations
                    , rPermutations
                    , takeNFrom
                    , permNFrom
                    , concatRep )
where
import qualified Data.List as DL


{---------------------------------------------------------------------
 -  combinations
 -      Returns a list of all possible combinations of n items from
 -      the input list.  Repeated elements are not allowed.
 ---------------------------------------------------------------------
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n xs@(x:xt) 
    = (map (x:) (combinations (n - 1) xt))
      ++ combinations n xt

{---------------------------------------------------------------------
 -  rCombinations
 -      Returns a list of all possible combinations of n items from
 -      the input list.  Repeated elements are allowed.
 ---------------------------------------------------------------------
-}
rCombinations :: Int -> [a] -> [[a]]
rCombinations 0 _  = [[]]
rCombinations _ [] = []
rCombinations n xs@(x:xt)
    = (map (x:) (rCombinations (n - 1) xs))
      ++ rCombinations n xt

{---------------------------------------------------------------------
 -  permutations
 -      Returns a list of all possible permutations of n items from
 -      the input list.  Repeated elements are not allowed.
 ---------------------------------------------------------------------
-}
permutations :: Int -> [a] -> [[a]]
permutations n xs = p n [] xs
    where
        p 0 _  _  = [[]]
        p _ _ []  = []
        p n ps xs@(x:xt) 
            = (map (x:) (p (n - 1) [] (ps++xt)))
              ++ p n (x:ps) xt 

{---------------------------------------------------------------------
 -  rPermutations
 -      Returns a list of all possible permutations of n items from
 -      the input list.  Repeated elements are allowed.
 ---------------------------------------------------------------------
-}
rPermutations :: Int -> [a] -> [[a]]
rPermutations n xs = rp n [] xs
    where
        rp 0 _  _  = [[]]
        rp _ _ []  = []
        rp n ps xs@(x:xt) 
            = (map (x:) (rp (n - 1) [] (ps++xs)))
              ++ rp n (x:ps) xt 
    
{---------------------------------------------------------------------
 -  takeNFrom
 -      Returns all possible __combinations__ of groups such that
 -      for each tuple ( n, es ) in the input list, there is a group
 -      which is the output of the supplied combination function
 -      with arguments n and es.
 ---------------------------------------------------------------------
-}
takeNFrom :: (Int -> [a] -> [[a]]) -> [( Int, [a])] -> [[a]]
takeNFrom _  []              = [[]]
takeNFrom cf ls@(( n, gs ) : t) = [ as ++ bs 
                                  | as <- (cf n gs)
                                  , bs <- takeNFrom cf t]

{---------------------------------------------------------------------
 -  permNFrom
 -      Returns all possible __permutations__ of groups such that
 -      for each tuple ( n, es ) in the input list, there is a group
 -      which is the output of the supplied combination function
 -      with arguments n and es.
 ---------------------------------------------------------------------
-}
permNFrom :: (Int -> [a] -> [[a]]) -> [( Int, [a])] -> [[a]]
permNFrom cf ls = concatMap DL.permutations $ takeNFrom cf ls

{---------------------------------------------------------------------
 -  concatRep
 -      concatMap with (replicate n) as the transform.
 ---------------------------------------------------------------------
-}
concatRep :: Int -> [a] -> [a]
concatRep n = concatMap $ replicate n

