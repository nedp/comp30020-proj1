{-
    State.hs

    Created by  :   Ned Pummeroy <nedp>
            at  :   2014-09-11 Thu 17:29:25
    Updated at  :   2014-09-11 Thu 17:31:13

    Student id  :   586530

    Description:
        This file defines types and functions for evaluating the
        best guess for a particular belief space, when trying to
        guess a hand of playing cards based on repeated feedback.

        This is part of a submission for Project 1 of the University
        of Melbourne subject COMP30020 Declarative Programming in
        semester 2 of 2014.
-}
module State ( State
            , state_guess
            , state_s
            , state_v
            , state_e
            , state_branches
            , createState
            , resolveState
            , bestState
            , statesFor
            , bestStateFor )
where
import GHC.Exts

{---------------------------------------------------------------------
 -
 -  State h r
 -
 -  Defines belief spaces associated with responses and tracks their
 -  overal size and score.
 -
 -  Characterised by:
 -      1.  A guess to which the responses will be given.
 -      2.  A list of branches, each of which is characterised by a
 -          response of type r, and a belief space of hands of h
 -          ( hand = [h], belief space = [[h]] )
 -
 -  Also stores calculated data:
 -      1.  The total size of the belief spaces in the state.
 -      2.  The total value of the belief spaces in the state.
 -          The value of each belief space is the negative of its
 -          size squared.
 -      3.  The expected value of the belief spaces in the state,
 -          which is the average of their values, weighted by their
 -          size.
 ---------------------------------------------------------------------
-}
data State h r = State { state_guess    :: [h]
                     , state_s        :: Int
                     , state_v        :: Int
                     , state_e        :: Double
                     , state_branches :: [(r, [[h]])]
                     }
    deriving ( Show )

{---------------------------------------------------------------------
 -  createState
 -      rf :: ([h] -> [h] -> r)
 -          The response function to use to group belief spaces in
 -          the state.
 -      g :: [h]
 -          The guess to create the state for.
 -      as :: [[h]]
 -          The answers to construct belief spaces from.
 -
 -      Returns a state which groups all of the possible answers by the
 -      response they would generate with that guess.
 ---------------------------------------------------------------------
-}
createState :: Ord r => ([h] -> [h] -> r) -> [h] -> [[h]] -> State h r
createState rf g as
    = addToState rf (groupWith (rf g) as) $ State g 0 0 0 []


{---------------------------------------------------------------------
 -  addToState
 -      rf :: ([h] -> [h] -> r)
 -          The response function to use to generate the responses.
 -      hss :: [[[h]]]
 -          The list of belief spaces to add to the state.
 -      :: State h r
 -          The state to which to add the belief spaces.
 -
 -      Returns the state with the belief spaces added, each associated
 -      with the correct response, with their scores, sizes, and
 -      expected values updated.
 ---------------------------------------------------------------------
-}
-- [h] = hand, [[h]] = belief space, [[[h]]] = list of belief spaces
addToState :: ([h] -> [h] -> r) -> [[[h]]] -> State h r -> State h r
addToState _ [] (State g s v _ bs) =
    let
        e = fromIntegral v / fromIntegral s
    in  State g s v e bs
addToState rf (hs:hst) (State g ts tv _ bs) =
    let
        hss = length hs
        hsv = -hss^2
        r  = rf g $ head hs
    in addToState rf hst
       $ State g (ts + hss) (tv + hsv) 0 $ ( r, hs ):bs


{---------------------------------------------------------------------
 -  resolveState
 -      r :: r
 -          the response to search for.
 -      t :: State h r
 -          the state in which to search.
 -
 -      Returns the belief space in the state associated with the
 -      specified response.  Throws an error if the response is
 -      not found.
 ---------------------------------------------------------------------
-}
resolveState :: ( Show h, Show r, Eq r ) => r -> State h r -> [[h]]
resolveState r t@(State _ _ _ _ [])
    = error $ "response (" ++ show r ++ ") not in state:\n" ++ show t
resolveState r (State g s v e ((tr, hs):bt))
    | tr == r   = hs
    | otherwise = resolveState r $ State g s v e bt


{---------------------------------------------------------------------
 -  bestState
 -      ts :: [State h r]
 -          the list of states to search.
 -
 -      Returns the state in the list with the highest expected value.
 -      Throws an error if an empty list is given.
 ---------------------------------------------------------------------
-}
bestState :: [State h r] -> State h r
bestState []     = error "No hand state available"
bestState (t:tt) = keepBest t tt
    where
        keepBest best [] = best
        keepBest best (t:tt)
            | state_e t > state_e best = keepBest t tt
            | otherwise              = keepBest best tt


{---------------------------------------------------------------------
 -  statesFor
 -      rf :: ([h] -> [h] -> r)
 -          The response function to use to group belief spaces in
 -          the states.
 -      gs :: [[h]]
 -          The guesses to create states for.
 -      as :: [[h]]
 -          The answers to construct belief spaces from.
 -
 -      Returns a list of states, one per guess, which groups all of
 -      the possible answers by the response they would generate with
 -      that guess.
 ---------------------------------------------------------------------
-}
statesFor :: Ord r => ([h] -> [h] -> r) -> [[h]] -> [[h]] -> [State h r]
statesFor rf gs as = map (\g -> createState rf g as) gs


{---------------------------------------------------------------------
 -  bestStateFor
 -      Passes the arguments to statesFor, and returns the best state
 -      from the result.
 ---------------------------------------------------------------------
-}
bestStateFor :: Ord r => ([h] -> [h] -> r) -> [[h]] -> [[h]] -> State h r
bestStateFor rf gs as = bestState $ statesFor rf gs as


