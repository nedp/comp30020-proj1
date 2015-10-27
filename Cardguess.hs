{-
    Cardguess.hs

    Created by  :   Ned Pummeroy <nedp>
            at  :   2014-08-28 Thu 17:23:07
    Updated at  :   2014-09-11 Thu 17:55:06

    Student id  :   586530

    Description:
        This file defines types and functions for making repeated
        attempts to guess a hand of playing cards based on responses,
        which are characterised by the hand, and a guess.

        This is part of a submission for Project 1 of the University
        of Melbourne subject COMP30020 Declarative Programming in
        semester 2 of 2014.
-}


module Cardguess (initialGuess, nextGuess, GameState)
where

import Card
import Combinations as COM
import State
import Data.List

{- When the belief space for card hands is larger than this,
 - we use belief spaces for ranks and suits instead.
 -}
kCOMBINE_THRESHOLD = 13 * 52 :: Int -- Experimentally, this seems reliable.

{- If and only if the hand size is smaller than this, use a
 - card based belief space initially.
 -}
kBIG_HAND_SIZE = 3 :: Int -- Experimentally, this seems reliable.

kMIN_SUIT  = minBound :: Suit
kMAX_SUIT  = maxBound :: Suit
kALL_SUITS = [minBound..maxBound] :: [Suit]

kMIN_RANK  = minBound :: Rank
kMAX_RANK  = maxBound :: Rank
kALL_RANKS = [minBound..maxBound] :: [Rank]

{---------------------------------------------------------------------
 -  initialGuess
 -      n :: Int
 -          the number of cards to be guessed
 -
 -      Returns a list of cards to be guessed, and a game state
 -      containing either no information for 1-2 card guesses,
 -      or a pair of states for suit and rank for 3+ card guesses.
 -      Constructs the guess from the rank and suit hands which are
 -      (nominally) optimal on the first guess.
 ---------------------------------------------------------------------
-}
initialGuess :: Int -> ( [Card], GameState )
initialGuess n = ( zipWith Card sh rh, gs )
    where
        {- Guess suits using Donald Knuth's 1977 mastermind strategy -}
        sh = take n $ cycle [ Club, Spade ] -- Suit Hand

        {- Guess equally spaced ranks -}
        rh = map ((\x -> [R2 ..] !! x).round) -- Rank Hand
             $ map ((* oneNp1th ) . fromIntegral) [1..n]

        {- Determine whether to use card, or rank/suit belief space for
         - the next guess. Need to know now to determine what to remember.
         -}
        gs = if n < kBIG_HAND_SIZE
             then GameState Nothing Nothing           []
             else GameState Nothing (Just ( st, rt )) []

        -- @TODO Test if need this:
        -- chs = map Hand
        --    $ Cardguess.combinations n
        --      [Card Club R2 .. Card Spade Ace]

        {- If we're using split belief spaces, prepare the states now -}
        st = createState sResponse sh shs -- Suit State
        rt = createState rResponse rh rhs -- Rank State
        shs = rPermutations n kALL_SUITS -- Suit HandS
        rhs = rCombinations n kALL_RANKS -- Rank HandS

        -- n guesses -> n+1 equal partitions
        oneNp1th = fromIntegral (length kALL_RANKS) / fromIntegral (n + 1)

{---------------------------------------------------------------------
 -  nextGuess
 -      :: ( [Card], GameState )
 -          the output of the previous guess
 -      :: Response
 -          the response to the previous guess
 -
 -      Returns a list of cards to be guessed, and a game state
 -      containing either no information for 1-2 card guesses,
 -      or a pair of states for suit and rank for 3+ card guesses.
 -
 -      First we resolve the state(s) passed to us by retrieving the
 -      branch which matches the response, and using the hands in the
 -      branch as our new belief space of possible guesses.
 -
 -      For large belief spaces, we process ranks and suits in
 -      isolation to complexity at the expense of guess quality. We
 -      revert to processing whole cards when the belief space becomes
 -      sufficiently small.
 -
 -      For each guess, a state is created in which each branch
 -      is a response and the belief space which will be consistent
 -      with that response for the state's guess.  We find the state
 -      which has the smallest expected belief space size, use its
 -      guess, and remember the state for quick processing next guess.
 ---------------------------------------------------------------------
-}
nextGuess :: ( [Card], GameState ) -> Response -> ( [Card], GameState )

-- pcs = Previous CardS
-- mpct = Maybe Previous Card State
-- pR = Previous Response - Capitalised to stand out from similar names.
{- Given Card State -}
nextGuess ( pcs, GameState mpct Nothing _ ) pR
    = ( state_guess ct, GameState (Just ct) Nothing [] )
    where
        ct = bestStateFor response chs chs
        chs = case mpct of Nothing  -> answersFor  pR pcs
                           Just pct -> resolveState pR pct

-- pst = Previous Suit State
-- prt = Previous Rank State
-- pRX = Previous Response (Lower rank)
--                         (correct Rank)
--                         (Higher rank)
--                         (correct Suit)
{- Given Suit and Rank States -}
nextGuess ( pcs, GameState Nothing (Just ( pst, prt )) history )
          pR@( _, pRL, pRR, pRH, pRS )
    = ( ch, GameState mct msrt $ ( pcs, pR ) : history )
    where
        {- Use full card belief space only if it will be 'small enough' -}
        ( ch, mct, msrt ) = if state_s st * state_s rt < kCOMBINE_THRESHOLD
                            then ( state_guess ct, Just ct, Nothing )
                            else ( srguess      , Nothing, Just ( st, rt ) )

        {- Combine rank and suit belief spaces and process the result. -}
        ct = bestStateFor response chs $! chs -- Card State
        -- Strict because we definitely have to process all elements.
        chs = foldl' useHistory ichs history -- Card HandS trimmed by history
        ichs = nubSpaceMerge Card shs rhs -- Initial Card HandS

        {- Result from processing suit and rank belief spaces -}
        srguess = zipWith Card (state_guess st) (state_guess rt)
        st = bestStateFor sResponse shs     shs -- best Suit State
        rt = bestStateFor rResponse bestrhs rhs -- best Rank State

        {- Process the previous states and the response -}
        bestrhs = if null rhns
                      then rhs
                      else rhns
        rhns = filter isNub rhs -- Rank Hand NubS
        shs  = resolveState pRS               pst -- Suit HandS
        rhs  = resolveState ( pRL, pRR, pRH ) prt -- Rank HandS
        -- Prefer to use hands with no repeated ranks, to reduce time
        -- complexity.  Keep repeated suits because the belief space
        -- for suits is smaller, so the trade off between complexity
        -- and guess quality is better.

{---------------------------------------------------------------------
 -
 -  GameState
 -
 -  Used for passing information from one guess to the next.
 -
 -  Characterised by:
 -      1.  A state for card belief spaces, if the belief space is
 -          small enough for practical processing.
 -      2.  A pair of states for suit and rank belief spaces, if the
 -          card belief space is too large.
 -      3.  A history of guesses and responses, used to prune the
 -          card belief space when we first start processing it.
 -
----------------------------------------------------------------------
-}
data GameState = GameState (Maybe (State Card CResponse))
                           (Maybe ( State Suit SResponse
                                  , State Rank RResponse ))
                           [( [Card], Response )]
    deriving ( Show )

{---------------------------------------------------------------------
 -  answersFor
 -      Response ::
 -          the response given
 -      [Card] ::
 -          the guess for which the response was given
 -      Returns a list of all possible answers, given that the
 -      specified guess had the specified response.
 -
 -      Works by combining all combinations of possibly correctly
 -      guessed cards, and all combinations of cards which weren't
 -      guessed but are consistent with the feedback provided.
 ---------------------------------------------------------------------
-}
answersFor :: Response -> [Card] -> [[Card]]
-- Response: Correct, Low ranks, correct Ranks, high Ranks, correct Suits
-- Guess CardS
answersFor ( c, l, r, h, s ) gcs
    = let
        correct = combinations c gcs
        consistent = filter (isNubWith gcs) $ spaceMerge Card ss rs

        {- Suit and rank combinations consistent with feedback.
         - Found by combining all parts of the belief spaces in the
         - correct quantities. Use permNFrom for suits because we
         - need permutations of either either suits or ranks, and
         - there are usually fewer suit permutations if the sizes are
         - large enough to matter.
         -}
        ss = nub $ permNFrom rCombinations -- SuitS
                             [ ( s - c, gss )
                             , ( n - c, allSuits \\ gss ) ]
        -- Not sure if should nub:
        rs = nub $ takeNFrom rCombinations -- RankS
                             [ ( l            , lowRanks )
                             , ( r - c        , grs )
                             , ( h            , highRanks )
                             , ( n - l - r - h, midRanks \\ grs ) ]

        {- Various parts of the belief space. -}
        allSuits  = concatRep (n - c)         kALL_SUITS
        midRanks  = concatRep (n - c - l - h) [minR .. maxR]
        highRanks = concatRep h               [succ maxR .. kMAX_RANK]
        lowRanks  = concatRep l               [kMIN_RANK .. pred minR]

        {- Parameters for constructing the belief space. -}
        minR = minimum grs
        maxR = maximum grs
        gss = map getSuit gcs -- Guessed SuitS
        grs = map getRank gcs -- Guessed RankS
        n = length gcs
    in
        nub [ a ++ b | a <- correct, b <- consistent ]

{---------------------------------------------------------------------
 -  spaceMerge
 -      Merges the belief spaces using the supplied function to merge
 -      individual hands.  All possible combinations of one element
 -      from each belief space will be included in the returned
 -      belief space.
 ---------------------------------------------------------------------
-}
spaceMerge :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
spaceMerge mf as bs = [zipWith mf a b | a <- as, b <- bs]

{---------------------------------------------------------------------
 -  nubSpaceMerge
 -      Runs spaceMerge with the supplied arguments, then filters out
 -      any hands which aren't nubs (eliminating duplicate cards).
 ---------------------------------------------------------------------
-}
nubSpaceMerge :: Eq c => (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
nubSpaceMerge mf ahs bhs = filter isNub $ spaceMerge mf ahs bhs

{---------------------------------------------------------------------
 -  isNub
 -      Returns True if the list is a nub (has no repeated elements),
 -      otherwise False
 ---------------------------------------------------------------------
-}
isNub :: Eq t => [t] -> Bool
isNub a = a == nub a

{---------------------------------------------------------------------
 -  isNubWith
 -      Returns True if the lists, when concatenated, are a nub.
 ---------------------------------------------------------------------
-}
isNubWith :: Eq t => [t] -> [t] -> Bool
isNubWith a b = isNub $ a ++ b


{---------------------------------------------------------------------
 -  useHistory
 -      hs :: [[Card]]
 -          The initial belief space.
 -      ( g, r ) :: ( [Card], Response )
 -          The guess and response pair with which to trim the belief
 -          space.
 -
 -      Returns the trimmed belief space after generating a state for
 -      the supplied guess and belief space, then resolving it with
 -      the supplied response.
 ---------------------------------------------------------------------
-}
useHistory :: [[Card]] -> ( [Card], Response ) -> [[Card]]
useHistory hs ( g, r ) = resolveState r $ createState response g hs


{---------------------------------------------------------------------
 -
 -  XResponse
 -
 -  Aliases for the Int Tuples which form the responses.
 -
 -  Response and CResponse are as specified in the project spec.
 -  SResponse and RResponse contain only the information pertaining
 -  to suits and ranks respectively, in the same order as in the
 -  project spec.
 -
----------------------------------------------------------------------
-}
type Response  = ( Int, Int, Int, Int, Int )
type CResponse = Response
type SResponse = Int
type RResponse = ( Int, Int, Int )

{---------------------------------------------------------------------
 -  response
 -      ug :: [Card]
 -          The guess, assumed to be unsorted.
 -      ua :: [Card]
 -          The answer, also assumed to be unsorted.
 -
 -      Generates a response for the supplied guess and answer.
 -      The response is of the form specified in the project spec.
 ---------------------------------------------------------------------
-}
response :: [Card] -> [Card] -> Response
response ug ua = ( c, l, r, h, s )
    where
        c = nSame a g
        l = length $ filter (< minimum gr) ar
        r = nSame ar gr
        h = length $ filter (> maximum gr) ar
        s = nSame as gs

        g = sort ug
        a = sort ua
        gr = sort $ map getRank ug
        ar = sort $ map getRank ua
        gs = map getSuit g
        as = map getSuit a

{---------------------------------------------------------------------
 -  sResponse
 -      ug :: [Suit]
 -          The guess, assumed to be unsorted.
 -      ua :: [Suit]
 -          The answer, also assumed to be unsorted.
 -
 -      Generates a response for the supplied guess and answer.
 -      The response is the part of the response specified in the
 -      project spec which pertains to suits.
 ---------------------------------------------------------------------
-}
sResponse :: [Suit] -> [Suit] -> SResponse
sResponse ug ua = s
    where
        s = nSame as gs
        as = sort ua
        gs = sort ug

{---------------------------------------------------------------------
 -  rResponse
 -      ug :: [Rank]
 -          The guess, assumed to be unsorted.
 -      ua :: [Rank]
 -          The answer, also assumed to be unsorted.
 -
 -      Generates a response for the supplied guess and answer.
 -      The response is the part of the response specified in the
 -      project spec which pertains to ranks.
 ---------------------------------------------------------------------
-}
rResponse :: [Rank] -> [Rank] -> RResponse
rResponse ug ua = ( l, r, h )
    where
        l = length $ filter (< minimum gr) ar
        r = nSame ar gr
        h = length $ filter (> maximum gr) ar

        gr = sort ug
        ar = sort ua

{---------------------------------------------------------------------
 -  nSame
 -      as :: [a]
 -          The first list, __assumed to be sorted__.
 -      bs :: [b]
 -          The second list, __also assumed to be sorted__.
 -
 -      Returns the number of elements of as which also appear in bs.
 -      as and bs are ASSUMED TO BE SORTED.
 ---------------------------------------------------------------------
-}
nSame :: Ord a => [a] -> [a] -> Int
nSame [] _ = 0
nSame _ [] = 0
nSame as@(a:at) gs@(g:gt)
    | a == g = 1 + nSame at gt
    | a <  g =     nSame at gs
    | a >  g =     nSame as gt

{---------------------------------------------------------------------
 -  splitCard
 -      Returns a tuple of the suit and rank characterising the card.
 ---------------------------------------------------------------------
-}
splitCard :: Card -> ( Suit, Rank )
splitCard (Card s r) = ( s, r )

{---------------------------------------------------------------------
 -  getRank
 -      Returns the card's rank.
 ---------------------------------------------------------------------
-}
getRank :: Card -> Rank
getRank (Card _ r) = r

{---------------------------------------------------------------------
 -  getSuit
 -      Retirms the card's suit.
 ---------------------------------------------------------------------
-}
getSuit :: Card -> Suit
getSuit (Card s _) = s

