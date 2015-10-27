{-
 -  File     : Response.hs
 -  Author   : Rongduan Zhu
 -  Purpose  : Provides feedback on guesses for Cardguess.hs.
 -
 - Taken from https://github.com/rongduan-zhu/dpOne on 2015-10-27.
 -
 -}

module Response (response) where

import Data.List
import Card

response :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
response answer guess = (c, l, r, u, s)
    where
    c = getOccurances guess answer
    r = getOccurancesR guess answer
    s = getOccurancesS guess answer
    l = length $ filter
        (\x -> x < minimum (card2Rank guess)) (card2Rank answer)
    u = length $ filter
        (\x -> x > maximum (card2Rank guess)) (card2Rank answer)
getOccurances :: Eq a => [a] -> [a] -> Int
getOccurances guess answer
    = length answer - length (removeOccurances guess answer)

getOccurancesR :: [Card] -> [Card] -> Int
getOccurancesR guess answer =
    getOccurances (card2Rank guess) (card2Rank answer)

getOccurancesS :: [Card] -> [Card] -> Int
getOccurancesS guess answer =
    getOccurances (card2Suit guess) (card2Suit answer)

removeOccurances :: Eq a => [a] -> [a] -> [a]
removeOccurances (x:xs) cards = removeOccurances xs (delete x cards)
removeOccurances [] cards = cards

card2Rank :: [Card] -> [Rank]
card2Rank cards = map rank cards

card2Suit :: [Card] -> [Suit]
card2Suit cards = map suit cards
