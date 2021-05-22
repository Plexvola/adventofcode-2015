module Day3 where

import Data.List(nub)
import Data.Bifunctor(bimap)

type Coordinates = (Int, Int)

directionsToCoordinates :: Coordinates -> String -> [Coordinates]
directionsToCoordinates c (d:ds)
    | d == '^'  = c : directionsToCoordinates (fst c + 1, snd c) ds
    | d == '>'  = c : directionsToCoordinates (fst c, snd c + 1) ds
    | d == 'v'  = c : directionsToCoordinates (fst c - 1, snd c) ds
    | d == '<'  = c : directionsToCoordinates (fst c, snd c - 1) ds
directionsToCoordinates c [] = [c]

deliveredHouses :: String -> Int
deliveredHouses = length . nub . directionsToCoordinates (0, 0)

getBothSantaPaths :: String -> (String, String)
getBothSantaPaths (d1:d2:ds) = bimap (d1 :) (d2 :) (getBothSantaPaths ds)
getBothSantaPaths ds         = ("", "")

robotDeliveredHouses :: String -> Int
robotDeliveredHouses s = length . nub $ (directionsToCoordinates (0, 0) . fst . getBothSantaPaths $ s)
                                     ++ (directionsToCoordinates (0, 0) . snd . getBothSantaPaths $ s)

runDay3 = do
    fn <- readFile "data/day3_input.txt"
    let d_houses = deliveredHouses fn
    let r_d_houses = robotDeliveredHouses fn
    print d_houses
    print r_d_houses
