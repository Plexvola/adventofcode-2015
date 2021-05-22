module Day6 where

{-# LANGUAGE OverloadedStrings #-}

import Data.Vector as V hiding (map, filter, elem, foldl)
import qualified Data.Vector as V
import Data.List
import Data.List.Split

lights :: Vector (Vector Bool)
lights = V.replicate 1000 $ V.replicate 1000 False

lights' :: Vector (Vector Int)
lights' = V.replicate 1000 $ V.replicate 1000 0

pMap :: Int -> Int -> (a -> a) -> Int -> a -> a
pMap a b f i x
    | a <= i && i <= b      = f x
    | otherwise             = x

lightsMap :: (a -> a) -> (Int, Int) -> (Int, Int) -> Vector (Vector a) -> Vector (Vector a)
lightsMap f x y = imap $ pMap (fst x) (fst y) (imap $ pMap (snd x) (snd y) f)

boolStringOp :: String -> Bool -> Bool
boolStringOp s
    | "toggle" `isPrefixOf` s   = (/=True)
    | "turn on" `isPrefixOf` s  = const True
    | "turn off" `isPrefixOf` s = const False

intStringOp :: String -> Int -> Int
intStringOp s
    | "toggle" `isPrefixOf` s   = (+2)
    | "turn on" `isPrefixOf` s  = (+1)
    | "turn off" `isPrefixOf` s = \x -> if x > 0 then x-1 else x


readStringCoord :: String -> ((Int, Int),(Int, Int))
readStringCoord s = (\[x,y] -> (x,y)) $ map ((\[x,y] -> (x,y)) . map (read . filter (\x -> x `elem` ['0'..'9'])) . splitOn ",") (splitOn "through" s)

lightsInstruction :: (String -> a -> a) -> Vector (Vector a) -> String -> Vector (Vector a)
lightsInstruction fstr v s = uncurry (lightsMap (fstr s)) (readStringCoord s) v

lightCount :: Vector (Vector Bool) -> Int
lightCount = V.sum . V.map (V.length . V.filter id)

lightCount' :: Vector (Vector Int) -> Int
lightCount' = V.sum . V.map V.sum

runDay6 = do
    fn <- readFile "data/day6_input.txt"
    let instructions = lines fn
    let lightsOn = lightCount $ foldl (lightsInstruction boolStringOp) lights instructions
    print lightsOn
    let lightsOn' = lightCount' $ foldl (lightsInstruction intStringOp) lights' instructions
    print lightsOn'