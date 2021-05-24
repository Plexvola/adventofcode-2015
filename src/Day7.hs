{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.List.Split
import Data.Bits
import Data.Char
import Data.Word

example :: [String]
example = ["123 -> x", "456 -> y", "x AND y -> d", "x OR y -> e", "x LSHIFT 2 -> f", "y RSHIFT 2 -> g", "NOT x -> h", "NOT y -> i"]

tupleWire :: String -> (String, String)
tupleWire w = (head $ splitOn " -> " w, last $ splitOn " -> " w)

wires :: [String] -> [(String, String)]
wires = map tupleWire

checkGate :: String -> String -> Bool
checkGate g = (/=1) . length . splitOn g

wireSignal :: [(String, String)] -> String -> Word16
wireSignal ws o
    | checkGate "AND" o    =
      wireSignal ws (head $ splitOn " AND " o) .&. wireSignal ws (last $ splitOn " AND " o)
    | checkGate "OR" o     =
      wireSignal ws (head $ splitOn " OR " o) .|. wireSignal ws (last $ splitOn " OR " o)
    | checkGate "NOT" o    = complement $ wireSignal ws (last $ splitOn "NOT " o)
    | checkGate "LSHIFT" o =
      shiftL (wireSignal ws (head $ splitOn " LSHIFT " o)) (fromIntegral $ wireSignal ws (last $ splitOn " LSHIFT " o))
    | checkGate "RSHIFT" o =
      shiftR (wireSignal ws (head $ splitOn " RSHIFT " o)) (fromIntegral $ wireSignal ws (last $ splitOn " RSHIFT " o))
    | all isDigit o        = read o
    | otherwise            = wireSignal ws . fst . head . filter ((==o).snd) $ ws

runDay7 = do
  fn <- readFile "data/day7_input.txt"
  let wireArray = wires $ lines fn
  let signalWireA = wireSignal wireArray "a"
  print signalWireA
