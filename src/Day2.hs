module Day2 where

import Data.Text hiding (map, lines, minimum, head, tail)
import Data.List(sort)

-- 2*l*w + 2*w*h + 2*h*l

data Rectangle =
    Rectangle { l :: Int
              , w :: Int
              , h :: Int
              }

listToRectangle :: [String] -> Rectangle
listToRectangle (l:w:h:_) = Rectangle (read l) (read w) (read h)

rectangleToList :: Rectangle -> [Int]
rectangleToList r = [l r, w r, h r]

getPresentsDim :: String -> [Rectangle]
getPresentsDim = map (listToRectangle . (map unpack . split (=='x')) . pack) . lines

rectangleToWrap :: Rectangle -> Int
rectangleToWrap r = sum (map (*2) [l r*w r, w r*h r, h r*l r]) + minimum [l r*w r, w r*h r, h r*l r]

getWrapLength :: String -> Int
getWrapLength = sum . map rectangleToWrap . getPresentsDim

ribbonToWrap :: Rectangle -> Int
ribbonToWrap r = 2 * (minimum . rectangleToList) r + 2 * (head . tail . sort . rectangleToList) r + (product . rectangleToList) r

getRibbonLength :: String -> Int
getRibbonLength = sum . map ribbonToWrap . getPresentsDim


runDay2 = do
    fn <- readFile "data/day2_input.txt"
    let wrap_l = getWrapLength fn
    let ribbon_l = getRibbonLength fn
    print wrap_l
    print ribbon_l