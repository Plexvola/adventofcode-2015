module Day5 where

hasVowels :: String -> Bool
hasVowels s = (length . filter (`elem` "aeiou")) s >= 3

hasDoubleLetter :: String -> Bool
hasDoubleLetter (c1:c2:s) = c1 == c2 || hasDoubleLetter (c2:s)
hasDoubleLetter _ = False

doesNotHaveAny :: String -> Bool
doesNotHaveAny (c1:c2:s) = notElem (c1:c2:"") ["ab", "cd", "pq", "xy"] && doesNotHaveAny (c2:s)
doesNotHaveAny _ = True

isNiceString :: String -> Bool
isNiceString s = hasVowels s && hasDoubleLetter s && doesNotHaveAny s

hasPairOfLetters :: (Char, Char) -> String -> Bool
hasPairOfLetters ch (c1:c2:s)   = c1 == fst ch && c2 == snd ch || hasPairOfLetters ch (c2:s)
hasPairOfLetters _ _            = False

hasTwoPairs :: String -> Bool
hasTwoPairs (c1:c2:cs)  = hasPairOfLetters (c1, c2) cs || hasTwoPairs (c2:cs)
hasTwoPairs _           = False

hasSpacedCoupleLetter :: String -> Bool
hasSpacedCoupleLetter (c1:c2:c3:s)  = (c1 == c3) || hasSpacedCoupleLetter (c2:c3:s)
hasSpacedCoupleLetter _             = False

isNiceString' :: String -> Bool
isNiceString' s = hasTwoPairs s && hasSpacedCoupleLetter s

runDay5 = do
    fn <- readFile "data/day5_input.txt"
    let nb_nice_strings = length . filter isNiceString $ lines fn
    print nb_nice_strings
    let nb_nice_strings_2 = length . filter isNiceString' $ lines fn
    print nb_nice_strings_2
