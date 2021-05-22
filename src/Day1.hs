module Day1 where

countParenLevel :: [Char] -> Int
countParenLevel parens = length (filter (=='(') parens) - length (filter (==')') parens)

countFirstBasement :: [Char] -> Int
countFirstBasement ps = countBasementLevel ps (countParenLevel ps)

countBasementLevel :: [Char] -> Int -> Int
countBasementLevel (p:ps) x
     | countParenLevel (p:ps) > x   = 0
     | otherwise                = countBasementLevel ps x + 1

runDay1 = do
  fn <- readFile "data/day1_input.txt"
  let level_p = countParenLevel fn
  let f_bas_p = countFirstBasement fn
  print level_p
  print f_bas_p
