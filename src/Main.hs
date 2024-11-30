{-# OPTIONS -Wall -Werror #-}

import Shape

main :: IO ()
main = do
  print $ nudge (baseCircle 30) 10 20
