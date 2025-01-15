import Control.Monad (foldM)
{-# OPTIONS -Wall -Werror #-}

solveRPN :: String -> Maybe Double
solveRPN str = do
  [res] <- foldM foldingFunction [] (words str)
  return res

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "+" = Just $ (y + x) : ys
foldingFunction (x : y : ys) "-" = Just $ (y - x) : ys
foldingFunction (x : y : ys) "*" = Just $ (y * x) : ys
foldingFunction (x : y : ys) "/" = Just $ (y / x) : ys
-- fmap :: (Double -> [Double]) Maybe Double -> Maybe [Double]
foldingFunction xs numberStr = fmap (:xs) (readMaybe numberStr)

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = let r = reads str in
  case r of
    [(a, "")] -> Just a
    _ -> Nothing
