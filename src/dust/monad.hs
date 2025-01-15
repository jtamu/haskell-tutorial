import Control.Monad
import Control.Monad.Writer

sevensOnly = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x

three = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multiWithLog = logNumber 3 >>= \x -> logNumber 5 >>= \y -> return (x * y)

-- multiWithLog = do
--   x <- logNumber 3
--   y <- logNumber 5
--   tell ["Result of multiplication is: " ++ show (x * y)]
--   return (x * y)
