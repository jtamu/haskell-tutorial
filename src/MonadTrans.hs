{-# LANGUAGE BlockArguments #-}

import Control.Monad (forM_)
import Control.Monad.State (MonadState (get), MonadTrans (lift), execState, execStateT, modify)

main :: IO ()
main = do
  print =<< sum' [1 .. 5]

sum' :: (Foldable t, Num a, Show a) => t a -> IO a
sum' xs = (`execStateT` 0) do
  forM_ xs $ \i -> do
    modify (+ i)
    v <- get
    lift $ putStrLn $ "+" ++ show i ++ " -> " ++ show v
