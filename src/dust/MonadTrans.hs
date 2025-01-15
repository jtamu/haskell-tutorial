module MonadTrans where

{-# LANGUAGE OverloadedStrings #-}

import Data.Text

main :: IO ()
main = do
    print $ splitOn "@" ""
    print $ splitOn "@" "test"
    print $ splitOn "@" "test@example.com"
    print $ splitOn "@" "test@example@com"
