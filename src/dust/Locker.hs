{-# OPTIONS -Wall -Werror #-}

import Data.Map qualified as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- LockerMapのダミーデータ
lockers :: LockerMap
lockers = Map.fromList [
    (100, (Taken, "ZD39I")),
    (101, (Free, "JAH3I"))
    ]

