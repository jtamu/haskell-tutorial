{-# OPTIONS -Wall -Werror #-}

-- [(A1, B1, C1), (A2, B2, C2)]
-- Cはオプション。A,Bはどちらかを選択する
-- Aを選択しCを選択しなかった場合、次はAを選択しなければならない
-- Bを選択しCを選択しなかった場合、次はBを選択しなければならない
-- Aを選択しCを選択した場合、次はBを選択しなければならない
-- Bを選択しCを選択した場合、次はAを選択しなければならない

data Route = A | B deriving (Show)

type PrevRouteCost = Int

type RouteCostA = Int

type RouteCostB = Int

type OptionalRouteCost = Int

type SumRouteCost = Int

calcRoute :: [([Route], PrevRouteCost)] -> (RouteCostA, RouteCostB, OptionalRouteCost) -> [([Route], SumRouteCost)]
calcRoute [] _ = []
calcRoute ((routes, prevCost) : xs) (costA, costB, optCost) = case route of
  A -> [(reverse (A : reverse routes), prevCost + costA), (reverse (B : reverse routes), prevCost + costA + optCost)] ++ calcRoute xs (costA, costB, optCost)
  B -> [(reverse (B : reverse routes), prevCost + costB), (reverse (A : reverse routes), prevCost + costB + optCost)] ++ calcRoute xs (costA, costB, optCost)
  where
    route = last routes

calcRoutes :: [(RouteCostA, RouteCostB, OptionalRouteCost)] -> [([Route], SumRouteCost)]
calcRoutes x = ptnA ++ ptnB
  where
    ptnA = foldl calcRoute [([A], 0)] x
    ptnB = foldl calcRoute [([B], 0)] x

defineOptimalRoute :: [([Route], SumRouteCost)] -> ([Route], SumRouteCost)
defineOptimalRoute = foldl1 minRoute
  where
    minRoute (r1, c1) (r2, c2) = if c1 < c2 then (r1, c1) else (r2, c2)

calcOptimalRoute :: [(RouteCostA, RouteCostB, OptionalRouteCost)] -> ([Route], SumRouteCost)
calcOptimalRoute = defineOptimalRoute . calcRoutes

groups :: [Int] -> [(Int, Int, Int)]
groups [] = []
groups [x1] = [(x1, 0, 0)]
groups [x1, x2] = [(x1, x2, 0)]
groups (x1 : x2 : x3 : xs) = (x1, x2, x3) : groups xs

main :: IO ()
main = do
  contents <- getContents
  let costs = map read $ lines contents
      routeCosts = groups costs
  print $ calcOptimalRoute routeCosts
