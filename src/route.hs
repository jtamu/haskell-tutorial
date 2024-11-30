{-# OPTIONS -Wall -Werror #-}

-- [(A1, B1, C1), (A2, B2, C2)]
-- Cはオプション。A,Bはどちらかを選択する
-- Aを選択しCを選択しなかった場合、次はAを選択しなければならない
-- Bを選択しCを選択しなかった場合、次はBを選択しなければならない
-- Aを選択しCを選択した場合、次はBを選択しなければならない
-- Bを選択しCを選択した場合、次はAを選択しなければならない

data Route = A | B deriving (Show)

type RouteCostA = Int

type RouteCostB = Int

type OptionalRouteCost = Int

type SumRouteCost = Int

data RouteInfo = RouteInfo [Route] SumRouteCost deriving (Show)

instance Eq RouteInfo where
  (==) (RouteInfo _ s1) (RouteInfo _ s2) = s1 == s2

instance Ord RouteInfo where
  compare (RouteInfo _ s1) (RouteInfo _ s2) = compare s1 s2

calcRoute :: [RouteInfo] -> (RouteCostA, RouteCostB, OptionalRouteCost) -> [RouteInfo]
calcRoute [] _ = []
calcRoute (RouteInfo routes prevCost : xs) (costA, costB, optCost) = case route of
  A -> [aA, aB] ++ calcRoute xs (costA, costB, optCost)
  B -> [bA, bB] ++ calcRoute xs (costA, costB, optCost)
  where
    route = last routes
    aA = RouteInfo (reverse (A : reverse routes)) (prevCost + costA)
    aB = RouteInfo (reverse (B : reverse routes)) (prevCost + costA + optCost)
    bA = RouteInfo (reverse (B : reverse routes)) (prevCost + costB)
    bB = RouteInfo (reverse (A : reverse routes)) (prevCost + costB + optCost)

calcRoutes :: [(RouteCostA, RouteCostB, OptionalRouteCost)] -> [RouteInfo]
calcRoutes x = ptnA ++ ptnB
  where
    ptnA = foldl calcRoute [RouteInfo [A] 0] x
    ptnB = foldl calcRoute [RouteInfo [B] 0] x

calcOptimalRoute :: [(RouteCostA, RouteCostB, OptionalRouteCost)] -> RouteInfo
calcOptimalRoute = minimum . calcRoutes

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
