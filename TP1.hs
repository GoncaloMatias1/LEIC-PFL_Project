import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

-- Auxiliar function to sum 2 values of type Maybe Distance
-- Returns Nothing if one of the values is Nothing or Just Distance
sumDistance :: Maybe Distance -> Maybe Distance -> Maybe Distance
sumDistance _ Nothing = Nothing
sumDistance Nothing _ = Nothing
sumDistance (Just x) (Just y) = Just (x + y)


type RoadMap = [(City,City,Distance)]

-- Returns all unique cities in the RoadMap.
-- Each city appears only once in the output list.
cities :: RoadMap -> [City]
cities roadMap = Data.List.nub [city | (city1, city2, _) <- roadMap, city <- [city1, city2]]

-- Returns True if the two cities are adjacent in the RoadMap.
-- Two cities are adjacent if there is a direct road between them.
-- This work because we have access to the tests of the cities in the roadMap.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

-- Returns the distance between 2 cities
-- The distance is Just 0 if the 2 cities are the same, Nothing if they are not connected, or Just n where n is the distance
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):rms) city1 city2
    | city1 == city2 = Just 0
    | (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1) = Just d
    | otherwise = distance rms city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

-- Auxiliar function that is used in pathDistance
-- Returns the sum of currentDistance and the distance between city1 and city2
pathDistanceAux :: (RoadMap, City, City) -> Maybe Distance -> Maybe Distance
pathDistanceAux (roadMap, city1, city2) currentDistance = sumDistance currentDistance (distance roadMap city1 city2)

-- Returns the total distance between cities in a path
-- The distance is Just 0 if there is only 1 city, Nothing if at least 2 cities are not connected, or Just n where n is the total distance
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing
pathDistance _ [city] = Just 0
pathDistance roadMap (p:ps) = foldr pathDistanceAux (Just 0) [(roadMap, city1, city2) | (city1, city2) <- zip (p:ps) ps]

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]