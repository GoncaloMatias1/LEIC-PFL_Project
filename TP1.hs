import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

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


-- Returns the list of cities that are adjacent to the given city in the RoadMap.
-- A city is adjacent to another city if there is a direct road between them.
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = [(c2, d) | (c1, c2, d) <- roadMap, c1 == city] ++
                       [(c1, d) | (c1, c2, d) <- roadMap, c2 == city]

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

-- Auxiliar function that is used in rome
-- In a list with tuples where each tuple contains the city and the number of adjacent cities, it only keeps the ones with more connections
selectRomeCities :: [(City, Int)] -> [(City, Int)]
selectRomeCities [] = [("-1", 0)]
selectRomeCities ((city, numAdjCities):cts)
    | numAdjCities > snd (head z) = [(city, numAdjCities)]
    | numAdjCities == snd (head z) = (city, numAdjCities) : z
    | otherwise = z
    where z = selectRomeCities cts


-- Returns the cities with more adjacent cities, or empty if the roadMap is empty
rome :: RoadMap -> [City]
rome [] = []
rome roadMap = map fst (selectRomeCities [(city, length (adjacent roadMap city)) | city <- cities roadMap])

-- Returns True if every city in the RoadMap can reach every other city, False otherwise
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True
isStronglyConnected roadMap = all (\city1 -> all (\city2 -> canReach city1 city2 []) uniqueCities) uniqueCities
    where
        uniqueCities = Data.List.nub [city | (c1, c2, _) <- roadMap, city <- [c1, c2]]
        canReach start end visited
            | start == end = True
            | start `elem` visited = False
            | otherwise = any (\nextCity -> canReach nextCity end (start:visited)) nextCities
            where
                nextCities = [c2 | (c1, c2, _) <- roadMap, c1 == start] ++
                            [c1 | (c1, c2, _) <- roadMap, c2 == start]


-- Auxiliar function to sort priority queue
sortQueue :: Data.Array.Array Int (Distance, [City]) -> City -> City -> Ordering
sortQueue cityInfo c1 c2
    | fst (cityInfo Data.Array.! (read c1 :: Int)) > fst (cityInfo Data.Array.! (read c2 :: Int)) = GT
    | fst (cityInfo Data.Array.! (read c1 :: Int)) < fst (cityInfo Data.Array.! (read c2 :: Int)) = LT
    | otherwise = EQ

-- Auxiliar function to relax edges
changeCityInfo :: Data.Array.Array Int (Distance, [City]) -> City -> [(City,Distance)] -> Data.Array.Array Int (Distance, [City])
changeCityInfo cityInfo _ [] = cityInfo
changeCityInfo cityInfo city ((c, distance):adj)
    | fst (cityInfo Data.Array.! (read c :: Int)) > newDistance = changeCityInfo (cityInfo Data.Array.// [(read c :: Int, (newDistance, [city]))]) city adj
    | fst (cityInfo Data.Array.! (read c :: Int)) == newDistance = changeCityInfo (cityInfo Data.Array.// [(read c :: Int, (newDistance, city : snd (cityInfo Data.Array.! (read c :: Int))))]) city adj
    | otherwise = changeCityInfo cityInfo city adj
    where newDistance = fst (cityInfo Data.Array.! (read city :: Int)) + distance

-- Auxiliar function that executes dijkstra algorithm
dijkstra :: RoadMap -> Data.Array.Array Int (Distance, [City]) -> [City] -> Data.Array.Array Int (Distance, [City])
dijkstra _ cityInfo [] = cityInfo
dijkstra roadMap cityInfo pQueue
    | fst (cityInfo Data.Array.! (read (head pQueue) :: Int)) == (maxBound :: Int) = cityInfo
    | otherwise = dijkstra roadMap newCityInfo (Data.List.sortBy (sortQueue newCityInfo) (tail pQueue))
    where newCityInfo = changeCityInfo cityInfo (head pQueue) (adjacent roadMap (head pQueue))

-- Auxiliar function that adds a city to a path
addCityToPath :: [Path] -> City -> [Path]
addCityToPath [path] city = [city : path]

-- Auxiliar function that calls makingPaths for a list of cities
makingPathsAux :: City -> [City] -> Data.Array.Array Int (Distance, [City]) -> [Path] -> [Path]
makingPathsAux orig [dest] cityInfo path = makingPaths orig dest cityInfo (addCityToPath path dest)
makingPathsAux orig (dest:ds) cityInfo path = makingPaths orig dest cityInfo (addCityToPath path dest) ++ makingPathsAux orig ds cityInfo path

-- Auxiliar function that creates a list of paths from cityInfo
makingPaths :: City -> City -> Data.Array.Array Int (Distance, [City]) -> [Path] -> [Path]
makingPaths orig dest cityInfo path
    | orig == dest = path
    | null predCities = []
    | length predCities > 1 = makingPathsAux orig predCities cityInfo path
    | otherwise = makingPaths orig (head predCities) cityInfo (addCityToPath path (head predCities))
    where predCities = snd (cityInfo Data.Array.! (read dest :: Int))


-- Returns a list with all the shortest paths between 2 cities
-- The list is empty if there i no path or contains only the city itself if the origin and destination city are the same
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap orig dest
    | orig == dest = [[orig]]
    | otherwise = makingPaths orig dest (dijkstra roadMap cityInfo (Data.List.sortBy (sortQueue cityInfo) (cities roadMap))) [[dest]]
    where cityInfo = Data.Array.array (0, length (cities roadMap) - 1) (map (, (maxBound :: Int, [])) [0 .. length (cities roadMap) - 1]) Data.Array.// [(read orig :: Int, (0, []))]

-- Convert RoadMap to adjacency matrix
createAdjMatrix :: RoadMap -> [City] -> Data.Array.Array (Int, Int) (Maybe Distance)
createAdjMatrix roadMap cityList = 
    Data.Array.array ((0,0), (n-1,n-1)) 
        [((i,j), getDist i j) | i <- [0..n-1], j <- [0..n-1]]
    where
        n = length cityList
        getDist i j 
            | i == j = Just 0
            | otherwise = distance roadMap (cityList !! i) (cityList !! j)

-- Helper function to get city index in the list
getCityIndex :: [City] -> City -> Int
getCityIndex cities city = case Data.List.elemIndex city cities of
    Just idx -> idx
    Nothing -> error "City not found"

-- Solve TSP using dynamic programming
solveTSP :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe Distance) -> Int -> (Maybe Distance, [Int])
solveTSP pos mask adjMatrix n
    | mask == (2^n - 1) = 
        case adjMatrix Data.Array.! (pos, 0) of  -- Return to start
            Nothing -> (Nothing, [])
            Just d -> (Just d, [pos, 0])
    | otherwise = 
        let possibilities = [(nextCity, totalCost, nextPath) | 
                           nextCity <- [0..n-1],
                           not (Data.Bits.testBit mask nextCity),
                           let newMask = Data.Bits.setBit mask nextCity,
                           let (subCost, subPath) = solveTSP nextCity newMask adjMatrix n,
                           let totalCost = case (adjMatrix Data.Array.! (pos, nextCity), subCost) of
                                           (Just d1, Just d2) -> Just (d1 + d2)
                                           _ -> Nothing,
                           totalCost /= Nothing,
                           let nextPath = subPath]
        in case possibilities of
            [] -> (Nothing, [])
            ps -> let (nextCity, bestCost, bestPath) = 
                        Data.List.minimumBy (\(_, Just c1, _) (_, Just c2, _) -> compare c1 c2) ps
                 in (bestCost, pos : bestPath)

-- Main TSP function
travelSales :: RoadMap -> Path
travelSales [] = []
travelSales roadMap
    | not (isStronglyConnected roadMap) = []
    | otherwise = 
        let cityList = cities roadMap
            n = length cityList
            adjMatrix = createAdjMatrix roadMap cityList
            (minDist, path) = solveTSP 0 1 adjMatrix n  -- Start from city 0 with only itself visited
        in case minDist of
            Nothing -> []
            Just _ -> map (cityList !!) path

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]