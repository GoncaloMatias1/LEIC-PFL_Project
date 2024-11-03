# Group members

- Gonçalo de Abreu Matias (up202108703) - 50% -> cities, areAdjacent, adjacent, isStronglyConnected, travelSales
- Gonçalo Guedes da Conceição (up202206456) - 50% -> distance, pathDistance, rome, shortestPath, auxiliar function sumDistance

# Tasks

## cities :: RoadMap -> [City]

Returns all unique cities in a roadmap

## areAdjacent :: RoadMap -> City -> City -> Bool

Returns True if the two cities are adjacent in the RoadMap and False otherwise
Two cities are adjacent if there is a direct road between them

## distance :: RoadMap -> City -> City -> Maybe Distance

Returns the distance between 2 cities if they are connected, Nothing if they are not, and Just 0 if they are the same

## adjacent :: RoadMap -> City -> [(City,Distance)]

Returns the list of cities that are adjacent to the given city in the RoadMap
A city is adjacent to another city if there is a direct road between them

## pathDistance :: RoadMap -> Path -> Maybe Distance

Returns the total distance between cities in a path
The distance is Just 0 if there is only 1 city, Nothing if at least 2 cities are not connected, or Just n where n is the total distance

## rome :: RoadMap -> [City]

Returns the cities with more adjacent cities, or empty if the roadMap is empty

## isStronglyConnected :: RoadMap -> Bool

Returns True if every city in the RoadMap can reach every other city, False otherwise

# shortestPath

We use the dijkstra algorithm to calculate the minimum distance between the source and all other cities, and store the information in a Data.Array.Array Int (Distance, [City]), where Distance is minimum distance between the source and the city and [City] is a list of all predecessor cities.

We chose to use an array because, if we assume all cities are ordered numbers starting in 0, it allows us to access its information in O(1) time if we use the city name to access a a position in the array.

Then we use the function makingPaths to create the shortest paths between the origin and our destination with the information in the array.

# travelSales
