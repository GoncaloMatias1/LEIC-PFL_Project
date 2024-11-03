# Group members

- Gonçalo de Abreu Matias (up202108703) - 50% -> cities, areAdjacent, adjacent, isStronglyConnected, travelSales
- Gonçalo Guedes da Conceição (up202206456) - 50% -> distance, pathDistance, rome, shortestPath, auxiliar function sumDistance

# shortestPath

We use the dijkstra algorithm to calculate the minimum distance between the source and all other cities, and store the information in a Data.Array.Array Int (Distance, [City]), where Distance is minimum distance between the source and the city and [City] is a list of all predecessor cities.

We chose to use an array because, if we assume all cities are ordered numbers starting in 0, it allows us to access its information in O(1) time if we use the city name to access a a position in the array.

Then we use the function makingPaths to create the shortest paths between the origin and our destination with the information in the array.

# TravelSales Function

This function takes a RoadMap and returns a TSP path (travelSales :: RoadMap -> Path) when it exists. 

We start by verifying if the algorithm receives an empty list. If it does, it returns that same empty list. 

As asked by the project requirements, we were advised to use dynamic programming to solve this function. This way we divide the big problem into smaller ones, making it easier to solve. The function uses recursion when each city is visited.

We also created a helper function createAdjMatrix to construct an adjacency matrix, so that we can more easily see the distances between the pair of cities. This is how the function works:
- when two cities are connected, the distance is stored
- when two cities are not connected, nothing is hold. 

The second helper function is getCityIndex, and we created it find a determined city index. This way we can reference cities in the adjacency matrix. This function also produces an error when a city is not found in the list. 

For solveTSP function, we use a mask to indicate which cities were already visited. We also have to return to the starting city. We would say the base case for the recursion step is to check if there is a direct path to the start city. When there is, the function returns that path, otherwise it won't return anything. 

In the main function travelSales, when we find the minimum distance and path in the previous function, travelSales reconstructs the path. The list we get represents the order cities in the tsp solution. This function also checks and handles incomplete graphs, for example it ensures all cities can be reached from any other city. An empty list is returned if no valid solution exists. 