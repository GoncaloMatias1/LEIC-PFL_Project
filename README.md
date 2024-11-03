# Group members

- Gonçalo de Abreu Matias - 50% -> cities, areAdjacent, adjacent, isStronglyConnected, travelSales
- Gonçalo Guedes da Conceição (up202206456) - 50% -> distance, pathDistance, rome, shortestPath, auxiliar function sumDistance

# shortestPath

We use the dijkstra algorithm to calculate the minimum distance between the source and all other cities, and store the information in a Data.Array.Array Int (Distance, [City]), where Distance is minimum distance between the source and the city and [City] is a list of all predecessor cities.

We chose to use an array because, if we assume all cities are ordered numbers starting in 0, it allows us to access its information in O(1) time if we use the city name to access a a position in the array.

Then we use the function makingPaths to create the shortest paths between the origin and our destination with the information in the array.

# travelSales
