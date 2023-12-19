# Maze-Solving Bot - README

## Introduction

Welcome to the Maze-Solving Bot project, where we explore the potential of intelligent bots in navigating through mazes. The primary objective is to develop a bot capable of systematically searching each tile in a maze, considering priority values and heuristic values. The chosen algorithm, Greedy Best First Search, guides the bot's decision-making process by estimating the distance to the goal for each tile.

## Algorithm and Data Structure

The implemented algorithm, Greedy Best First Search, excels in efficiently searching through a large space of possible paths. It calculates the estimated distance from the current tile to the goal tile, prioritizing tiles with lower heuristic values. The Manhattan distance serves as a heuristic, estimating the cost of reaching the goal from each location. The use of Minimum Heap (Min Heap) as a data structure further optimizes the search process, storing the lowest priority node in its root node.

## Program

- **Language:** JavaScript
- **Execution Steps:**
  1. Download the zip file.
  2. Extract the zip file.
  3. Run a web server for the project to run and open the project directory.
  4. Use Visual Studio Code with the Live Server extension for a seamless experience.

## Test Cases

Explore the bot's capabilities through the following test cases (text files in the zip file):

1. **TestCase1 (maze.txt):** Demonstrates the bot's inability to find a path to the goal.
2. **TestCase2 (maze1.txt):** Successfully navigates a 20x20 maze.
3. **TestCase3 (maze2.txt):** Handles a 30x30 maze with more obstacles effectively.
4. **TestCase4 (maze5.txt):** Navigates a large 50x50 maze with ease.
5. **TestCase5 (maze4.txt):** Takes an alternative route before reaching the goal.
6. **TestCase6 (maze5.txt):** Finds the goal after initial exploration failure.

## Results and Analysis

The bot excels in finding the shortest path in mazes, solving simple and moderately complex mazes efficiently. It adapts well to changing maze environments, although it may struggle with highly complex and convoluted paths. The performance may vary based on the size, resolution, and obstacles in the maze.

## Recommendations

1. Consider more advanced algorithms (e.g., A* or Dijkstra's) for complex mazes.
2. Utilize heuristics and machine learning techniques to enhance efficiency.
3. Increase computational resources for improved performance.
4. Implement a robust obstacle detection and avoidance system.
5. Carefully analyze specific problem requirements when choosing algorithms.

## References

- Difference Between Machine Learning, Artificial Intelligence, And Bots. [Link](https://www.zymr.com/blog/difference-machine-learning-artificial-intelligence-bots#:~:text=This%20is%20the%20basic%20idea,intelligence%20to%20carry%20out%20tasks.)
- Greedy Best First Search algorithm. [Link](https://www.geeksforgeeks.org/greedy-best-first-search-algorithm/)
- Heap Data Structures. [Link](https://www.tutorialspoint.com/data_structures_algorithms/heap_data_structure.htm)
- Informed Search Algorithms in AI. [Link](https://www.javatpoint.com/ai-informed-search-algorithms)
- Munoz, D. G., Bouchereau, F., Vargas-Rosales, C., & Enriquez, R. (2009). Heuristic Approaches to the Position Location Problem. [Link](https://doi.org/10.1016/b978-0-12-374353-4.00010-7)
- How to build an Artificial Intelligence Chatbot? Intellipaat Blog. [Link](https://intellipaat.com/blog/how-to-build-an-artificial-intelligence-chatbot/)
