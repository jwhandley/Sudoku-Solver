## Sudoku-Solver

A very simple Sudoku solver written in Scala.
Uses a depth-first search algorithm written in a functional style.

For example, the program solves this hard puzzle in approximately 1.5ms (after one warmup iteration):

```
+-------+-------+-------+  
| 1     |     6 | 5     | 
|       |   5 9 |       | 
| 8     | 4     |     6 | 
+-------+-------+-------+ 
| 2     | 7     |     4 | 
|     1 | 6     |   3   | 
|       |       |       | 
+-------+-------+-------+ 
|     7 | 1     |     2 | 
|     9 |       |     5 | 
|     3 | 9 7   | 4     | 
+-------+-------+-------+ 
```

Solution: 
```
+-------+-------+-------+
| 1 9 2 | 8 3 6 | 5 4 7 |
| 7 4 6 | 2 5 9 | 8 1 3 |
| 8 3 5 | 4 1 7 | 9 2 6 |
+-------+-------+-------+
| 2 6 8 | 7 9 3 | 1 5 4 |
| 9 5 1 | 6 4 2 | 7 3 8 |
| 3 7 4 | 5 8 1 | 2 6 9 |
+-------+-------+-------+
| 5 8 7 | 1 6 4 | 3 9 2 |
| 4 1 9 | 3 2 8 | 6 7 5 |
| 6 2 3 | 9 7 5 | 4 8 1 |
+-------+-------+-------+
```