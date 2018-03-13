## Advent of Code 2017
My solutions for [Advent of Code 2017](http://adventofcode.com/2017), using Haskell as the primary language, with C# as backup when the Haskell solution takes too long to process or runs out of memory.

Day | Title | Development Notes
--- | --- | ---
21 | [Fractal Art](./21.hs) | ASCII pattern rotations and flips.  Uses **transpose**.
20 | [Particle Swarm](./20-1.hs) | Very simple physics. Part 2 incomplete
19 | [A Series of Tubes](./19.hs) | A fun one, tracing over [a large meandering pipe](./19.input), collecting the characters seen along the way
18 | [Duet](./18-1.hs) | Model a basic CPU with opcodes.  Part 2 incomplete
17 | [Spinlock](./17-1.hs) |
16 | [Permutation Promenade](./16-1.hs) | Some more parser combinators
15 | [Dueling Generators](./15.cs) | Uses C# with a few higher order functions and completes right away. The 40 million iterations causes my Haskell attempt to run out of memory. My first use of the **yield return** feature of C#.
14 | [Disk Defragmentation](./14-1.hs) | Set operation **intersect**
13 | [Packet Scanners](./13-1.hs) |
12 | [Digital Plumber](./12-1.hs) | Set operations **union** and **\\\\**
11 | [Hex Ed](./11.hs) | Basic operations on a hex grid
10 | [Knot Hash](./10.hs) |
9 | [Stream Processing](./9.hs) | First use of parser combinators, some custom ones built upon [NanoParsec](http://dev.stephendiehl.com/fun/002_parsers.html) primitives.  First algebraic data type (Thing)
8 | [I Heard You Like Registers](./8-1.hs) |
7 | [Recursive Circus](./7-1.hs) | Uses a tree with 0 to *n* child nodes per node
6 | [Memory Reallocation](./6.hs) | Some basic vector addition
5 | [A Maze of Twisty Trampolines, All Alike](./5-1.hs) | Uses **Data.Sequence** instead of the built-in list to reduce execution time.
4 | [High-Entropy Passphrases](./4-1.hs) |
3 | [Spiral Memory](./3-1.hs) |
2 | [Corruption Checksum](./2-1.hs) |
1 | [Inverse Captcha](./1-1.hs) |

