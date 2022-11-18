Some notes:

-   Download the [template zip file](https://chalmers.instructure.com/courses/20991/files/2353434?wrap=1 "Lab3.zip") [Download template zip file](https://chalmers.instructure.com/courses/20991/files/2353434/download?download_frd=1) for your lab work. Do not change the name of the Sudoku.hs file therein, or any of the declarations of types and function names in this file as we need them for automated testing.
-   Remember that you have to work in pairs. **Only groups of size 2 are allowed to submit!** Submissions by 3 people are not permitted. Submissions by groups of 1 are only permitted with prior permission.
-   When you are done, please submit your solution using the Fire system.
-   If you are stuck on this assignment, please read the page on [Getting Help](https://chalmers.instructure.com/courses/20991/pages/getting-help "Getting Help") carefully!

Good luck!

## Lab Assignment 3: Sudoku

In this Lab Assignment, you will design a Haskell program that will be able to solve Sudokus, a popular logical puzzle originating from Japan.

### Assignments and Deadlines

There are 6 regular assignments as part of this Lab: A, B, C, D, E, and F. The lab consists (again) of two parts.

For submission, assignments A, B, C and D are called **Lab 3A**.

Assignments E and F are called **Lab 3B**.

There are also extra assignments. You can choose freely whether to do one of these. Those are just for fun.

### Hints

Some assignments have hints. Often, these involve particular standard Haskell functions that you could use. Some of these functions are defined in modules that you have to import yourself explicitly. You can use the following resources to find more information about those functions:

-   [Hoogle Links to an external site.](http://www.haskell.org/hoogle/), the library function search engine
-   [A Tour of the Haskell Prelude Links to an external site.](https://www.cse.chalmers.se/edu/year/2018/course/TDA452/tourofprelude.html) shows standard Haskell functions that you get without importing any module (or come from standard modules such as `Data.List` and `Data.Char`).
-   [Haskell Hierarchical Libraries Links to an external site.](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html), all the library modules, included with the latest version of GHC.

We encourage you to actually go and find information about the functions that are mentioned in the hints!

## Sudokus

Sudoku is a logic puzzle originating in Japan and has caught on in popularity also in the West in recent years. Many newspapers publish a daily Sudoku puzzle for the readers to solve.

A Sudoku puzzle consists of a 9x9 grid. Some of the cells in the grid have digits (from 1 to 9), others are blank. The objective of the puzzle is to fill in the blank cells with digits from 1 to 9, in such a way that every row, every column and every 3x3 block has exactly one occurrence of each digit 1 to 9.

Here is an example of a Sudoku puzzle

![example.gif](https://chalmers.instructure.com/courses/20991/files/2353410/preview)

Here is the solution to the Sudoku puzzle

![solution.gif](https://chalmers.instructure.com/courses/20991/files/2353406/preview)

In this lab assignment, you will write a Haskell program that can read in a Sudoku puzzle and solve it.

### More Information

If you want to read more about Sudokus, here are a few links:

-   The [Daily Sudoku Links to an external site.](http://www.dailysudoku.com/sudoku/index.shtml) has examples and explanations.
-   [Wikipedia on Sudoku Links to an external site.](http://en.wikipedia.org/wiki/Sudoku).
-   [sudoku.com Links to an external site.](http://www.sudoku.com/) has examples and explanations.
-   [sudoku.com.au Links to an external site.](http://sudoku.com.au/) has sudoku puzzles that you can solve online.

## Modelling Sudokus

To implement a Sudoku-solving program, we need to come up with a way of modelling Sudokus. A Sudoku is a matrix of _cells_ which contain either a digit or are blank. The natural way of modelling a matrix is as a list of lists. The outer list represents all the _rows_, and the elements of the list are the elements of each row. Digits or blanks can be represented by using the Haskell `Maybe` type. Digits are simply represented by `Int`.

Summing up, a natural way to represent Sudokus is using the following Haskell datatype:

```
type Cell   = Maybe Inttype Row    = [Cell]data Sudoku = Sudoku [Row]
```

we also have a simple function

```
rows :: Sudoku -> [Row]rows (Sudoku ms) = ms
```

For example, the above Sudoku puzzle has the following representation in Haskell:

```
example :: Sudoku
example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]
```

or, written in a more compact way:

```
example :: Sudoku
example =
  Sudoku
    [[j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ],
     [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ],
     [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ],
     [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8],
     [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9],
     [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ],
     [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ],
     [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ],
     [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]]
  where
    n = Nothing
    j = Just
```

Now, a number of assignments follow, which will lead you step-by-step towards an implementation of a Sudoku-solver.

## Some Basic Sudoku Functions

To warm up, we start with a number of basic functions on Sudokus.

### Assignment A

___

**A1.** Implement a function

```
allBlankSudoku :: Sudoku
```

that represents a Sudoku that only contains blank cells (this means that no digits are present). Do not use copy-and-paste programming here! Your definition does not need to be longer than a few short lines.

**A2.** The Sudoku type we have defined allows for more things than valid Sudokus. For example, there is nothing in the type definition that says that a Sudoku has 9 rows and 9 columns, or that digits need to lie between 1 and 9. Implement a function

```
isSudoku :: Sudoku -> Bool
```

that checks if all such extra conditions are met by the given Sudoku.  Such a function is usually referred to as a _data type invariant_.  Examples:

```
isSudoku (Sudoku [])
False
isSudoku allBlankSudoku
True
isSudoku example
True
isSudoku (Sudoku (tail (rows example)))
False
```

_Note:_ Here we are only checking whether the sudokus have the right shape contain have valid numbers. We are not checking the other sudoku conditions (e.g. that there are no repeated numbers in a row; those checks will be done as a later part of the lab).

It is possible to define types that more precisely captures well-formed sudokus, thus making the `isSudoku` function superfluous and preventing bugs involving ill-formed sudokus from happening. If you want to explore this possibility, see [the extra assignment **Ä1**](https://chalmers.instructure.com/courses/20991/pages/lab-3-extra "Lab 3 Extra").

___

**A3.** Our job is to solve Sudokus. So, it would be handy to know when a Sudoku is solved or not. To begin with, we need a way to check if a Sudoku has been completely filled, i.e. there are no blank cells left. Implement the following function:

```
isFilled :: Sudoku -> Bool
```

Note that we do not check here if the Sudoku is a _valid_ solution; we will do this later. This means that `isFilled` returns `True` for any Sudoku without blanks (even if there is a row that contains the same digit twice, for example).

### Hints

To implement the above, use list comprehensions! Also, the following standard Haskell functions might come in handy:

```
replicate :: Int -> a -> [a]
length    :: [a] -> Int
and       :: [Bool] -> Bool
```

## Reading and Printing Sudokus

Next, we need to have a way of representing Sudokus in a file. In that way, our program can read Sudokus from a file, and it is easy for us to create and store several Sudoku puzzles.

The following is an example text-representation that we will use in this assignment. It actually represents the example above.

```
36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43
```

There are 9 lines of text in this representation, each corresponding to a row. Each line contains 9 characters. A digit 1 - 9 represents a filled cell, and a period (.) represents a blank cell.

### Assignment B

___

**B1.** Implement a function:

```
printSudoku :: Sudoku -> IO ()
```

that, given a Sudoku, creates instructions to print the Sudoku on the screen, using the format shown above. Example:

```
printSudoku allBlankSudoku
.........
.........
.........
.........
.........
.........
.........
.........
.........
```

```
printSudoku example
36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43
```

___

**B2.** Implement a function:

```
readSudoku :: FilePath -> IO Sudoku
```

that, given a filename, creates instructions that read the Sudoku from the file, and deliver it as the result of the instructions. You may decide yourself what to do when the file does not contain a representation of a Sudoku, but you must not try to accept invalid Sudoku files. Examples:

```
sud <- readSudoku "example.sud"
printSudoku sud
36..712..
.5....18.
..92.47..
....13.28
4..5.2..9
27.46....
..53.89..
.83....6.
..769..43
readSudoku "Sudoku.hs"
Program error: Not a Sudoku!
```

### Hints

To implement the above, you will need to be able to convert between characters (type `Char`) and digits/integers (type `Int`). The standard functions `show` and `digitToInt` (import the module `Data.Char`) will come in handy here.

Here are some more functions that might come in handy:

```
digitToInt :: Char -> Int
putStr     :: String -> IO ()
putStrLn   :: String -> IO ()
readFile   :: FilePath -> IO String
lines      :: String -> [String]
unlines    :: [String] -> String
```

Here are some example Sudoku-files that you can download and use:

-   [example.sud](https://chalmers.instructure.com/courses/20991/files/2353405/download?wrap=1 "example.sud") [Download example.sud](https://chalmers.instructure.com/courses/20991/files/2353405/download?download_frd=1), containing the above example.
-   [sudokus.zip](https://chalmers.instructure.com/courses/20991/files/2353403/download?wrap=1 "sudokus.zip") [Download sudokus.zip](https://chalmers.instructure.com/courses/20991/files/2353403/download?download_frd=1), a ZIPped collection of sudokus, both easy and hard ones. The easy ones should all be solvable by your final program within minutes; the hard ones will probably take a very long time (unless you do extra Assignment X and/or Y)!.

## Generating Sudokus as Test Data

Finally, we need to be able to test properties about the functions related to our Sudokus. In order to do so, QuickCheck needs to be able to generate arbitrary Sudokus.

Let us split this problem into a number of smaller problems. First, we need to know how to generate arbitrary cell values (of type `Maybe Int`). Then, we need to know how to generate 81 such cells, and compose them all into a Sudoku.

### Assignment C

___

**C1.** Implement a function:

```
cell :: Gen Cell
```

that contains instructions for generating a Sudoku cell. You have to think about the following: - Cells either contain a digit between 1 and 9 (for example `Just 3`) or are empty (`Nothing`), - We would like our generated Sudokus to resemble realistic Sudoku puzzles. Therefore, the distribution should be around 10% probability for non-empty cells vs. 90% probability for empty cells. (This is not something strict; you can play around with this if you like.)

Example:

```
sample cell
Just 3
Nothing
Nothing
Just 7
Nothing
```

___

**C2.** Make Sudokus an instance of the class Arbitrary.

```
instance Arbitrary Sudoku where
  ...
```

by generating 9 rows of 9 cells.

**Hint** the function `vectorOf :: Int -> Gen a -> Gen [a]` will do all the hard work for you here.

___

**C3.** Define a property

```
prop_Sudoku :: Sudoku -> Bool
```

that expresses that each generated Sudoku actually is a Sudoku according to Assignment A2. Also, use QuickCheck to check that the property actually holds for all Sudokus that are generated.

### Hints

Here are some functions from `Test.QuickCheck` that might come in handy:

```
sample    :: Show a => Gen a -> IO ()

choose    :: Random a => (a,a) -> Gen a
elements  :: [a] -> Gen a
frequency :: [(Int,Gen a)] -> Gen a
sequence  :: [Gen a] -> Gen [a]
vectorOf  :: Int -> Gen a -> Gen [a]
```

You might want to take a look at the lecture and example code on test data generation.

## Rows, Columns, Blocks

Now, we are going to think about what actually constitutes a valid solution to a Sudoku. There are three constraints that a valid solution has to fulfil:

-   No row can contain the same digit twice;
-   No column can contain the same digit twice;
-   No 3x3 block can contain the same digit twice.

This leads us to the definition of a _block_; a block is either a row or a column or a 3x3 block. A block, therefore, contains 9 cells:

```
type Block = [Maybe Int]
```

We are going to define a function that checks if a Sudoku is not violating any of the above constraints, by checking that none of the blocks violate those constraints.

### Assignment D

___

**D1.** Implement a function:

```
isOkayBlock :: Block -> Bool
```

that, given a block, checks if that block does not contain the same digit twice. Examples:

```
isOkayBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
True
isOkayBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
False
```

___

**D2.** Implement a function:

```
blocks :: Sudoku -> [Block]
```

that, given a Sudoku, creates a list of all blocks of that Sudoku. This means: - 9 rows, - 9 columns, - 9 3x3 blocks.

Also add a property that states that, for each Sudoku, there are 3\*9 blocks, and each block has exactly 9 cells.

```
prop_blocks_lengths :: ...
```

___

**D3.** Now, implement a function:

```
isOkay :: Sudoku -> Bool
```

that, given a Sudoku, checks that all rows, columns, and 3x3 blocks do not contain repeated digits. Examples:

```
isOkay allBlankSudoku
True
sud <- readSudoku "example.sud"
isOkay sud
True
```

### Hints

Here are some functions that might come in handy (depending on how you choose to solve the assignments):

```
nub       :: Eq a => [a] -> [a]
transpose :: [[a]] -> [[a]]
take      :: Int -> [a] -> [a]
drop      :: Int -> [a] -> [a]
splitAt   :: Int -> [a] -> ([a], [a])
```

Note that some of the above functions only appear when you `import Data.List`.

You might want to take a look at the exercises and answers on lists and list comprehensions.

## Positions and Finding Blanks

We are getting closer to the final solving function. Let us start thinking about how such a function would work.

Given a Sudoku, if there are no blanks left in the Sudoku, we are done. Otherwise, there is at least one blank cell that needs to be filled in somehow. We are going to write functions to find and manipulate blank cells.

It is quite natural to start to talk about _positions_. A position is a coordinate that identifies a cell in the Sudoku. Here is a way of modelling coordinates:

```
type Pos = (Int,Int)
```

We use positions as indicating first the row and then the column. For example, the position (3,5) denotes the 5th cell in the 3rd row.

Note: It is common in programming languages to start counting at 0! Therefore, the position that indicates the upper left corner is (0,0), and the position indicating the lower right corner is (8,8).

### Assignment E

___

**E1.** Implement a function:

```
blanks :: Sudoku -> [Pos]
```

that, given a Sudoku returns a list of the positions in the Sudoku that are still blank. You may decide on the order in which the positions appear. Examples:

```
length (blanks allBlankSudoku) == 9*9
True
blanks example
[(0,2),(0,3),(0,7),(0,8),(1,0),(1,2),(1,3),(1,4),(1,5),(1,8),(2,0),(2,1),(2,4),(2,7),(2,8),(3,0),(3,1),(3,2),(3,3),(3,6),(4,1),(4,2),(4,4),(4,6),(4,7),(5,2),(5,5),(5,6),(5,7),(5,8),(6,0),(6,1),(6,4),(6,7),(6,8),(7,0),(7,3),(7,4),(7,5),(7,6),(7,8),(8,0),(8,1),(8,5),(8,6)]
```

In addition, write a property that checks that the blanks of allBlankSudoku are indeed all of the expected 9x9 cells.  

`prop_blanks_allBlank :: Bool   `

Of course you could just run it and inspect it manually, but the purpose of the exercise here is to program it without a lot of cut-and-paste.  

**E2.** Implement a function:

```
(!!=) :: [a] -> (Int,a) -> [a]
```

that, given a list, and a tuple containing an index in the list and a new value, updates the given list with the new value at the given index. Examples:

```
['a','b','c','d'] !!= (1,'X')
['a','X','c','d']
["p","qq","rrr"] !!= (0,"bepa")
["bepa","qq","rrr"]
```

Also, write a property that state(s) the expected properties of this function. Think about what can go wrong!

```
prop_bangBangEquals_correct :: ...
```

So that we can test your `prop_bangBangEquals_correct` function, you must give it a non-polymorphic type (i.e. don't use type variables in its type).

___

**E3.** Implement a function:

```
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
```

that, given a Sudoku, a position, and a new cell value, updates the given Sudoku at the given position with the new value. Example:

```
printSudoku (update allBlankSudoku (1,3) (Just 5))
.........
...5.....
.........
.........
.........
.........
.........
.........
.........
```

Also, write a property that checks that the updated position really has gotten the new value.

```
prop_update_updated :: ...
```

___

### Hints

There is a standard function `(!!)` in Haskell for getting a specific element from a list. It starts indexing at 0, so, for example, to get the first element from a list xs, you can use xs !! 0.

We usually use the standard function `zip` to pair up elements in a list with their corresponding index. Example:

```
["apa","bepa","cepa"] `zip` [1..3]
[("apa",1),("bepa",2),("cepa",3)]
```

This, in combination with list comprehensions, should be very useful for this assignment!

When testing a property that is polymorphic (meaning that it has type variables in its type), you need to add a type signature that picks an arbitrary type. For example, when testing properties for the function (!!=), which works for lists of any type, you have to fix the type when testing, for example, lists of Integers. Do this by adding a type signature to your properties.

Here are some more useful functions:

```
head :: [a] -> a
(!!) :: [a] -> Int -> a
zip  :: [a] -> [b] -> [(a,b)]
```

## Solving Sudokus

Finally, we have all the bits in place to attack our main problem: Solving a given Sudoku.

Our objective is to define a Haskell function

```
solve :: Sudoku -> Maybe Sudoku
```

Here we suggest that you use the classic lazy method of implementing [backtracking Links to an external site.](http://en.wikipedia.org/wiki/Backtracking) (which fortunately does not require you to think about backtracking at all).

The strategy is to compute a list of _all_ solutions to a given sudoku and then take the first one in the list as the answer. This may sound horribly inefficient, since, for example, the all-blank sudoku has [6,670,903,752,021,072,936,960 possible solutions, Links to an external site.](https://en.wikipedia.org/wiki/Mathematics_of_Sudoku#Enumerating_all_possible_Sudoku_solutions) so it might take a while to compute them all! The key is that Haskell is a _lazy_ language which means that it does not compute more than it needs. So if you only ask for the first solution, only the first one will get computed, and `solve allBlankSudoku` should print the answer in the blink of an eye.

The idea, in a bit more detail, can be implemented as follows. Function `solve` calls a recursive helper function `solve'` which computes a list of _all_ solutions. If the list is empty then `solve` returns Nothing. Otherwise, it takes the first solution, `s`, and returns `Just s`. The helper function takes an extra argument: the list of blanks of the sudoku to be solved.

-   It first checks that the sudoku to be solved is not a bad one (using A2 and D3) -- if it is bad then there are no solutions. Otherwise,
    -   if there are no holes then the sudoku must be a solution.
    -   If there is at least one blank then we fill it in the nine different ways, and recursively solve those nine updated sudokus; we join all lists of solutions into one list.

### Assignment F

___

**F1.** Implement a function:

```
solve :: Sudoku -> Maybe Sudoku
```

using the above idea. Examples:

```
printSudoku (fromJust (solve allBlankSudoku))
123456789
456789123
789123456
214365897
365897214
897214365
531642978
642978531
978531642
```

```
sud <- readSudoku "example.sud"
printSudoku (fromJust (solve sud))
364871295
752936184
819254736
596713428
431582679
278469351
645328917
983147562
127695843
sud <- readSudoku "impossible.sud"
solve sud
Nothing
```

(In the above examples, we use the standard function `fromJust` from the library `Data.Maybe`.)

___

**F2.** For your own convenience, define a function:

```
readAndSolve :: FilePath -> IO ()
```

that produces instructions for reading the Sudoku from the given file, solving it, and printing the answer. Examples:

```
readAndSolve "example.sud"
364871295
752936184
819254736
596713428
431582679
278469351
645328917
983147562
127695843
readAndSolve "impossible.sud"
(no solution)
```

___

**F3.** Implement a function:

```
isSolutionOf :: Sudoku -> Sudoku -> Bool
```

that checks, given two Sudokus, whether the first one is a solution (i.e. all blocks are okay, there are no blanks), and also whether the first one is a solution of the second one (i.e. all digits in the second sudoku are maintained in the first one).

Since the purpose of this function will be to help with part F4 - the main test function for your solver - your definition of `isSolutionOf` should _not_ use the `solve` function in it definition.

Examples:

```
fromJust (solve allBlankSudoku) `isSolutionOf` allBlankSudoku
True
allBlankSudoku `isSolutionOf` allBlankSudoku
False
fromJust (solve allBlankSudoku) `isSolutionOf` example
False
```

___

**F4.** Define a property:

```
prop_SolveSound :: Sudoku -> Property
```

that says that the function `solve` is _sound_. Soundness means that every supposed solution produced by `solve` actually is a valid solution to the original problem.

### Hints

All the work we did in the assignments A -- E should be used in order to implement the function `solve`.

QuickChecking the property `prop_SolveSound` will probably take a long \`time. Be patient! Alternatively, there are a number of things you can do about this.

-   You can test on fewer examples (using the QuickCheck function `quickCheckWith`). You can for example define:
    
    ```
    fewerChecks prop =
      quickCheckWith stdArgs{maxSuccess=30 } prop
    ```
    
    and then write `fewerChecks prop_SolveSound` when you want to QuickCheck the property.
    
-   You can also generate Sudokus with a different probability distribution. Try increasing the number of digits in an arbitrary Sudoku by fiddling with the frequencies in the `cell` function from Assignment C1 and see what happens.
-   Instead of running interpreted code inside GHCi, you can compile your program to an executable file containing optimised native machine code, and run that:
    
    ```
    ghc -O Sudoku.hs
    [1 of 1] Compiling Main      ( Sudoku.hs, Sudoku.o )
    Linking Sudoku ...
    ./Sudoku
    …
    ```
    
    _Note:_ You need to define a `main :: IO()` function that runs the test you are interested in.
    

It is okay if you do not find a completely satisfactory solution to this issue.

Here are some useful functions:

```
listToMaybe :: [a] -> Maybe a
catMaybes   :: [Maybe a] -> [a]
fromJust    :: Maybe a -> a  -- not recommended
```

Note that `fromJust` is a _partial function_ that can cause your program to crash if used incorrectly (i.e. if your program tries to compute `fromJust Nothing`). The function `head` is also a partial function: it is not defined for empty lists, `head []` crashes. Using other library functions or pattern matching instead is more likely to result in code that is both elegant and correct.

Here is an example of an impossible Sudoku:

-   [impossible.sud](https://chalmers.instructure.com/courses/20991/files/2353407/download?wrap=1 "impossible.sud") [Download impossible.sud](https://chalmers.instructure.com/courses/20991/files/2353407/download?download_frd=1)

Just for fun. You can choose freely whether to do 0, 1 or more of these. Don't expect us to spend time grading these, however. There are no perfect, pre-defined answers here.

-   [Bring it on!](https://chalmers.instructure.com/courses/20991/pages/lab-3-extra "Lab 3 Extra")

## Submission

Submit your solutions using the Fire system.

Your submission should consist of the following file:

-   **Sudoku.hs**, containing your solution. It should contain enough comments to understand what is going on.

Before you submit your code, spend some time on **cleaning up** your code; make it simpler, remove unnecessary things, etc. We will reject your solution if it is not clean. Clean code:

-   Does not have long lines (< 78 characters)
-   Has a consistent layout
-   Has type signatures for all top-level functions
-   Has good comments
-   Has no junk (junk is unused code, commented code, unnecessary comments)
-   Has no overly complicated function definitions
-   Does not contain any repetitive code (copy-and-paste programming)

We strongly suggest that you use the [hlint Links to an external site.](https://github.com/ndmitchell/hlint#readme) program to help with many of these issues and other Haskell style issues.

Automatic checks will run when you submit your answers. The tests include running **hlint** and perhaps testing some of your functions with QuickCheck. The purpose of this is to give you some quick feedback and to help speed up the grading process. If the feedback you get is not helpful, you can safely ignore it.

_Based on a lab originally written and developed by [Koen Lindström Claessen Links to an external site.](http://www.cse.chalmers.se/~koen/)_

Last updates: 2020-11-11 by Dave