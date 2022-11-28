{- Lab 3
   Date: November 21, 2022
   Authors: Drake Axelrod, Hugo Lom
   Lab group: 68
 -}

module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad (mfilter)


------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

-- data Sudoku = Sudoku [Row] 
--  deriving ( Show, Eq )
newtype Sudoku
  = Sudoku [Row]
  deriving (Show, Eq)


rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
-- create a sudoku with 9 rows, each row has 9 blanks
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- immediately return false if the sudoku is empty
-- check if the sudoku has 9 rows and each row has 9 cells
-- check if each cell is a valid cell
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku (Sudoku rows) = length rows == 9 && all f rows
  where f r = (length r == 9) && all (maybe True (\n -> 1 <= n && n <= 9)) r

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
-- check is rows is valid sudoku checks if length of rows is 9 and each row has 9 cells
-- check if each cell is a valid cell and not Nothing
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all (all isJust) rows


------------------------------------------------------------------------------

-- * B1

-- | printSudoku prints a nice representation of the sudoku to stdout
-- print each row replace Nothing with '.' and the value with the value
-- unlines creates a string with newlines between each row
-- map (map f) maps the function f over each cell in the row
-- concatMap (map f) maps the function f over each cell in the row and concatenates the result
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s) = putStrLn $ unlines $ map (concatMap f) s
  where f Nothing = "."
        f (Just n) = show n
  

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
-- reads the file into a string s and then parses the string into a sudoku
-- using using map (map f) to map the function f over each cell in the row
-- lines splits the string into a list of strings, each string is a row
-- char2Cell converts a character to a cell
-- digitToInt converts a digit to an integer
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  s <- readFile fp
  return $ Sudoku $ map (map char2Cell) $ lines s
  where char2Cell '.' = Nothing
        char2Cell n = Just $ digitToInt n

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
-- elements generates an arbitrary element from a list
-- replicate 9 Nothing generates a list of 9 Nothing
-- map Just [1..9] generates a list of 9 Just 1..9
-- then we concat the two lists for a even distribution of numbers and Nothing
cell :: Gen Cell
cell = elements $ replicate 27 Nothing ++ map Just [1..9]
-- [Nothing, map Maybe [1..9]]
-- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#v:choose

-- * C2

-- | an instance for generating Arbitrary Sudokus
-- vectorOf generates a list of n arbitrary elements
-- so calling vectorOf 9 on vectorOf 9 cell generates
-- a list of 9 lists of 9 arbitrary elements
instance Arbitrary Sudoku where
 arbitrary = do
   s <- vectorOf 9 (vectorOf 9 cell)
   return $ Sudoku s

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3
-- | Checks if the sudoku is a 9x9 grid where every cell is either missing or between 1 and 9
-- use isSudoku since it solves the same problem
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- | Checks if the block has only unique cells, given that they are filled in
isOkayBlock :: Block -> Bool
-- takes list sorts and filters on ~= nothing
-- check if empty, checks if value
-- checks consecutive elements for ~=
isOkayBlock = not . null . nub . filter (/= Nothing)


-- * D2
-- | Helper method, creates a single row of blocks given three sudoku rows.
blocks' :: [[Cell]] -> [Block]
blocks' [r1, r2, r3] = map (concatMap f) $ chunksOf 3 $ zip3 r1 r2 r3
  where f (c1, c2, c3) = [c1, c2, c3]
blocks' _ = undefined

-- | Given a number `n` and a list, returns a new list containing chunks of `n` 
-- items from the list
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Extracts all 9, 3x3 blocks from the sudoku. 
allBlocks :: Sudoku -> [Block]
allBlocks = concatMap blocks' . chunksOf 3 . rows
-- concat . map blocks' . chunksOf 3 . rows 

-- | Recursively sorts a list of lists of cells
-- made this just to see if output was the same as the test
sortBlocks :: [Block] -> [Block]
sortBlocks = sort . map sort

-- | given a Sudoku, creates a list of all blocks (rows, columns, 3x3 boxes) of 
-- that Sudoku.
blocks :: Sudoku -> [Block]
-- concat the rows, columns and blocks
blocks (Sudoku r) = r ++ transpose r ++ allBlocks ( Sudoku r) 


-- | Checks if the length of the block size is 27 and that all
-- the blocks length is nine
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = f . blocks
  where f bs = length bs == 27 && all (\b -> length b == 9) bs

-- * D3

-- | Checks that within every block, there are no repeated digits
isOkay :: Sudoku -> Bool
isOkay = all isOkayBlock . blocks

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

-- | Blanks extracts the positions of all blank cells in a Sudoku
-- refactor blanks using elemIndices
blanks :: Sudoku -> [Pos]
blanks (Sudoku r) = concat $ zipWith f r [0..]
  where f r' i = map (\j -> (i,j)) $ elemIndices Nothing r'


-- | Property that checks that the blanks of allBlankSudoku 
-- are indeed all of the expected 9x9 cells.  
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = all (\c -> (0,0) <= c && c <= (8,8)) b && length b == 81
  where b = blanks allBlankSudoku


-- * E2

-- | Update a list at a supplied index with the supplied value
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = take i xs ++ [y] ++ drop (i+1) xs


-- So that we can test your `prop_bangBangEquals_correct` function, 
-- you must give it a non-polymorphic type
-- (i.e. don't use type variables in its type).
prop_bangBangEquals_correct :: [Cell] -> (Int,Cell) -> Property
prop_bangBangEquals_correct xs (i,y) = 
  0 <= i && i < length xs && not (null xs) ==> l && v
  where xs' = xs !!= (i,y)
        l = length xs' == length xs
        v = xs' !! i == y



-- * E3
-- | Returns the sudoku with a single cell updated 
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku r) (y,x) c = Sudoku $ r !!= (y, (r !! y) !!= (x, c))

-- | Map within tuples
mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (x,y) = (f x, f y)

-- | Assert that the cell was updated correctly
prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s p c = r' !! y !! x == c
  where s' = update s p' c
        Sudoku r' = s'
        p' = mapT (abs . (`mod`9)) p
        (y, x) = p'


------------------------------------------------------------------------------

-- * F1

-- | Helper function to generate every solution to a given a sudoku 
-- by recursively generating every possible sudoku variation for the given 
-- empty cells. It discards all variations that results in an invalid sudoku.
solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' s _     | not (isOkay s) = []
solve' s []    = [s]
solve' s (p:r) = concatMap (\c -> flip solve' r $ update s p (Just c)) [1..9]

-- | Solve a Sudoku puzzle by extracting either the first solution produced by solve'
-- or indicating failure if no solutions exists.
solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s) = Nothing
        -- | isFilled s && isOkay s = Just s
        | otherwise = listToMaybe $ solve' s $ blanks s


-- * F2
-- | Reads a Sudoku from a file and solves it, printing the result
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
    s <- readSudoku fp
    case solve s of
      Just s' -> printSudoku s'
      _       -> putStr "(no solution)\n"

-- * F3
-- | Helper function to check equality between two sudokus while ignoring
-- empty cells in either sudoku (treating them as "possibly" equal). Also
-- nice unicode support!
(😋) :: Eq a => Maybe a -> Maybe a -> Bool
Nothing 😋 (Just _) = True
(Just _) 😋 Nothing = True
x 😋 y = x == y

-- | Checks if a filled sudoku is solution to another sudoku by checking
-- that each filled cell in the second sudoku exists at same position in 
-- the solved one
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s s' | isFilled s && isOkay s = all id $ zipWith (😋) c c'
                  | otherwise = False
  where c  = concat $ rows s
        c' = concat $ rows s'

-- * F4
-- | QuickCheck property that checks if a solved sudoku is the solution of a
-- presolved sudoku
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s | not (isOkay s) = property Discard 
                  | otherwise = maybe (property Discard) f (solve s)
  where f s' = property $ isSolutionOf s' s
