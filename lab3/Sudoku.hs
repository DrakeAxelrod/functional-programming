module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe
import Data.List


------------------------------------------------------------------------------
main :: IO ()
main = do
   printSudoku example
-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

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
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku []) = False
isSudoku (Sudoku rows) = length rows == 9 && all f rows
  where f r = (length r == 9) && all (\c -> maybe True (\n -> 1 <= n && n <= 9) c) r

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = isSudoku (Sudoku rows) && length rows == 9 && all f rows
  where f r = (length r == 9) && all (not . isNothing) r


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s) = putStrLn $ unlines $ map (concatMap f) s
  where f Nothing = "."
        f (Just n) = show n
  

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  s <- readFile fp
  return $ Sudoku $ map (map char2Cell) $ lines s
  where char2Cell '.' = Nothing
        char2Cell n = Just $ digitToInt n

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = elements $ (take 9 $ repeat Nothing) ++ map Just [1..9]
-- [Nothing, map Maybe [1..9]]
-- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#v:choose

-- * C2

-- | an instance for generating Arbitrary Sudokus
-- generates 
instance Arbitrary Sudoku where
 arbitrary = do
   rows <- vectorOf 9 (vectorOf 9 cell)
   return $ Sudoku rows

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
isOkayBlock :: Block -> Bool
-- takes list sorts and filters on ~= nothing
-- check if empty, checks if value
-- checks consecutive elements for ~=
isOkayBlock = f . sort . filter (/= Nothing)
  where f [] = True
        f [x] = True
        f (x:y:xs) = x /= y && f (y:xs)


-- * D2

blocks' :: [[Cell]] -> [Block]
blocks' [r1, r2, r3] = map (concat . map f) $ chunksOf 3 $ zip3 r1 r2 r3
  where f (c1, c2, c3) = [c1, c2, c3]
blocks' _ = undefined

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = concat $ map blocks' $ chunksOf 3 rows 

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = f . blocks
  where f bs = length bs == 9 && all (\b -> length b == 9) bs

-- * D3

isOkay :: Sudoku -> Bool
isOkay (Sudoku rs) = all isOkayBlock $ concat [rs, transpose rs, blocks (Sudoku rs)]
  

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
