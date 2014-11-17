
--sudoku solver pulled together by working on exercises from some university -dgn

import Control.Monad
import Data.Char

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

data Sudoku = Sudoku [[Maybe Int]]

--why have a type constructor Sudoku or data type when i'm just going to pull out the [[Maybe Int]] ?
rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

empty :: Sudoku
--empty = Sudoku [ [Nothing | rows <- [1..9]] | cols <- [1..9] ]
empty = Sudoku [replicate 9 Nothing | rows <- [1..9]]

isSudoku :: Sudoku -> Bool --9 rows, 9 cols, every elem is Nothing or Just [1..9]
isSudoku (Sudoku s) = (==9) (length s) &&
                      ( and . map (==9) . map length $ s ) &&
                      ( and . map isvalid . concat $ s ) where
                        isvalid x = case x of
                          Nothing -> True
                          Just x -> elem x [1..9]
                      
charrep :: Maybe Int -> Char
charrep x = case x of
 Just x -> intToDigit x
 Nothing -> '_'
 
--try to write with mapM_
prnt :: [[Maybe Int]] -> IO ()
prnt [] = do return ()
prnt (row:rows) = do
  putStrLn . (map charrep) $ row 
  prnt rows


main = do
  prnt . rows $ empty
  putStrLn "\n"
  prnt . rows $ example
