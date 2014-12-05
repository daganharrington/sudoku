
--sudoku solver pulled together by working on exercises from some university -dgn

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split

example :: Sudoku
example =
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

type Sudoku = [[Maybe Int]] --to say [Block] is misleading
type Block  = [Maybe Int]
type Pos    = (Int,Int) --matrix style position (not chessboard)

empty :: Sudoku
empty = [replicate 9 Nothing | rows <- [1..9]]

--9 rows, 9 cols, every elem is Nothing or Just [1..9]
isSudoku :: Sudoku -> Bool 
isSudoku s = (==9) (length s ) &&
             ( and . map (==9) . map length $ s) &&
             ( and . map isvalid . concat $ s) where
               isvalid x = case x of
                  Nothing -> True
                  Just x -> elem x [1..9]                      

--read nub src for this obvoius way to find duplicates
isSafeBlock :: Block -> Bool
isSafeBlock [] = error "invalid block"
isSafeBlock b = isSafeBlock' (filter isJust b) [] where
 isSafeBlock' [] _ = True 
 isSafeBlock' (b:bs) ls
    | elem b ls    = False
    | otherwise    = isSafeBlock' bs (b:ls)

blocks :: Sudoku -> [Block]
blocks s = s ++ getcols s ++ getsquares s where  
  getcols = transpose 
  getsquares s = concat . map (map concat . transpose) . chunksOf 3 . map (chunksOf 3) $ s --yitz in #haskell for help

--is the entire sudoku free from relevant collisions (row/col/block)?
prop_isOK :: Sudoku -> Bool 
prop_isOK = and . (map isSafeBlock) . blocks

poslabels :: Sudoku -> [(Int,[(Int, Maybe Int)])] --ugly type : -> [(row#, [(col#, cell value)])]
poslabels = zip [0..] . map (zip [0..]) --zerobased

--clean this up .. not very intelligible
allBlanks :: Sudoku -> [Pos]
allBlanks = concatMap rowBlanks . poslabels where 
  rowBlanks x = [(r,cs) | r <- [fst x], cs <- (map fst . filter (isNothing . snd) . snd $ x)]

prop_noBlanks :: Sudoku -> Bool
prop_noBlanks s = allBlanks s == []

--randomize later ... is this actually an improvement?
fstblank :: [Pos] -> Pos
fstblank = head

--randblank :: ? -> ? -> Pos

(!!=) :: [a] -> (Int, a) -> [a] --update the elem @ Int with a new a
(!!=) [] _ = []
(!!=) (x:xs) (0, a) = a : xs --zero-based
(!!=) (x:xs) (pos,newval) = x : (!!=) xs (pos-1,newval) 

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (x:xs) (row,col) newval
  | row == 0  = (!!=) x (col, newval) : xs
  | otherwise = x : update xs (row-1,col) newval 

solve :: Sudoku -> Maybe Sudoku
solve s
   | (not . prop_isOK $ s) = Nothing --not valid
   | prop_noBlanks s && prop_isOK s = Just s --valid and full <=> solved
   | otherwise = undefined --pick the first blank and solve with substituting [1..9]



--   | otherwise =  fromJust . listToMaybe $ [solve (update s (fstblank . allBlanks $ s) (Just x )) | x <- [1..9] ]
--   | otherwise = solve (foldr (\newval -> update s (fstblank . allBlanks $ s) newval) s [Just x | x <- [1..9]])
--   | otherwise = listToMaybe $ [sol | n <- [1..9], sol <- (solve (update s (fstblank . allBlanks $ s) (Just n)))]
--   | otherwise = solve . fromJust . listToMaybe $ [sol | n <- [1..9], sol <- (update s (fstblank . allBlanks $ s) (Just n))]
--   | otherwise = foldl (solve (\newval -> update s (fstblank . allBlanks $ s) newval))  s [Just x | x <- [1..9]]  

--SOLVE NEEDS TO BE MONADIC .. CAN'T BACKTRACK WITHOUT

--need to chain maybe sudokus
-- Maybe Sudoku -> (Sudoku -> Maybe Sudoku) -> Maybe Sudoku

--(>>=) :: m s -> (s -> m s) -> m s 

--bnd :: Maybe Sudoku -> (Sudoku -> Maybe Sudoku) -> Maybe Sudoku

--test s = do
--   s' <- (Just (update s (fstblank . allBlanks $ s) (Just 4 )))
--   print s'
  

--try to write with mapM_
prnt :: [[Maybe Int]] -> IO ()
prnt []         = do return ()
prnt (row:rows) = do
  putStrLn . (map charrep) $ row
  prnt rows
    where
      charrep x = case x of 
        Just x -> intToDigit x
        Nothing -> '_' 


main = do
  prnt empty
  putStrLn "\n"
  prnt example
  putStrLn "\n"
  let x = (update example (1,1) (Just 7)) 
  prnt x
--  x <- [1..9]
  print (prop_isOK  x)

