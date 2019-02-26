G52AFP Coursework 1 - Connect Four Game
   
Thomas Cameron Gregory Dudley
psytcd@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.List (isInfixOf, transpose)
> import Data.Sequence (Seq, fromList, update, index)
> import Data.Foldable (toList)
> import Data.Char (digitToInt)

----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]

> type Row = [Player]

> type SBoard = Seq SRow

> type SRow   = Seq Player

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------
Test Boards


The initial empty board:

> empty :: Board
> empty =  replicate rows (replicate cols B)

A board for testing:

> testboard :: Board
> testboard =  [[B,B,B,B,B,B,B],
>               [B,B,B,B,B,B,B],
>		        [B,B,B,X,O,B,B],
>		        [B,B,B,O,X,B,B],
>		        [B,B,B,O,O,X,B],
>		        [B,X,X,X,O,X,X]]

----------------------------------------------------------------------
Utility functions:

extraction:

> getRows :: Board -> [Row]
> getRows =  id

> getCols :: Board -> [Row]
> getCols =  transpose 

> getDiags     :: Board -> [Row]
> getDiags xss =  (diagonals xss) ++ (antiDiagonals xss)

Diagonals function from package Data.Universe.Helpers 
http://hackage.haskell.org/package/universe-base-1.0.2.1

> diagonals :: [[a]] -> [[a]]
> diagonals = tail . go [] where
>   go b es_ = [h | h:_ <- b] : case es_ of
>       []   -> transpose ts
>       e:es -> go (e:ts) es
>       where ts = [t | _:t <- b]


> antiDiagonals :: [[a]] -> [[a]]
> antiDiagonals =  diagonals.(map reverse)

insertion:
for clarity we convert our boards as matrixes to sequences, as these support updating items via index
using the update function. https://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-Sequence.html
The update method gives us a performance benefit, although the cost of conversion outweighs this so its mostly just for clarity.

> insertAt         :: Int -> Int -> Player -> Board -> Board
> insertAt x y p b =  toBoard(doubleUpdate x y p (toSequence b))

> doubleUpdate         :: Int -> Int -> a -> Seq( Seq a) -> Seq (Seq a)
> doubleUpdate x y a s =  update y (update x a (index s y)) s

> toSequence :: Board -> Seq (Seq Player)
> toSequence =  fromList.(map fromList)

> toBoard :: Seq (Seq Player) -> Board
> toBoard =  (map toList).toList


Get remaining spaces in a column:

> free    :: Row -> Int
> free    =  ((-)rows).length.dropWhile(==B)

get the next index to insert at:

> next    :: Row -> Int
> next r  =  (free r) - 1

----------------------------------------------------------------------
Count player in row

> countRow    :: Player -> Row -> Int
> countRow p  =  (length . filter (== p))

Count player in board

> countBoard     :: Player -> Board -> Int
> countBoard p b  = sum (map (countRow p) b)

----------------------------------------------------------------------
Turn checking:

In our game, X always goes first.
So when the Number of Xs == the Number of Os it is X's go,
otherwise it is O's go.

> isEqual   :: Board -> Bool
> isEqual b = (countBoard X b) == (countBoard O b)

> turn               :: Board -> Player
> turn b | isEqual b = X
>        | otherwise = O

----------------------------------------------------------------------
Win Checking:

> winRow     :: Player -> Row -> Bool
> winRow p =  isInfixOf (replicate win p)

> winBoard     :: Player -> Board -> Bool
> winBoard p b =  any (winRow p) (getRows b) ||
>                 any (winRow p) (getCols b) ||
>                 any (winRow p) (getDiags b)				

Check if board full:

> full :: Board -> Bool
> full = (notElem B).concat

----------------------------------------------------------------------
Make a move:

> move       :: Int -> Player -> Board -> Board
> move x p b =  insertAt x (next(getCols b !! x)) p b

Get input:

> input      :: Player -> IO Int
> input p     =  do putStrLn("Enter your move player "++show p++":")
>                   line <- getLine
>                   let i = read(line)
>                   return i

----------------------------------------------------------------------
Our game loop:

> play   :: Board -> IO ()
> play b | winBoard X b = putStrLn "Player X won!"
>        | winBoard O b = putStrLn "Player O won!"
>		 | full b       = putStrLn "Its a draw."
>		 | otherwise    = do 
>                            let p = turn b
>                            k  <- input p
>                            let nb = move k p b
>                            showBoard(nb)
>                            play nb

> main   :: IO ()
> main   =  do showBoard empty
>              play empty

---------------------------------------------------------------------
Minimax types:

> data Tree a = Node a [Tree a]

moves :: SBoard -> Seq SBoard

growTree :: Board -> Tree SBoard
