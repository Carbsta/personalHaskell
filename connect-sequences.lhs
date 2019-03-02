G52AFP Coursework 1 - Connect Four Game
   
Your full name(s)
Your full email address(es)

----------------------------------------------------------------------

> import Data.List (isInfixOf, transpose)
> import Data.Sequence (Seq, fromList, update, index, dropWhileL, (!?), filter, replicate)
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

The board itself is represented as a list of columns where each column is
a list of player values, subject to the above row and column sizes,
we represent the board "sideways" so we only need to transpose it when displaying it or checking for wins,
rather than having to transpose whenever we want to make a move:

> type Board = [Col]

> type Col = [Player]

> type SBoard = Seq SCol

> type SCol   = Seq Player

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow (getRows b) ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = Prelude.replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

> sshowBoard :: SBoard -> IO ()
> sshowBoard b = putStrLn (unlines (map sshowRow (getRows (toBoard b)) ++ [line] ++ [nums]))
>               where
>                  sshowRow = map sshowPlayer
>                  line    = Prelude.replicate cols '-'
>                  nums    = take cols ['0'..]
>
> sshowPlayer :: Player -> Char
> sshowPlayer O = 'O'
> sshowPlayer B = '.'
> sshowPlayer X = 'X'

----------------------------------------------------------------------
Test Boards


The initial empty board:

> empty :: Board
> empty =  Prelude.replicate cols (Prelude.replicate rows B)

> sempty :: SBoard
> sempty  =  Data.Sequence.replicate cols (Data.Sequence.replicate rows B)

A board for testing, note how the board is "sideways"
The top right cell is 0,0 and pieces "fall" from left to right down each column:

> testboard :: Board
> testboard =  [[B,B,B,B,B,B],
>               [B,B,B,B,B,B],
>		        [B,B,B,B,B,O],
>		        [B,B,B,B,O,X],
>		        [B,B,B,O,O,X],
>		        [B,X,X,X,O,X],
>               [B,X,X,O,X,O]]


> stestboard :: SBoard
> stestboard =  toSequence testboard

----------------------------------------------------------------------
Utility functions:

extraction:

> getCols :: Board -> [Col]
> getCols =  id

> getRows :: Board -> [Col]
> getRows =  transpose 

> getDiags     :: Board -> [Col]
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
> doubleUpdate x y a s =  update x (update y a (index s x)) s

> toSequence :: Board -> SBoard
> toSequence =  fromList.(map fromList)

> toBoard :: SBoard -> Board
> toBoard =  (map toList).toList


Get remaining spaces in a column:

> free    :: Col -> Int
> free    =  ((-)rows).length.dropWhile(==B)

get the next index to insert at:

> next    :: Col -> Int
> next c  =  (free c) - 1


Sequence versions (trying to remove lists from the program entirely):

> sgetCols :: SBoard -> Seq SCol
> sgetCols = id

> sgetRows :: SBoard -> Seq SCol
> sgetRows =  toSequence.transpose.toBoard

> sgetDiags :: SBoard -> Seq SCol
> sgetDiags = toSequence.getDiags.toBoard


> smove      :: Int -> Player -> SBoard -> SBoard
> smove x p b =  doubleUpdate x (snext (b `index` x)) p b

> sfree    :: SCol -> Int
> sfree    =  ((-)rows).length.dropWhileL(==B)

> snext    :: SCol -> Int
> snext c  =  (sfree c) - 1

----------------------------------------------------------------------
Count player in column

> countCol    :: Player -> Col -> Int
> countCol p  =  (length . Prelude.filter (== p))

> scountCol    :: Player -> SCol -> Int
> scountCol p  =  (length . Data.Sequence.filter (== p))

Count player in board

> countBoard     :: Player -> Board -> Int
> countBoard p b  = sum (map (countCol p) b)

> scountBoard    :: Player -> SBoard -> Int
> scountBoard p b = sum (fmap (scountCol p) b)

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


sequence versions

> sisEqual    :: SBoard -> Bool
> sisEqual b  =  (scountBoard X b) == (scountBoard O b)

> sturn              :: SBoard -> Player
> sturn b | sisEqual b = X
>         | otherwise  = O

----------------------------------------------------------------------
Win Checking:

> winCol     :: Player -> Col -> Bool
> winCol p =  isInfixOf (Prelude.replicate win p)

> winBoard     :: Player -> Board -> Bool
> winBoard p b =  any (winCol p) (getCols b) ||
>                 any (winCol p) (getRows b) ||
>                 any (winCol p) (getDiags b)	

			

Check if board full:

> full :: Board -> Bool
> full = (notElem B).concat

----------------------------------------------------------------------
Make a move:

> move       :: Int -> Player -> Board -> Board
> move x p b =  insertAt x (next (b !! x)) p b

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



smoves    :: Player -> SBoard -> Seq SBoard
smoves p  = do 

sgrowTree :: Board -> Tree SBoard

sprune :: Int -> Tree a -> Tree a

