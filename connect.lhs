G52AFP Coursework 1 - Connect Four Game
   
Thomas Cameron Gregory Dudley
psytcd@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.List (isInfixOf, transpose, group)
> import Data.Sequence (Seq, fromList, update, index)
> import Data.Foldable (toList)
> import Data.Char (digitToInt)
> import Data.Maybe (fromJust, catMaybes)
> import Text.Read (readMaybe)

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

We can turn the CPU player on or off by changing this value between True or False

> cpu :: Bool
> cpu =  True

parameter for the evaluation function that determines the weight given to forming lengths of win-1 over forming lengths of win-2.
optimal value would have to be determined through experimentation.

> penultimateMult :: Int
> penultimateMult =  win*2

The board itself is represented as a list of columns where each column is
a list of player values, subject to the above row and column sizes,
we represent the board "sideways" so we only need to transpose it when displaying it or checking for wins,
rather than having to transpose whenever we want to make a move:

> type Board = [Col]

> type Col = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow (getRows b) ++ [line] ++ [nums]))
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
> empty =  replicate cols (replicate rows B)

Some boards used for testing, note how the board is "sideways"
The top right cell is 0,0 and pieces "fall" from left to right down each column:

> testboard :: Board
> testboard =  [[B,B,B,B,B,B],
>               [B,B,B,B,B,B],
>		        [B,B,B,B,B,O],
>		        [B,B,B,B,X,X],
>		        [B,B,B,O,O,X],
>		        [B,O,X,X,O,X],
>               [B,X,X,O,X,O]]

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

> insertAt         :: Int -> Int -> Player -> Board -> Board
> insertAt x y p b =  toBoard(doubleUpdate x y p (toSequence b))

> doubleUpdate         :: Int -> Int -> a -> Seq( Seq a) -> Seq (Seq a)
> doubleUpdate x y a s =  update x (update y a (index s x)) s

> toSequence :: Board -> Seq (Seq Player)
> toSequence =  fromList.(map fromList)

> toBoard :: Seq (Seq Player) -> Board
> toBoard =  (map toList).toList


Get remaining spaces in a column:

> free    :: Col -> Int
> free    =  ((-)rows).length.dropWhile(==B)

get the next index to insert at:

> next    :: Col -> Int
> next c  =  (free c) - 1

----------------------------------------------------------------------
Count player in column

> countCol    :: Player -> Col -> Int
> countCol p  =  (length . filter (== p))

Count player in board

> countBoard     :: Player -> Board -> Int
> countBoard p b  = sum (map (countCol p) b)

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

> winCol     :: Player -> Col -> Bool
> winCol p =  isInfixOf (replicate win p)

> winBoard     :: Player -> Board -> Bool
> winBoard p b =  any (winCol p) (getCols b) ||
>                 any (winCol p) (getRows b) ||
>                 any (winCol p) (getDiags b)	

			

Check if board full:

> full :: Board -> Bool
> full = (notElem B).concat

----------------------------------------------------------------------
Make a move, moves can fail so returns a Maybe Board:

> move       :: Int -> Player -> Board -> Maybe Board
> move x p b | x > cols-1          = Nothing
>            | next (b !! x) == -1 = Nothing
>            | otherwise           = Just (insertAt x (next (b !! x)) p b)

The input that a human makes might fail, so this is a wrapper for the move function that takes a Maybe Int

> maybeMove              :: Maybe Int -> Player -> Board -> Maybe Board
> maybeMove Nothing _ _  = Nothing
> maybeMove (Just x) p b = move x p b

Get input:

> input      :: Player -> IO (Maybe Int)
> input p     =  do putStrLn("Enter your move player "++show p++":")
>                   line <- getLine
>                   let i = readMaybe(line)
>                   return i

----------------------------------------------------------------------
Our game loop.
A board can be a win, loss, or full. Otherwise there are moves to make.
On the CPU's turn, we call the compMove function to calculate the best move to make using minimax.
On the player's turn we take an input and send this to maybeMove.
If the move or the input fails we ask the player to make another move.

> play   :: Board -> IO ()
> play b | winBoard X b       = putStrLn "Player X won!"
>        | winBoard O b       = putStrLn "Player O won!"
>		 | full b             = putStrLn "Its a draw."
>        | turn b == O && cpu = do putStrLn "CPU is plotting your demise... "
>                                  let nb = compMove b
>                                  showBoard(nb)
>                                  play(nb)
>		 | otherwise          = do 
>                                 let p = turn b
>                                 k  <- input p
>                                 let nb = maybeMove k p b
>                                 if nb == Nothing then
>                                    putStrLn "Not a valid move." >>
>                                    play b
>                                 else
>                                    showBoard(fromJust nb) >>
>                                    play (fromJust nb) 

Our main function just calls our game loop, passing in the empty board.

> main   :: IO ()
> main   =  do showBoard empty
>              play empty

---------------------------------------------------------------------
Minimax.
This implementation of minimax grows the tree down to the specified depth.
It labels the leaf nodes a tuple of (Player,Int).
The Int value is the value of the evaluation function for the board, which returns a positive value
for boards where X is winning and a negative value for boards where O is winning.
This allows boards where there is no winner, that would be assigned the player B, to be compared.
For example given the choice between two boards with the lablels (B,-4), (B,2) for Player O
the minimax algorithm would pick the board with the label (B,-4) as it is trying to minimise the evaluation function value.
This allows for comparisons between boards where no player has won, while the search space is still large, and the depth value is smaller.


Trees are Nodes with a value and a list of their child Nodes

> data Tree a = Node a [Tree a]
>                    deriving Show


moves creates all the possible moves from a board, catMaybes removes all the moves that would have been invalid.

> moves     :: Player -> Board -> [Board]
> moves p b =  catMaybes [move i p b | i <- [0..(cols-1)]]

growTree grows the tree by calling moves until a player has won or the board is full (when moves returns an empty list)

> growTree                  :: Board -> Tree Board
> growTree b | winBoard X b || winBoard O b = Node b []
>            | otherwise    = Node b (map growTree (moves (turn b) b))

prune cuts the tree at a specified depth.

> prune               :: Int -> Tree a -> Tree a
> prune 0 (Node x _)  = Node x []
> prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

evaluation calculates a value for a board based on the number of seqences of length win-1 and win-2 present in the board,
we only call it for boards that would normally just be labelled B, so leaves where no player has won, which is common at the start of the game.
We break this down into evaluating the board for both players, and calculating player a - player b. Our getCols, getRows, and getDiags functions allow us
to evaluate a column at a time and sum the results.
The "penultimateMult" factor determines how much more valuable our algorithm considers getting a sequence of win-1 over getting a sequence of win-2
The former will often take us to a board that is only one move away from winning, but the latter may open up more potential ways to win.
We consider the state of the board from the opponents perspective too as we want to also block them from forming sequences of length win-1 and win-2 that could
lead them to winning positions.

> evaluation     :: Player -> Board -> Int
> evaluation p b = (evaluateBoard p b) - (evaluateBoard (opponent p) b)

> evaluateBoard     :: Player -> Board -> Int
> evaluateBoard p b = sum (map (evaluateColN 1 p) (getCols b)) * penultimateMult + 
>                     sum (map (evaluateColN 2 p) (getCols b)) +
>                     sum (map (evaluateColN 1 p) (getRows b)) * penultimateMult +
>                     sum (map (evaluateColN 2 p) (getRows b)) +
>                     sum (map (evaluateColN 1 p) (getDiags b)) * penultimateMult +
>                     sum (map (evaluateColN 2 p) (getDiags b))

> evaluateColN       :: Int -> Player -> Col -> Int
> evaluateColN n p c =  length (filter (==(replicate (win -n) p)) (group c))


A helper function that flips the player, to allow us to evaluate how a move impacts the opponent's board.

> opponent :: Player -> Player
> opponent X = O
> opponent O = X
> opponent B = B

For the minimax algorithm leaf nodes are either labled (X,0), (O,0) or (B,n)
where n = evaluation(X,b), so n will have a positive value if X is winning and a negative value if O is winning.
(O,0) < (B,n) < (X,0) thanks to how comparisons of tuples work.
These values are then propagated back up the tree by choosing the minimum (for player O) and maxium (for player X) p,v pairs.

> minimax :: Tree Board -> Tree ((Player, Int), Board)
> minimax (Node b [])
>    | winBoard X b = Node ((X,0),b) []
>  	 | winBoard O b = Node ((O,0),b) []
>	 | otherwise    = Node ((B,evaluation X b),b) []
> minimax (Node b ts)
>    | turn b == O = Node (minimum players, b) ts'
>  	 | turn b == X = Node (maximum players, b) ts'
>	                 where
>					   ts'      = map minimax ts
>					   players  = [(p,v) | Node ((p,v),_) _ <- ts']

compMove decides the best move for the CPU player to take by performing minimax on the gametree that has been grown to depth depth.
There may be multiple equally good moves so it just selects the first one, our evaluation function helps reduce the number of boards that are considered equal anyway.

> compMove :: Board -> Board
> compMove b  = head[b' | Node (pv,b') _ <- ts, pv == pv']
>                 where
>                   gametree = prune depth (growTree b)
>                   Node (pv',_) ts = minimax gametree
