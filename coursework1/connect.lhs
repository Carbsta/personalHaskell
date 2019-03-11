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
> depth = 4

We can turn the CPU players on or off by changing these values between True or False,
X always goes first, O goes second:

---Modify these Boolean values---
----To Change who goes first-----
--Or to play Human v Human ------
--Or------------AI v AI---------- 

> cpuX :: Bool
> cpuX =  False

> cpuO :: Bool
> cpuO =  True

---------------------------------


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
Test Boards:
empty is also used to start the game.

The initial empty board:

> empty :: Board
> empty =  replicate cols (replicate rows B)

a board used for testing, note how the board is "sideways"
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
Extraction of columns from the board:

We treat Columns, Rows and Diagonals as the same
and return them all as a list of columns.

getting the columns is easy, as the board is allready represented as a
list of columns, so this is just the identity of the board. 

> getCols :: Board -> [Col]
> getCols =  id

getting the rows requires transposing the board.

> getRows :: Board -> [Col]
> getRows =  transpose 

getting the diagonals is more complicated, we extract the diagonals and the 
anti diagonals and return these as one list.

> getDiags     :: Board -> [Col]
> getDiags xss =  (diagonals xss) ++ (antiDiagonals xss)

Diagonals function from package Data.Universe.Helpers, 
http://hackage.haskell.org/package/universe-base-1.0.2.1

> diagonals :: [[a]] -> [[a]]
> diagonals = tail . go [] where
>   go b es_ = [h | h:_ <- b] : case es_ of
>       []   -> transpose ts
>       e:es -> go (e:ts) es
>       where ts = [t | _:t <- b]

diagonals gets one set of diagonals, so antiDiagonals gets the other set by reversing the board and applying diagonals.

> antiDiagonals :: [[a]] -> [[a]]
> antiDiagonals =  diagonals.(map reverse)

----------------------------------------------------------------------
Insertion into the board:

for clarity we convert our boards as matrixes to sequences, as these support updating items via index
using the update function. https://hackage.haskell.org/package/containers-0.2.0.1/docs/Data-Sequence.html
We use this update function to define our own doubleUpdate function that allows us to insert into a
two dimensional sequence of sequences and finally our insertAt function converts the board to sequences,
updates them, and converts back.

> insertAt         :: Int -> Int -> Player -> Board -> Board
> insertAt x y p b =  toBoard(doubleUpdate x y p (toSequence b))

> doubleUpdate         :: Int -> Int -> a -> Seq( Seq a) -> Seq (Seq a)
> doubleUpdate x y a s =  update x (update y a (index s x)) s

> toSequence :: Board -> Seq (Seq Player)
> toSequence =  fromList.(map fromList)

> toBoard :: Seq (Seq Player) -> Board
> toBoard =  (map toList).toList

----------------------------------------------------------------------
Counting. These functions are used for turn checking.

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
Moves and input:

Make a move, from an Int specifiying the column, and taking a Player value to be
inserted and a Board to insert into.
Moves may fail, so returns a Maybe Board.
Moves fail if the Column specified doesn't exist or is full, so Nothing is returned.
Otherwise we make the move and return a Just Board.

> move       :: Int -> Player -> Board -> Maybe Board
> move x p b | x > cols-1          = Nothing
>            | next (b !! x) == -1 = Nothing
>            | otherwise           = Just (insertAt x (next (b !! x)) p b)

The input that a human makes might fail, so this is a wrapper for the move function that takes a Maybe Int

> maybeMove              :: Maybe Int -> Player -> Board -> Maybe Board
> maybeMove Nothing _ _  = Nothing
> maybeMove (Just x) p b = move x p b


As pieces in connect n are affected by gravity, we need some helper functions to find the next
available space in a column to insert into.
Free counts the number of free spaces in a column, and next uses this to get the index of the next available
space. As free returns a -1 if the column is full, we can also use this to check if a column is full.

> free    :: Col -> Int
> free    =  ((-)rows).length.dropWhile(==B)

get the next index to insert at:

> next    :: Col -> Int
> next c  =  (free c) - 1

Get input, display a prompt to the player who's turn it is and then ask for their move
Returns a Maybe Int as they may enter a value that is not an integer and readMaybe may fail:

> input      :: Player -> IO (Maybe Int)
> input p     =  do putStrLn("Enter your move player "++show p++":")
>                   line <- getLine
>                   let i = readMaybe(line)
>                   return i

----------------------------------------------------------------------
Game loop:
A board can be a win, loss, or full. Otherwise there are moves to make.
If it is the computers turn we call computer, which takes the board, applies the minimax algorithm
to make a move and produces an IO output.
Otherwise the board is passed to the player function which allows a human to make a move.

> play                        :: Board -> IO ()
> play b | winBoard X b       = showBoard b >> putStrLn "Player X won!"
>        | winBoard O b       = showBoard b >> putStrLn "Player O won!"
>		 | full b             = showBoard b >> putStrLn "Its a draw."
>        | compTurn b         = computer b
>		 | otherwise          = player b

Our main function just displays the empty board and calls our game loop, passing in the empty board.

> main   :: IO ()
> main   =  showBoard empty >> play empty

compTurn is just a helper function to help decide if a computer should play now or if it is a human's turn.

> compTurn   :: Board -> Bool
> compTurn b | turn b == X && cpuX = True
>            | turn b == O && cpuO = True
>            | otherwise           = False

If it is a player's turn the game prompts them for input, and passes this to maybeMove, 
if maybeMove returns Nothing then we know either the input failed or the move failed
so we call the game loop again with the original board.
Otherwise we show the new board by using fromJust to unwrap it and pass it back to
the game loop.

> player   :: Board -> IO ()
> player b =  do
>                let p = turn b
>                k  <- input p
>                let nb = maybeMove k p b
>                if nb == Nothing then
>                   putStrLn "Not a valid move." >>
>                   play b
>                else
>                   showBoard(fromJust nb) >>
>                   play (fromJust nb)

The computer player outputs a message so the player knows the computer is making its move.
It then hands the game playing to the compMove function which returns a board using the
minimax algorithm. It then displays this board and passes it back to the game loop.
The guards are for choosing between the regular version of minimax, implemented for the
coursework, or the version of minimax specified at the bottom of this file as a bonus
which uses an evaluation function as a heuristic for when the depth isn't sufficient
to find a winning move. This can be enabled in the bottom section of this file.

> computer   :: Board -> IO ()
> computer b 
>    | useHeuristic b = heuristicComputer b
>    | otherwise      = do 
>                         putStrLn "CPU is plotting its enemies' demise... "
>                         let nb = compMove b
>                         showBoard(nb)
>                         play(nb)

---------------------------------------------------------------------
Minimax:

Trees are Nodes with a value and a list of their child Nodes.
Deriving Show isn't necessary for the game, but does help for testing
as I worked on the game.

> data Tree a = Node a [Tree a]
>                    deriving Show


moves creates all the possible moves from a board reachable by a single move,
so the neighbours of the current board, 
catMaybes removes all the moves that would have been invalid.

> moves     :: Player -> Board -> [Board]
> moves p b =  catMaybes [move i p b | i <- [0..(cols-1)]]

growTree grows the tree by calling moves until a player has won or the board is full (when moves returns an empty list)

> growTree                                   :: Board -> Tree Board
> growTree b | winBoard X b || winBoard O b  =  Node b []
>            | otherwise                     =  Node b (map growTree (moves (turn b) b))

prune cuts the tree at a specified depth.

> prune               :: Int -> Tree a -> Tree a
> prune 0 (Node x _)  = Node x []
> prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax labels each leaf node (the base case) of the game tree with the label of which player wins, or B if no player wins.
These values are recursively propagated up the tree, with a parent node getting the value of its
minimum or maxium child (if its Os turn we pick the minimum value, X picks the maxiumum value)
The function returns a fully labelled game tree, with the root being the current board, labelled with which player
will win if both players play optimally. (or B when the winner is not yet determined as the tree isn't deep enough)

> minimax :: Tree Board -> Tree (Player, Board)
> minimax (Node b [])
>    | winBoard X b = Node (X,b) []
>  	 | winBoard O b = Node (O,b) []
>	 | otherwise    = Node (B,b) []
> minimax (Node b ts)
>    | turn b == O = Node (minimum players, b) ts'
>  	 | turn b == X = Node (maximum players, b) ts'
>	                 where
>					   ts'      = map minimax ts
>					   players  = [p | Node (p,_) _ <- ts']


The compMove function uses our minimax algorithm to pick a board from the child of the root of the gametree,
It picks the first board which is labelled the same as the root of the gametree.
As the gametree is always grown from the current board this root label will either be the minimum or maxiumum
move depending on who's turn it is.

> compMove    :: Board -> Board
> compMove b  =  head[b' | Node (p,b') _ <- ts, p == p']
>                where
>                  gametree = prune depth (growTree b)
>                  Node (p',_) ts = minimax gametree


----------------------------- BONUS ROUND ---------------------------
-----------------Minimax with evaluation function.-------------------
With sufficient depth minimax will always find winning moves, however at lower depths (or larger search spaces thanks to bigger boards and longer winning lengths)
it can be useful to have a method to compare non winning boards that are at the leaves of the game tree. As some non winning boards are more likely to lead to
winning boards than others. To represent this I wrote a simple evaluation function that counts the numbers of win-1 (3 in row) and win-2 (2 in a row) sequences for
the player and the opponent, so non winning board states can be compared with the basic idea that sequences of length win-2 and win-1 are likely to lead to sequences
of length win in a few number of moves. This heuristic is helpful for situations where we can't just brute force our way to a winning board as the depth required
gets too large.
When testing the CPU with the heuristic enabled will always beat the CPU with the heuristic disabled at the same depth, although it did lose against
some other agents using better evaluation functions. The parameters of the evaluation function were set by intuition and light testing, however they could be
optimised, perhaps using a technique like simulated annealing.
There are other methods that can applied, such as alpha-beta-pruning and iterative deepining to improve the perfomance of minimax, however I went with an
evaluation function as it can be neatly implemented with minimal changes to our existing minimax implementation. To see it in action you can set the following
flags to true, one for each CPU player:

---Modify these Boolean values---

> useHeuristicX :: Bool
> useHeuristicX =  False

> useHeuristicO :: Bool
> useHeuristicO =  False

---------------------------------

> useHeuristic   :: Board -> Bool
> useHeuristic b = (turn b == X && useHeuristicX) || (turn b == O && useHeuristicO)

> heuristicComputer   :: Board -> IO ()
> heuristicComputer b = do 
>                       putStrLn "CPU is plotting its enemies' demise... "
>                       let nb = hcompMove b
>                       showBoard(nb)
>                       play(nb)

For the minimax algorithm leaf nodes are either labled (X,n), (O,n) or (B,n), a tuple of (Player,Int).
where n = evaluation(X,b), so n will have a positive value if X is winning and a negative value if O is winning.
(O,0) < (B,n) < (X,0) thanks to how comparisons of tuples work.
These values are then propagated back up the tree by choosing the minimum (for player O) and maxium (for player X) p,v pairs.This implementation of minimax grows the tree down to the specified depth.

For example given the choice between two boards with the lablels (B,-4), (B,2) for Player O
the minimax algorithm would pick the board with the label (B,-4) as it is trying to minimise the evaluation function value.
Player O would also choose (X,-3) over (X,0) if it only had loosing moves to choose between, to try slow down its loss just like a human player would on the assumption that
the other (human) player may make a mistake.

> hminimax :: Tree Board -> Tree ((Player, Int), Board)
> hminimax (Node b [])
>    | winBoard X b = Node ((X,evaluation X b),b) []
>  	 | winBoard O b = Node ((O,evaluation X b),b) []
>	 | otherwise    = Node ((B,evaluation X b),b) []
> hminimax (Node b ts)
>    | turn b == O = Node (minimum players, b) ts'
>  	 | turn b == X = Node (maximum players, b) ts'
>	                 where
>					   ts'      = map hminimax ts
>					   players  = [(p,v) | Node ((p,v),_) _ <- ts']

compMove decides the best move for the CPU player to take by performing minimax on the gametree that has been grown to depth depth.
There may be multiple equally good moves so it just selects the first one, our evaluation function helps reduce the number of boards that are considered equal anyway.

> hcompMove    :: Board -> Board
> hcompMove b  =  head[b' | Node (pv,b') _ <- ts, pv == pv']
>                 where
>                   gametree = prune depth (growTree b)
>                   Node (pv',_) ts = hminimax gametree


evaluation calculates a value for a board based on the number of seqences of length win-1 and win-2 present in the board,
we only call it for boards that would normally just be labelled B, so leaves where no player has won, which is common at the start of the game.
We break this down into evaluating the board for both players, and calculating player a - player b. Our getCols, getRows, and getDiags functions allow us
to evaluate a column at a time and sum the results.
The "penultimateMult" factor determines how much more valuable our algorithm considers getting a sequence of win-1 over getting a sequence of win-2
The former will often take us to a board that is only one move away from winning, but the latter may open up more potential ways to win.
We consider the state of the board from the opponents perspective too as we want to also block them from forming sequences of length win-1 and win-2 that could
lead them to winning positions.

> penultimateMult :: Int
> penultimateMult =  win * 2

> evaluation     :: Player -> Board -> Int
> evaluation p b =  (evaluateBoard p b) - (evaluateBoard (opponent p) b)

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

> opponent   :: Player -> Player
> opponent X =  O
> opponent O =  X
> opponent B =  B