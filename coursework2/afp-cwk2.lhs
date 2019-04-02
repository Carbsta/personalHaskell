G52AFP Coursework 2 - Monadic Compiler

Thomas Cameron Gregory Dudley
psytcd@nottingham.ac.uk

--------------------------------------------------------------------------------

> import Data.List (elemIndices)
> import Control.Lens ((^?),element)
> import Data.Maybe
> import Control.Monad.Writer


Imperative language:
This is the specification provided by Graham, although I have also
made Op derive Eq so the Inst type can derive Eq to allow my jump
function to lookup 

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seq [Prog]
>             deriving Show
>
> data Expr = Val Int | Var Name | App Op Expr Expr
>             deriving Show
>
> type Name = Char
>
> data Op   = Add | Sub | Mul | Div
>             deriving (Show, Eq)

Factorial example:

> fac :: Int -> Prog
> fac n = Seq [Assign 'A' (Val 1),
>              Assign 'B' (Val n),
>              While (Var 'B') (Seq
>                 [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>                  Assign 'B' (App Sub (Var 'B') (Val (1)))])]

Virtual machine:
I defined a datatype Machine to wrap up all the fields of the machine, using the record syntax.
This gives us some some nice functions for accessing all parts of our machine as well as avoiding
having to pass around all the individual components.
PC is a program counter that allows the machine to keep track of where it is in the code,
and is used to index the code.

> data Machine = M { stack :: Stack,
>                    mem   :: Mem,
>                    pc    :: PC,
>                    code  :: Code }
>                    deriving Show
>
> type Stack = [Int]
>
> type Mem   = [(Name,Int)]
>
> type Code  = [Inst]
>
> type PC  = Int
> 
> data Inst  = PUSH Int
>            | PUSHV Name
>            | POP Name
>            | DO Op
>            | JUMP Label
>            | JUMPZ Label
>            | LABEL Label
>              deriving (Show, Eq)
> 
> type Label = Int

State monad as supplied by Graham,
with the minor modification to parameterise the type.
This allows us to represent our Machine as a state.

> newtype ST s a = S (s -> (a, s))
>
> app :: ST s a -> s -> (a, s)
> app (S st) x 	=  st x
>
> instance Functor (ST s) where
>    -- fmap :: (a -> b) -> ST a -> ST b
>    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))
>
> instance Applicative (ST s) where
>    -- pure :: a -> ST a
>    pure x = S (\s -> (x,s))
>
>    -- (<*>) :: ST (a -> b) -> ST a -> ST b
>    stf <*> stx = S (\s ->
>       let (f,s')  = app stf s
>           (x,s'') = app stx s' in (f x, s''))
>
> instance Monad (ST s) where
>    -- return :: a -> ST a
>    return x = S (\s -> (x,s))
>
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

--------------------------------------------------------------------------------

Compilation, using the writer monad.
We use WriterT to allow us to still use the state monad.

comp is simply a high level function that applies the result of execWriterT 
to an initial label which we specify as 0.
execWriterT extracts the result of a writer computation, which in our case
is the result of compprog p.
fst then extracts the code from the resulting tuple as we no longer care
about the label.

> comp   :: Prog -> Code
> comp p =  fst(app (execWriterT (compprog p)) 0)

compprog is a recursive function for compiling the code a token at a time.
The two base cases are either when you have an empty sequence of programs,
Seq [] or you have an Assign token.
The recursive cases are the If, While and Non empty Seq.
All of these statements contain more Programs that get passed to compprog.
Expressions, in the Assign, If, and While statements are passed to compexpr.
The function calls tell to build up a log using the WriterT monad, which is
then accessed by calling execWriterT on the returned WriterT.
A StateTransformer is used internally to increment the label by generating
fresh labels. The function fresh generates a fresh label, wrapped up in the
WriterT machinary using the function Lift.

> compprog :: Prog -> WriterT Code (ST Label) ()
> compprog (Seq [])        = return ()
> compprog (Assign n expr) = do compexpr expr
>                               tell [POP n]
> compprog (If expr p1 p2) = do l <- fresh
>                               l'<- fresh
>                               compexpr expr
>                               tell [JUMPZ l]
>                               compprog p1
>                               tell [JUMP l', LABEL l]
>                               compprog p2
>                               tell [LABEL l']
> compprog (While expr p)  = do l <- fresh
>                               l'<- fresh
>                               tell [LABEL l]
>                               compexpr expr
>                               tell [JUMPZ l']
>                               compprog p
>                               tell [JUMP l, LABEL l']
> compprog (Seq (p:ps))    = do compprog p
>                               compprog (Seq ps)

compexpr takes an expression and returns a WriterT Code monad
which contains the StateTransformer as well. It uses tell
to build the log of the WriterT like in compprog.
It is recursive, with the base cases being Var and Val expressions,
and the recursive case being an App expression.

> compexpr  :: Expr -> WriterT Code (ST Label) ()
> compexpr (Val a) = tell [PUSH a]
> compexpr (Var n) = tell [PUSHV n]
> compexpr (App op ex1 ex2) = do compexpr ex1 
>                                compexpr ex2
>                                tell [DO op]

fresh generates a new label, I had to include lift
to wrap it up in a WriterT monad.

> fresh :: WriterT Code (ST Label) Label
> fresh =  lift(S(\s -> (s, s+1)))


--------------------------------------------------------------------------------

Execution:

To avoid passing around respresentations of memory, stack and a program counter
through convoluted curried functions, I decided to take the more conceptually
clear approach of respresenting a Machine as a State and using the ST monad
to pass this state through the execution.
I intially wrote it the more "hacky" way of passing memory, stack etc around 
in the same way that I initally wrote the compilation functions without
the state monad, but in my opinion this monadic version is conceptually better
as it makes sense to think about our Virtual Machine as type of State Machine.
It comes with the initial cost of having to define a couple of generic helper functions
to manipulate the stack, mem and pc values, but makes the actual exec and step
functions far clearer.

exec is our highest level execution function that creates and initial machine
with an empty stack and memory, a program counter initialised to instruction 0
and the code of the machine set to the code passed into the function.
It then applies the run function to this machine, and then extracts the result
and wraps it up in a Mem.

> exec   :: Code -> Mem
> exec c =  mem(snd(app (run) M {stack = [], mem = [], pc = 0, code = c}))

The run function simply runs a Machine until it terminates, which is determined
by when the program counter of the machine doesn't index the code. We use two
functions from the Lens library, ^? and element, to index the code.
if the program counter is not an index of the code l is Nothing and the machine
is unchanged. (by calling update with the id function)
when l is not nothing, so is an instruction, step is called to handle the single
instruction, updating the machine, the helper function next then increments the
program counter and run is called recursively.

> run  :: ST Machine ()
> run  = do co <- get code
>           c  <- get pc
>           let l = co ^? element c
>           if (l /= Nothing) then 
>             step (fromJust l) >>
>             next >>
>             run
>           else
>             update id         



> step :: Inst -> ST Machine ()
> step (PUSH i)  = push i
> step (PUSHV n) = pushv n
> step (POP n)   = pop n
> step (DO o)    = doo o
> step (JUMP l)  = jump l
> step (JUMPZ l) = do i <- ipop
>                     if(i == 0) then jump l else update id
> step (LABEL _) = update id


next and jump are helper functions for manipulating the program counter:

> next :: ST Machine ()
> next = update (\m -> m {pc = (pc m)+1})

> jump  :: Label -> ST Machine ()
> jump l = do co <- get code
>             update (\m -> m {pc = head(elemIndices (LABEL l) co)})

get and update are generic functions that allow us to manipulate the
internal state of the machine:

> get :: (Machine -> a) -> ST Machine a
> get f = S (\m -> (f m,m))

> update   :: (Machine -> Machine) -> ST Machine ()
> update f = S (\m -> ((),f m))

ipop is used internally when passing the integer values around, but we don't want any side effects with memory,
e.g to then be used in a Do operation:

> ipop :: ST Machine Int
> ipop = do x:xs <- get stack
>           update (\m -> m {stack = xs})
>           return x

rmem is used to get the memory at the specified name,
the memory value returned is empty if there is 
nothing stored at that address.

> rmem   :: Name -> ST Machine Int
> rmem n =  do me <- get mem
>              return (fromJust (lookup n me))

The following helper functions are all called by step,
and have been taken out of the step function for clarity:

> pop   :: Name -> ST Machine ()
> pop n = do i <- ipop
>            me <- get mem
>            let xs = takeWhile(nmatch) me
>                ys = drop 1 $ dropWhile(nmatch) me
>                nmatch = ((/=n).fst)
>            update (\m -> m {mem = xs ++ (n,i): ys})


> push :: Int -> ST Machine ()
> push i = update (\m -> m {stack = i:stack m})

> pushv  :: Name -> ST Machine ()
> pushv n = do i <- rmem n
>              push i

> doo   :: Op -> ST Machine ()
> doo o =  do i'<- ipop
>             i <- ipop
>             push (op o i i')

op takes an operator and two ints and applies the operator
in the order a Op b:

> op     :: Op -> Int -> Int -> Int
> op Add = (+)
> op Sub = (-)
> op Mul = (*)
> op Div = div