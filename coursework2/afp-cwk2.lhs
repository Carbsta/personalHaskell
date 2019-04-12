G52AFP Coursework 2 - Monadic Compiler

Thomas Cameron Gregory Dudley
psytcd@nottingham.ac.uk

--------------------------------------------------------------------------------

> import Data.List (elemIndices)
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

--------------------------------------------------------------------------------
compprog

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
WriterT machinery using the function Lift.

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

--------------------------------------------------------------------------------
compexpr

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
It comes with the initial cost of having to define a couple of generic helper 
functions to manipulate the stack, mem and pc values, 
but makes the actual exec and step functions far clearer.

exec is our highest level execution function that creates an initial machine
with an empty stack and memory, a program counter initialised to instruction 0
and the code of the machine set to the code passed into the function.
It then applies the run function to this machine, and then extracts the result
and wraps it up in a Mem.

> exec   :: Code -> Mem
> exec c =  mem(snd(app (run) M {stack = [], mem = [], pc = 0, code = c}))

--------------------------------------------------------------------------------
run

The run function simply runs a Machine until it terminates, which is determined
by when the program counter of the machine doesn't index the code.
When the program counter is less than length of the code, so still indexes an 
instruction,
step is called to handle the single instruction returned from co!!c,
updating the machine. The helper function next then increments the
program counter and run is called recursively.

> run  :: ST Machine ()
> run  = do co <- get code
>           c  <- get pc
>           if (c < length co) then 
>             step (co!!c) >>
>             next >>
>             run
>           else
>             update id         

--------------------------------------------------------------------------------
step

step executes a single instruction, a single step of the computation.
It takes an instruction and returns a ST Machine which contains
the modified Machine and returns nothing.

For a push instruction we simply update the machine with the value
on the front of the stack.

For a pushv instruction we first fetch this value from memory using the function
rmem (read memory) and then perform a normal push by simply calling step again
but with a push instruction and the value.

For a pop instruction we get the first value on the stack using the ipop function,
retrive the memory from the machine and then we have two cases:
If the variable is not in memory, we add it to the end of memory with the value.
Otherwise we replace where it occurs in memory with the new value.
An append could be avoided by placing new variables at the start of memory,
if we conceptualise memory growing backwards like a stack, however as per
the specification and for "niceness" variables appear in memory in the order
they are defined (and used) in the program, so we add them to the end
of the memory using an append.

for a do instruction we use ipop to get the top two values
from the stack. Then we apply the operator to these values,
and push this value to the stack, again reusing code by calling step.

for a jump instruction, we get the code from the machine,
and update the program counter with the index of the label instruction
that matches the jump instruction. the elemIndices function returns
occurences of an element in a list, and we just grab the first as there
will only ever be one LABEL X instruction in the code.

Jumpz is just jump with an extra step, first we check if the top
value of the stack is 0, using ipop, and if so we do a jump as before
(again, calling step to reuse code) if not, we do nothing for this step
by simply updating with the id function.

Label instructions do nothing, so we just update the machine
with the id function.

> step :: Inst -> ST Machine ()
> step (PUSH i)  = update (\m -> m {stack = i:stack m})
> step (PUSHV n) = do i <- rmem n
>                     step (PUSH i)
> step (POP n)   = do i <- ipop
>                     me <- get mem
>                     if ((lookup n me) == Nothing) 
>                     then update (\m -> m {mem = me++[(n,i)]})
>                     else update (\m -> m {mem = map (\p@(n',_) -> if n' == n then (n,i) else p) me})
> step (DO o)    = do i'<- ipop
>                     i <- ipop
>                     step (PUSH (op o i i'))
> step (JUMP l)  = do co <- get code
>                     update (\m -> m {pc = head(elemIndices (LABEL l) co)})
> step (JUMPZ l) = do i <- ipop
>                     if(i == 0) then step (JUMP l) else update id
> step (LABEL _) = update id

--------------------------------------------------------------------------------
next

next simply increments the program counter of the machine,
wrapped up inside the ST machinery using our update function.

> next :: ST Machine ()
> next = update (\m -> m {pc = (pc m)+1})

--------------------------------------------------------------------------------
get and update

get and update are generic functions that allow us to manipulate the
internal state of the machine. These two functions are the extra step
compared to just passing around the stack and memory without the state,
as step is defined interms of them.

get takes a function that takes a machine and returns a value 
(such as the ones provided by the record syntax we used to define Machine)
and returns this value wrapped up in our ST.

> get :: (Machine -> a) -> ST Machine a
> get f = S (\m -> (f m,m))

update allows us to apply any function that manipulates a Machine
to a Machine wrapped up in a ST, returning nothing as its purely
for updating the state of the Machine.

> update   :: (Machine -> Machine) -> ST Machine ()
> update f = S (\m -> ((),f m))

--------------------------------------------------------------------------------
ipop and rmem

ipop is used internally to get the top value of the stack,
but we don't want the memory to be updated like a normal pop,
for example to then be used in a Do operation.

> ipop :: ST Machine Int
> ipop = do x:xs <- get stack
>           update (\m -> m {stack = xs})
>           return x

rmem is used to get the memory at the specified name,
lookup is used to get the right value of a tuple with
the left value acting as a key, this value is wrapped
up in a Maybe value, since we don't need to worry about
runtime crashes we can just extract it with fromJust.
(otherwise we would have to propagate the error
through our higher level functions too).

> rmem   :: Name -> ST Machine Int
> rmem n =  do me <- get mem
>              return (fromJust (lookup n me))

--------------------------------------------------------------------------------
op

op takes an operator and two ints and applies the operator
in the order a Op b and returns the result.

> op     :: Op -> Int -> Int -> Int
> op Add = (+)
> op Sub = (-)
> op Mul = (*)
> op Div = div

EOF