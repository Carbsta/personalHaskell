G52AFP Coursework 2 - Monadic Compiler

Thomas Cameron Gregory Dudley
Your full email address(es)

--------------------------------------------------------------------------------

> import Data.List (elemIndices)
> import Control.Lens ((^?),element)
> import Data.Maybe


Imperative language:

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

State monad  (I made it generic so it can also be used for execution of the machine):

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

Compilation

> compexpr  :: Expr -> Code
> compexpr (Val a) = [PUSH a]
> compexpr (Var n) = [PUSHV n]
> compexpr (App op ex1 ex2) = (compexpr ex1 ++ compexpr ex2) ++ [DO op]


> compprog :: Prog -> ST Label Code
> compprog (Seq [])        = return []
> compprog (Assign n expr) = return (compexpr expr ++ [POP n])
> compprog (If expr p1 p2) = do l <- fresh
>                               l' <- fresh
>                               ifn <- compprog p1
>                               elsen <- compprog p2
>                               return (compexpr expr ++ [JUMPZ l] ++ ifn ++ [JUMP l'] ++ [LABEL l] ++ elsen ++ [LABEL l'])
> compprog (While expr p)  = do l <- fresh
>                               l'<- fresh
>                               sub <- compprog p
>                               return ([LABEL l] ++ compexpr expr ++ [JUMPZ l'] ++ sub ++ [JUMP l, LABEL l'])
> compprog (Seq (p:ps))    = do c <- compprog p
>                               cs <- compprog (Seq ps)
>                               return (c ++ cs)


> comp :: Prog -> Code
> comp p = fst(app (compprog p) 0)


> fresh :: ST Label Label
> fresh =  S (\n -> (n, n+1))


--------------------------------------------------------------------------------

Execution:

> exec   :: Code -> Mem
> exec c =  mem(snd(app (run) M {stack = [], mem = [], pc = 0, code = c}))

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


next and jump are functions for manipulating the program counter:

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

ipop is used internally when passing the integer values around,
e.g to then be used in a Do operation:

> ipop :: ST Machine Int
> ipop = do x:xs <- get stack
>           update (\m -> m {stack = xs})
>           return x

rmem is used to get the memory at the specified name,
the memory value returned is empty if there is 
nothing stored at that address.

> rmem   :: Name -> ST Machine Mem
> rmem n =  do me <- get mem
>              return (filter ((==n).fst) me)

The following functions are all called by step,
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
> pushv n = do (n',i):_ <- rmem n
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