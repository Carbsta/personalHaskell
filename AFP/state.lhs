The State Monad

We want to write functions that manipulate some form of state that
can be changed over time.
For simplicitiy lets just assume the sate is just and integer value.
(this can be modified of course)

> type State = Int

The most basic form of function we can perform on this type is a 
state transformer
, abbreviated ST, which takes an input state as its argument
and produces an output state as its result,
in which the output state reflects any updates that were made
to the state by the function during its execution.

	type ST = State -> State

However we may wish to return a result value as well as updating the state.
For example if the state represents a counter, a function that increments 
the counter may also wish to return the current counter's value.
So we generalise the type of state transformers to also return a result
value, with the type of such values being a parameter of the ST type.

	type ST a = State -> (a, State)

However, conversely, a state transformer may also wish to *take*
argument values. We don't need to change anything for this as 
we can just exploit currying.

Since ST is a parameterised type, its natural to try and make it
a monad so we can use the power of the do notation.
But, types declared by "type" cannot be made into instances of
classes. So we have to define ST using the newtype mechanism.
This requires introducing a dummy constructor, we will call S.

> newtype ST a = S (State -> (a,State))

Its convenient to define a special purpouse application function for this
type. Which simply removes the dummy constructor:

> app :: ST a -> State -> (a, State)
> app (S st) x = st x

To make this parameterised ST type into a monad, we have to first make
it into a functor. Easy:

> instance Functor ST where
>   -- fmap :: (a -> b) -> ST a -> ST b
>   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

inturn the type ST can then be made into an applicative functor

> instance Applicative ST where
>   -- pure :: a -> ST a
>   pure x = S (\s -> (x,s))
>
>   -- (<*>) :: ST (a -> b) -> ST a -> ST b
>   stf <*> stx = S (\s ->
>      let (f,s')  = app stf s
>          (x,s'') = app stx s' in (f x, s''))

Finally, the monadic instance for ST is declared as follows:

> instance Monad ST where
>    -- (>>=) :: ST a -> (a -> ST b) -> ST b
>    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

Lets do an example using this new State Transformer monad magic.

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>               deriving Show

A state is the next fresh integer we are gonna label the tree with.
The next such integer can be generated by derfining a state transformer
that returns the current state as its result, and the next integer as
the new state.

> fresh :: ST Int
> fresh =  S (\n -> (n, n+1))

in applicative style:
(g <$> x behaves as pure g <*> x)

> alabel :: Tree a -> ST (Tree Int)
> alabel (Leaf _) = Leaf <$> fresh
> alabel (Node l r) = Node <$> alabel l <*> alabel r

to test:

> tree :: Tree Char
> tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

we can also define an equivalent monadic version of the relabelling
function using the do notation

> mlabel :: Tree a -> ST (Tree Int)
> mlabel (Leaf _) = Leaf <$> fresh
> mlabel (Node l r) = do l' <- mlabel l
>                        r' <- mlabel r
>                        return (Node l' r')