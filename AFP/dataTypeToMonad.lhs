Consider the parameterised type

> data Expr a = Var a
>             | Val Int
>             | Add (Expr a) (Expr a)

lets turn it into a monad!

> instance Monad Expr where
>   --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
> (Var v)   >>= f = f v
> (Val n)   >>= f = Val n
> (Add x y) >>= f = Add (x >>= f)(y >>= f)


val n :: Expr a
f :: a -> Expr b

so we can't apply the function to Val n, so just copy it accross

x,y :: Expr a
f :: a -> Expr b
so lets use the bind, (x >>= f) (y >>= f)

What does this monad do?
Substitution!
It substitutes the variables in the expression with something else.

e.g 

> f :: Char -> Expr a
> f 'x' = Add (Val 1)(Val 2)
> f 'y' = Val 3

lets apply this to an expression

> test :: Expr Char
> test = Add (Var 'x')(Var 'y')

Add(Var 'x')(Var 'y') >>= f

Add(Add(Val 1)(Val 2))(Val 3)