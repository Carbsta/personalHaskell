Flattening a tree

> data Tree = Leaf Int | Node Tree Tree

first one:

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

How can we elimate the ++

the trick, in the recursive case we are doing some flattening and appending,
so we could define a more general function that combines the behaviour of flatten and ++

our property/specification: flatten' t ns = flatten t ++ ns

what is the definition of flatten' ?
we want to solve this equation for flatten'
we can do it with an inductive proof, giving us 
the base and recursive case.

Base Case:

flatten' (Leaf n) ns
= spec
flatten (Leaf n) ++ ns
= applying flatten
[n] ++ ns
= applying ++
n:ns

Hence, the base case is true if we define:
	flatten' (Leaf n) ns = n:ns

Inductive Case:

flatten' (Node l r) ns
= spec
flatten (Node l r) ++ ns
= applying flatten
(flatten l ++ flatten r) ++ ns
= associativity of ++
flatten l ++ (flatten r ++ ns)
= by the inducton hypothesis for r
flatten l ++ (flatten' r ns)
= by the induction hypothesis for l
flatten' l (flatten' r ns)

in conclusion we have defined a new function:

> flatten' :: Tree -> [Int] -> [Int]
> flatten' (Leaf n) ns   = n:ns
> flatten' (Node l r) ns = flatten' l (flatten' r ns)

hence we can redefine flatten:

> flatten :: Tree -> [Int]
> flatten t = flatten' t []

onwards!

A simple compiler

Language (Hutton's Razor):

> data Expr = Val Int | Add Expr Expr

Semantics:

> eval :: Expr -> Int
> eval (Val n) = n
> eval (Add x y) = eval x + eval y

Virtual machine:

> type Stack = [Int]

> type Code = [Op]

> data Op = PUSH Int | ADD
>           deriving Show

Semantics:

> exec :: Code -> Stack -> Stack
> exec []         s   = s
> exec (PUSH n:c) s   = exec c (n:s)
> exec (ADD:c)(m:n:s) = exec c (n+m : s)

Compiler (original)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

But, our compiler uses append! (will come back to this)

Compiler correctness:

let e = Add (Add (Val 2)(Val 3))(Val 4)

exec (comp e) [] = [eval e]

this is too specific though, our induction won't work, we need to generalise further:

exec (comp e) s = eval e : s

this is our compiler correctness theorem

Expr ----------> Int
 |      eval      |
 |                |
 |                | 
 |comp    :)      | (:s)
 |                |
 |                |
 v   (`exec` s)   v
Code ----------> Stack

as a diagram ^

:) means it commutes.

Proving this theorem:

Induction on expression e.

Base case: e = Val n

exec (comp (Val n)) s
= {applying definition of comp}
exec [PUSH n] s
= {applying definition of exec}
n : s
= {unapplying definition of eval}
eval (Val n) : s

Inductive Case: e = Add x y

exec (comp (Add x y))
= {applying definition of comp}
exec (comp x ++ comp ++ [ADD]) s
= {associativity of append}
exec (comp x ++ (comp y ++ [ADD])) s
= {using our distributivity lemma}
exec (comp y ++ [ADD]) (exec (comp x) s)
= {applying our correctness theorem}
exec (comp y ++ [ADD])(eval x : s)
= {applying distributivity lemma}
exec [ADD] (exec (comp y)(eval x : s))
= {applying our correctness theorem}
exec [ADD] (eval y : eval x : s)
= {applying the definition of exec}
(eval x + eval y) : s
= {unapplying the definition of eval}
eval (Add x y) : s


Distributivity Lemma:

exec (c ++ d) s = exec d (exec c s)

proof by induction on c, assuming the stack never underflows.
(see book for the proof)

Problem, this proof has 22 steps.
(base case, inductive case, distributiviy lemma, associativity of append lemma)
2 line program, 22 steps of proof.
Is this scalable??? (Imagine proving a C compiler)

How can we make this better? Remove append!

Trick: use an accumulator again!

> comp :: Expr -> Code
> comp e = comp' e []

> comp' :: Expr -> Code -> Code
> comp' (Val n)   c = PUSH n : c
> comp' (Add x y) c = comp x (comp' y (ADD : c))

specification for this new compiler:
(we can use this spec and induction to arrive at the code above.)

comp' e c = comp e ++ c

Let us generalise our compiler correctness theorem

exec (comp e) s = eval e : s

generalises to:

exec (comp' e c) s = exec c (eval e : s)

Proof through induction on e again.

Base Case:
exec (comp' (Val n) c) s

exec(comp' (Val n) c) s
= {applying comp'}
exec (PUSH n : c) s
= {applying exec}
exec c (n : s)
= {unapplying eval}
exec c (eval (Val n) : s)

Inductive case:
exec (comp' (Add x y) c) s

exec (comp' (Add x y) c) s
= {applying comp'}
exec (comp' x (comp' y (ADD : c))) s
= {applying our hypothesis for x}
exec (comp' y (ADD : c)) (eval x : s)
= {applying our hypothesis again}
exec (ADD : c) (eval y : eval x : s)
= {applying exec}
exec c ((eval x + eval y) : s)
= {unapplying eval}
exec c (eval (Add x y) : s)

some main benefits:

22 step proof has now become an 8 step proof.
No lemmas used.
use of ++ has vanished. (much more efficient!)
potentional stack underflow error has vanished. (In the distributivity lemma)

So what is the take away point?

Mathematics is an excelent tool for guiding the development of efficient programs
with simple proofs.
