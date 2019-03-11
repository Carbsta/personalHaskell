The Language

> data Prog = Assign Name Expr
>           | If Expr Prog Prog
>           | While Expr Prog
>           | Seq [Prog]

> data Expr = Val Int
>           | Var Name
>           | App Op Expr Expr


To simplify things we are saying you can only have single character variable names

> type Name = Char

> data Op = Add | Sub | Mull | Div

In our language we will use 0 as False, same convention as C/Java

Example: factorial of n (Don't worry about syntax, don't need parsing)

begin
A := 1;
B := n;
while B do
	begin
	A := A * B;
	B := B - 1
	end
end

How are we going to input our programs? (since we won't be doing parsing)
- As a sequence of instructions.

Seq [Assign 'A' (Val 1),
	 Assign 'B' (Val n),
	 While (Var 'B')(Seq [
	 	Assign 'A'(App Mul(Var 'A')(Var 'B')),
	 	Assign 'B'(App Sub(Var 'B')(Val 1))]
	 )]

-----------------------------------------------------------------------------------
Virtual Machine

our machine shall use a Stack

> type Stack = [Int]

where the head of the list is the top of the stack:
s = [1,2,3] 
1 is on the top of the stack. (very convenient in Haskell)

Our machine also needs some memory

> type Mem = [(Name,Int)]

Ours is a list of addresses, with names, and values at the address.

Our machine also needs some code, an instruction set.

> type Code = 

> data Inst = PUSH Int
>           | PUSHV Name
>           | POP Name
>           | DO Op
>           | JUMP Label
>           | JUMPZ Label
>           | LABEL Label

> type Label = Int

labels are just markers for statements of code, that we can use for jumping around.

push takes an integer and pushes it to the top of the stack.
pushv does the same, but instead takes a variable name, looks it up in the memory and then
pushes this value onto the stack.
pop is how our assignment works, it takes a value off the top of the stack,
and puts it into the variable with the given name.
Do takes two values off the stack, and applies the operator to them, then pushes the result to the stack.
 e.g if the stack is [1,4,3] and we DO + then we do 1 + 4 and push to the stack, which becomes [5,3]
Jump takes the top value from the stack and jumps to the location label.
Jumpz takes the top value from the stack, if it is zero it jumps, otherwise continues.

Example. How do we do our factorial program as instructions.

A := 1  --> PUSH 1, POP 'A',
B := n  --> PUSH n, POP 'B',
while B do  --> LABEL 0
            --> PUSHV 'B', JUMPZ, 1
    A := A * B    --> PUSHV 'A', PUSHV 'B', DO Mul, POP 'A',
    B := B - 1    --> PUSHV 'B', PUSH 1, DO Sub, POP 'B',
            --> JUMP 0,
        --> LABEL 1

--------------------------------------------------------------------------------------------
Execution Function

exec :: Code -> Mem

e.g exec(comp(fac 10))

= [('A',3628800),('B',0)]


Hints and tips, have look at the tree relabling example with state monads to make sure you are comfortable.
Will need the state monad for the compiler, handling labels.
(although try write the compiler without monads first)

Suggestions: write a compiler for expressions first:

compexpr :: Expr -> Code

then (without monads)

compprog :: Prog -> Label -> (Code, Label)

label is behaving like a state, so a nicer way to write this function is using monads to produce:

compprog :: Prog -> ST Code

lets think about how compexpr would work. 
given the expression (1+2)*(3+4)
we would compile the left hand side recursively, then the right hand side recursively then the mult operator:

PUSH 1
PUSH 2
DO ADD

PUSH 3
PUSH 4
DO ADD

DO MUL

So for programs, what if we had a while loop
While e do p --->

LABEL L1
code for e
JUMPZ L2
code for p
JUMP L1
LABEL L2

and how about If statements:

If e then p else q --->

code for e
JUMPZ L1
code for p
JUMP L2
LABEL L1
code for q
LABEL L2