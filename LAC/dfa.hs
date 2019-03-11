type Dfa = (Q, Alphabet, Delta, Initial, Final)

type Set a = [a]
type Q = Set State
type Alphabet = Set Symbol
type Sigmastar = Set Words
type Language = Set Words
type Delta = (State, Symbol) -> State
type Edelta = (State, Words) -> State
type Initial = State
type Final = Set State

type State = Int
type Symbol = Char
type Words = Set Symbol

oddDFA :: Dfa
oddDFA = (states, alphabet, delta, initial, final)

epsilon :: Words
epsilon =  []

alphabet :: Alphabet
alphabet =  ['0','1']

language :: Language
language =  [w | w <- astar, isFinal(edelta(initial, w))] 

astar :: Sigmastar
astar = sigmastar alphabet

sigmastar :: Alphabet -> Sigmastar
sigmastar xs = epsilon : concat (iterate genLists (map (\x -> [x]) xs))
  where
    genLists xss = [y : ys | y <- xs, ys <- xss]

isValid :: Words -> Bool
isValid w =  isFinal (edelta(initial, w))

isFinal :: State -> Bool
isFinal s =  elem s final

states :: Q
states =  [0, 1]

initial :: Initial
initial =  0

final :: Final
final =  [0]

delta :: Delta
delta (0, '0') = 1
delta (0, '1') = 0
delta (1, '0') = 0
delta (1, '1') = 1


edelta :: Edelta
edelta (q, x:w) = edelta(delta(q,x),w)
edelta (q, epsilon) = q