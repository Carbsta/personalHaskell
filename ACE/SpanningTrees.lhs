Spanning tree.

connected, undirected graph -> tree

output tree connects all the vertices in the graph using only the edges present in the graph.

Minimum Spanning Tree.

connected, undirected, weighted graph -> spanning tree

output spanning tree is miniumum in the sense that the sum of weights of the edges is the smallest for any
spanning tree.

Why is a MST a tree?
- if a graph is connected and acyclic then it is a tree.

we want a minium spanning subgraph: a subset of the edges that is connected and contains every node.
assuming all weights are non negative, then if the graph has a cycle then we can remove an edge of the cycle.
The graph will still be connected, and have a smaller weight. Once finished there are no cylces left, so its a tree.

Don't confuse a minimum TREE with a "minimum" (shortest) PATH
finding shortest path is a TSP problem, much harder.


> type Graph a = [Vertex a]

> type Vertex a = (a, Adjacenties a)

> type Adjacenties a = [Edge a]

> type Edge a = (a, Weight)

> type Weight = Int

> example :: Graph Char
> example = [('A',[('E',10),('C',12),('B',8)]),
>            ('B',[('A',8),('C',6)]),
>            ('C',[('B',6),('A',12),('E',6),('D',3)]),
>            ('D',[('E',6),('D',3)]),
>            ('E',[('A',10),('C',6),('D',6)])]

> test :: Graph Char
> test = [('A',[('E',10),('C',12),('B',8)])]

> test2 :: Graph Char
> test2 = [('A',[('E',10),('C',12),('B',8)]),
>         ('B',[('A',8),('C',6)])]

Prim's algorithm:

pick any vertex M,
choose shortest edge from M to any other vertex N
add edge (M,N) to the MST

Loop: continue to add at every step a shortest edge, from a vertex in MST to a vertex outside.
Until all vertices are in MST

(if there are multiple shortest then just pick one arbitrary vertex)

Greedy: just adds a minimum weight edge.
without worrying about the overall structure / without looking ahead.
-> makes a logically optimal choice at each step.

> prim :: (Eq a) => Graph a -> Graph a
> prim [] = []
> prim ((v,es):vs) = 

> neighbours :: (Eq a) => [Vertex a] -> [Edge a]
> neighbours [] = []
> neighbours ((v,es):vs) = filter ((`notElem` (values ((v,es):vs))).fst)(es ++ (neighbours vs))

> values :: Graph a -> [a]
> values [] = []
> values ((v,_):vs) = v : values vs
