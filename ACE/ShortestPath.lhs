Dijstra's

> import Data.Graph

example graph

> elist :: [Edge]
> elist =  [(0,1),(0,2),(2,3),(3,1)]

> ebounds :: Bounds
> ebounds = (0,3)

> egraph :: Graph
> egraph = buildG ebounds elist