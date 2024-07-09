(use /junk-drawer)
# you can create direct graphs and find paths through them!
# lets create a graph that looks like this
# +---+ 1 +---+ 8 +---+
# | A |-->| B |-->| C |
# +---+   +---+   +---+
#          2|   3^  |1
#           v   /   v
# +---+ 1 +---+   +---+
# | D |<--| E |   | F |
# +---+   +---+   +---+
#   |1              |1
#   v               v
# +---+ 1 +---+   +---+
# | G |-->| H |   | I |
# +---+   +---+   +---+
# note that nodes can have any arbitrary data in them
# edges can have an edge name (defaults to the "to" node in the edge) and weight (defaults to 1)
(var *graph*
     (digraph/create
      (digraph/node :a :x 0 :y 0) (digraph/node :b :x 1 :y 0) (digraph/node :c :x 2 :y 0)
      (digraph/node :d :x 0 :y 1) (digraph/node :e :x 1 :y 1) (digraph/node :f :x 2 :y 1)
      (digraph/node :g :x 0 :y 2) (digraph/node :h :x 1 :y 2) (digraph/node :i :x 2 :y 2)

      (digraph/edge :a :b)
      (digraph/edge :b :c 8) (digraph/edge :b :e 2)
      (digraph/edge :c :f)
      (digraph/edge :d :g)
      (digraph/edge :e :c 3) (digraph/edge :e :d)
      (digraph/edge :e :i)
      (digraph/edge :g :h)))

# you can add nodes and edges after creating graph
(:add-node *graph* (digraph/node :c2 :x 3 :y 0))
(:add-edge *graph* (digraph/edge :c :c2))

# then look at neighbors or other data of a node
(printf "does the graph contain the newly added node? %q" (:contains *graph* :c2))
(printf "neighbors for node C: %q" (:neighbors *graph* :c))

# you can also remove the nodes or edges if you don't want them any more 
(:drop-edge *graph* :c2)
(printf "neighbors for node C are different now: %q" (:neighbors *graph* :c))
(:drop-node *graph* :c2)
(printf "does the graph contain the previously added node? %q" (:contains *graph* :c2))

# you can even find the path (really the edges) from one node to another
(printf "edges to get from node A to node I: %q" (:find-path *graph* :a :i))
