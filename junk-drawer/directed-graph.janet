(setdyn :doc ```
A directed graph is a collection of nodes with edges, in which the edges have a
direction. The implimentation here uses an adjaceny matrix under the hood, and
has these additions...

- Edges may also have a unique name and weight.
- Nodes may have arbitrary data in them.

Nodes and edges are created using their respective macros (node) and (edge).
A new graph is created using the (create) function.

The following graph functions are public in this module, and are also added to the
graph objects metatable if you prefer a OoP style.

 - contains
 - add-node
 - add-edge
 - get-node
 - neighbors
 - list-nodes
 - list-edges
 - find-path

check out the docs on any of those macros/functions for more.
```)

(defn node
  ```
  Create a node to be used in the "create" or "add-node" functions. Provide
  the node name, and any number of key value pairs for the node data. Name
  must be a keyword.

  (node :name
    :a "pizza"
    :b (fn [] "hotdog"))
  ```
  [name & properties]
  [:node (keyword name) {:edges @{} :data (table ;properties)}])

(defn edge
  ```
  Create an edge to be used in the "create" or "add-edge" functions. Can be
  any of these forms. note that weight defaults to 1, and the edge name defaults
  to the to-node name.

  - (edge :edge-name :from-node :to-node weight)
  - (edge :edge-name :from-node :to-node)
  - (edge :from-node :to-node weight)
  - (edge :from-node :to-node)
  ```
  [& pattern]
  (match pattern
    [(name (keyword? name)) (from (keyword? from)) (to (keyword? to)) (weight (number? weight))]
    [:edge from {:to to :name name :weight weight}]

    [(name (keyword? name)) (from (keyword? from)) (to (keyword? to))]
    [:edge from {:to to :name name :weight 1}]

    [(from (keyword? from)) (to (keyword? to)) (weight (number? weight))]
    [:edge from {:to to :name to :weight weight}]

    [(from (keyword? from)) (to (keyword? to))]
    [:edge from {:to to :name to :weight 1}]))

(defn contains [self name]
  ```
  Return whether or not the node :name exists in the graph.
  ```
  (not (nil? (get-in self [:adjacency-table name]))))

(defn add-node
  ```
  Add a node to the graph. Throws error if node already exists in the graph.
  Should use the "node" macro for to create the new node.
  ```
  [self [NODE name node-def]]

  (if (:contains self name)
    (errorf "graph already contains node %s" name)
    (put-in self [:adjacency-table name] node-def)))

(defn add-edge
  ```
  Add a new edge to the graph. You should use the "edge" macro to create the new edge.
  Both from and to nodes must exist.
  ```
  [self [EDGE from {:to to :name name :weight weight}]]

  (cond (not (:contains self from))
        (errorf "graph does not contain from node %s" from)

        (not (:contains self to))
        (errorf "graph does not contain to node %s" to)

        (put-in self [:adjacency-table from :edges name]
                {:to to :weight weight})))

(defn drop-edge 
  ```
  Remove an edge from the graph.
  ```
  [self name]
  (eachk node (self :adjacency-table)
    (put-in self [:adjacency-table node :edges name] nil)))

(defn drop-node 
  ```
  Remove a node and all edges related to it from a graph.
  ```
  [self name]
  
  (put-in self [:adjacency-table name] nil)

  (eachk node (self :adjacency-table)
    (eachk edge (((self :adjacency-table) node) :edges)
      (when (= name (get-in self [:adjacency-table node :edges edge :to]))
        (put-in self [:adjacency-table node :edges edge] nil)))))

(defn neighbors
  ```
  Return all the neighbors of the node, in the form {:from :name :to :weight}
  ```
  [self from-name]

  (if-let [node (get-in self [:adjacency-table from-name])
           edges (get node :edges)]
    (map (fn [(name data)]
           {:from from-name
            :name name
            :to (get data :to)
            :weight (get data :weight)})
         (pairs edges))
    []))

(defn get-node
  "The data & edges for the provided node name."
  [self name]
  (get-in self [:adjacency-table name]))

(defn list-nodes
  "Names of all the nodes in the graph."
  [self]
  (keys (self :adjacency-table)))

(defn list-edges [self]
  "Return all the edges in the graph in the form {:from :name :to :weight}."
  [;(mapcat (fn [(from-node-name node)]
            (map (fn [(edge-name edge)]
                   (table/to-struct
                    (merge {:from from-node-name :name edge-name}
                           edge)))
                 (pairs (get node :edges))))
          (pairs (self :adjacency-table)))])

(defn- priority-push [priority-queue data weight]
  (array/push priority-queue [data weight]))

(defn- priority-pop [priority-queue]
  (var lowest-i 0)
  (var lowest-weight 0)

  (for i 0 (length priority-queue)
    (when-let [[data weight] (get priority-queue i)
               is-lower (< weight lowest-weight)]
      (set lowest-i i)
      (set lowest-weight weight)))

  (let [(data weight) (get priority-queue lowest-i)]
    (array/remove priority-queue lowest-i 1)
    data))

(defn find-path
  ```
  Find the shortest path from the "start" node to "end" node. Uses a breadth first
  search which takes into account edge weights and exits out early if goal is found.
  Returns a list of the edge-names to take from start to reach the goal.

  You can also provided an optional (fn heuristic [graph goal-name next-node-name] number)
  function to further improve the search. Consider using the vector module with your node and
  using a heuristic like this:

  (node :name-here :position (vector/new 1 3))

  (defn manhattan-distance-heuristic [graph goal next]
    (let [{:data goal-data} (:get-node graph goal)
          {:data next-data} (:get-node graph next)]
      (:length2 (goal-data :position)
                (next-data :position))))
  ```
  [self start goal &opt heuristic]

  (default heuristic (fn [& rest] 0))
  (let [frontier @[[start 0]]
        came-from @{} # path A->B is stored as (came-from B) => A
        cost-so-far @{start 0}]

    (var current nil)
    (while (and (array/peek frontier) (not= current goal))
      (set current (priority-pop frontier))
      (loop [{:name edge-name :to next :weight weight} :in (:neighbors self current)
             :let [new-cost (+ (cost-so-far current) weight)]
             :when (or (nil? (get came-from next))
                       (< new-cost (cost-so-far next)))]
        (put cost-so-far next new-cost)
        (priority-push frontier next (+ new-cost (heuristic self goal next)))
        (put came-from next {:from current :edge-name edge-name})))


    # follow the came-from backwards from the goal to the start.
    (var current {:from goal})
    (let [path @[]]
      (while (not= (get current :from) start)
        (array/push path current)
        (set current (get came-from (get current :from))))
      (array/push path current)

      (->> (reverse path)
           (map |(get $ :edge-name))
           (filter |(not (nil? $)))
           (splice)
           (tuple)))))

(def Graph
  @{:contains contains
    :add-node add-node
    :add-edge add-edge
    :drop-node drop-node
    :drop-edge drop-edge 
    :get-node get-node
    :neighbors neighbors
    :list-nodes list-nodes
    :list-edges list-edges
    :find-path find-path
    :adjacency-table @{}})

(defn create [& patterns]
  ```
  Instantiate a new directed graph with optional starting nodes and edges.
  See "node" and "edge" macros for more.

  (create
   (node :red)
   (node :green
      :key "val"
      :say (fn [self] "hello world"))
   (edge :red :green)
   (edge :panic :green :red 2)) # override name and weight
  ```
  (let [graph (table/setproto @{:adjacency-table @{}} Graph)
        nodes (filter |(= :node (first $)) patterns)
        edges (filter |(= :edge (first $)) patterns)]

    (each n nodes (:add-node graph n))
    (each e edges (:add-edge graph e))

    graph))
