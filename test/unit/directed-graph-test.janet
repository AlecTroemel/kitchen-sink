(import spork/test)
(use /junk-drawer/directed-graph)

(test/start-suite 0)
(let [graph (create
              (node :red)
              (node :green
                    :key "val"
                    :say (fn [self] "hello world"))
              (edge :red :green)
              (edge :panic :green :red 2))]
  (test/assert (and (:contains graph :red)
                    (:contains graph :green))
               "graph init creates provided nodes")

  (:add-node graph (node :blue :another "data"))
  (test/assert (:contains graph :blue)
               "contains returns true for just added node")

  (test/assert (= (get-in (:get-node graph :blue) [:data :another]) "data")
               "get-node returns the node and contains the provided data")

  (:add-edge graph (edge :blue :red 3))
  (test/assert (not (nil? (get-in (:get-node graph :blue) [:edges :red])))
               "add-edge added the edge")

  (test/assert (= (first (:neighbors graph :blue))
                  {:from :blue :name :red :to :red :weight 3})
               "neighbors returns neighbors")

  (test/assert (= (length (:list-nodes graph)) 3)
               "there are 3 nodes in the graph")

  (test/assert (= (length (:list-edges graph)) 3)
               "there are 2 edges in the graph")

  (test/assert (= (:list-edges graph)
                  [{:from :red :name :green :to :green :weight 1}
                   {:from :blue :name :red :to :red :weight 3}
                   {:from :green :name :panic :to :red :weight 2}])
               "edges list uses correct data format"))
(test/end-suite)


(test/start-suite 1)
(let [graph (create (node :a) (node :b) (node :c)
                    (node :g)
                    (node :d) (node :e) (node :f)

                    (edge :a :b)
                    (edge :b :d)
                    (edge :b2g :b :g 10)
                    (edge :c :f)
                    (edge :d :e)
                    (edge :e2g :e :g)
                    (edge :g :c))]

  (test/assert (= (:find-path graph :a :f)
                  [:b :d :e :e2g :c :f])
               "should find correct path"))
(test/end-suite)

# Test if we can use forms for node data and edges info
(test/start-suite 2)
(let [data @{:a "pizza" :b "hotdog"}
      new-node (node :a ;(kvs data))]
  (test/assert (= (get-in new-node [2 :data :a]) "pizza")
               "should have spliced data correctly"))

(let [name :pizza
      from :a
      to :b
      new-edge (edge name from to)]
  (test/assert (= new-edge [:edge :a {:name :pizza :to :b :weight 1}])))
(test/end-suite)

# Test dropping an edge by name
(test/start-suite 3)
(let [graph (create (node :a) (node :b) (node :c)

                    (edge :a :b)
                    (edge :b :c)
                    (edge :c :a))]
  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{:b {:to :b :weight 1}}}
                          :b {:data @{} :edges @{:c {:to :c :weight 1}}}
                          :c {:data @{} :edges @{:a {:to :a :weight 1}}}}}))
  (:drop-edge graph :b)
  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{}} # Edge removed <- here 
                          :b {:data @{} :edges @{:c {:to :c :weight 1}}}
                          :c {:data @{} :edges @{:a {:to :a :weight 1}}}}}))
  (:drop-edge graph :c)
  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{}}
                          :b {:data @{} :edges @{}} # Edge removed <- here 
                          :c {:data @{} :edges @{:a {:to :a :weight 1}}}}})))
(test/end-suite)

# Test dropping an edge by name, more complex example
(test/start-suite 4)
(let [graph (create (node :a) (node :b) (node :c) (node :d) (node :e) (node :f)

                    (edge :a-b :a :b)
                    (edge :b-c :b :c)
                    (edge :c-a :c :a)
                    (edge :a-e :a :e)
                    (edge :b-e :b :e)
                    (edge :c-e :c :e)
                    (edge :f-a :f :a)
                    (edge :f-b :f :b)
                    (edge :f-c :f :c))]
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{:b-c {:to :c :weight 1} 
                                                  :b-e {:to :e :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  :c-e {:to :e :weight 1}}}
                           :d {:data @{} :edges @{}}                      
    
                           :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}}))
  (:drop-edge graph :b-c)
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{# :b-c {:to :c :weight 1} 
                                                  :b-e {:to :e :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  :c-e {:to :e :weight 1}}}
                           :d {:data @{} :edges @{}}                      
    
                           :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}}) 
               (string/format "Got: %M" graph))
  (:drop-edge graph :f-b)
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{:b-e {:to :e :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  :c-e {:to :e :weight 1}}}
                           :d {:data @{} :edges @{}}                      
    
                           :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  # :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}})))
(test/end-suite)


# Test dropping a node by name
(test/start-suite 5)
(let [graph (create (node :a) (node :b) (node :c)

                    (edge :a :b)
                    (edge :b :c))]

  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{:b {:to :b :weight 1}}}
                          :b {:data @{} :edges @{:c {:to :c :weight 1}}}
                          :c {:data @{} :edges @{}}}}))
  (:drop-node graph :c)
  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{:b {:to :b :weight 1}}}
                          :b {:data @{} :edges @{}} # Edge removed <- here
                          }}) # Node removed <- here
               (string/format "Got this: %M" graph))
  (:drop-node graph :b)
  (test/assert (deep= graph
                      @{:adjacency-table
                        @{:a {:data @{} :edges @{}} # Edge removed <- here
                          }}))) # Node removed <- here
(test/end-suite)

# Test dropping a node by name, more complex example
(test/start-suite 6)
(let [graph (create (node :a) (node :b) (node :c) (node :d) (node :e) (node :f)

                    (edge :a-b :a :b)
                    (edge :b-c :b :c)
                    (edge :c-a :c :a)
                    (edge :a-e :a :e)
                    (edge :b-e :b :e)
                    (edge :c-e :c :e)
                    (edge :f-a :f :a)
                    (edge :f-b :f :b)
                    (edge :f-c :f :c))]
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{:b-c {:to :c :weight 1} 
                                                  :b-e {:to :e :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  :c-e {:to :e :weight 1}}}
                           :d {:data @{} :edges @{}}                      
    
                           :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}}))
  (:drop-node graph :d)
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{:b-c {:to :c :weight 1} 
                                                  :b-e {:to :e :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  :c-e {:to :e :weight 1}}}
                           # :d {:data @{} :edges @{}}                      
    
                           :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}}))
  (:drop-node graph :e)
  (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{#:a-e {:to :e :weight 1} 
                                                  :a-b {:to :b :weight 1}}}
                           :b {:data @{} :edges @{:b-c {:to :c :weight 1} 
                                                  #:b-e {:to :e :weight 1}
                                                  }}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1} 
                                                  #:c-e {:to :e :weight 1}
                                                  }}
                           # :e {:data @{} :edges @{}}                      
    
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  :f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}}))
   (:drop-node graph :b)
   (test/assert (deep= graph
                       @{:adjacency-table 
                         @{:a {:data @{} :edges @{# :a-b {:to :b :weight 1}
                                                  }}
                           # :b {:data @{} :edges @{:b-c {:to :c :weight 1}}}
                           :c {:data @{} :edges @{:c-a {:to :a :weight 1}}}
                           :f {:data @{} :edges @{:f-a {:to :a :weight 1} 
                                                  #:f-b {:to :b :weight 1} 
                                                  :f-c {:to :c :weight 1}}}}})))
(test/end-suite)
