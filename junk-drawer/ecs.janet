(use spork/schema)

(import /junk-drawer/sparse-set)
(import /junk-drawer/cache)

(defmacro def-component [name & fields]
  "Define a new component with the specified fields."
  ~(defn ,name [&named ,;(map symbol (keys (table ;fields)))]
     (-> (table/setproto ,(table ;(mapcat |[$ (symbol $)] (keys (table ;fields))))
                          @{:__id__ ,(keyword name)
                            :__validate__ ,((make-validator ~(props ,;fields)))})
         (:__validate__))))

(defmacro def-tag [name]
  "Define a new tag (component with no data)."
  ~(defn ,name []
     (table/setproto @{}
                     @{:__id__ ,(keyword name)
                       :__validate__ (fn [& args] false)})))

(defmacro def-system [name queries & body]
  "Define a system to do work on a list of queries."
  ~(def ,name
     (tuple
       ,(values queries)
       (fn [,;(keys queries) dt] ,;body))))

(defn- get-or-create-component-set [{:database db :capacity cap} cmp-name]
  "return the sparse set for the component, creating if it it does not already exist."
  (match (get db cmp-name)
    nil (let [new-set (sparse-set/init cap)]
          (put db cmp-name new-set)
          new-set)
    cmp-set cmp-set))

(defn add-component [world eid component]
  "Add a new component to an existing entity."
  (let [cmp-name (component :__id__)
        cmp-set (get-or-create-component-set world cmp-name)]
    (:insert cmp-set eid component)
    (:clear (world :view-cache) cmp-name)))

(defn remove-component [world ent component-name]
  "Remove a component by its name from an entity."
  (let [pool (get-in world [:database component-name])]
    (assert (not (nil? pool)) "component does not exist in world")
    (assert (not= -1 (:search pool ent)) "entity with component does not exist in world")
    (:delete pool ent)
    (:clear (get world :view-cache) component-name)))

(defn add-entity [world & components]
  "Add a new entity with the given components to the world."

  # Use a free ID (from deleted entity) if available
  (let [eid (or (array/pop (world :reusable-ids))
                (get world :id-counter))]

    # Add individual component data to database
    (each component components
      (add-component world eid component))

    # increment the id counter when there are no more
    # free id's to use
    (when (empty? (world :reusable-ids))
      (put world :id-counter (inc (world :id-counter))))))

(defn remove-entity [world ent]
  "Remove an entity from the world by its ID."
  (eachp [name pool] (world :database)
         (:delete pool ent)
         (:clear (get world :view-cache) name))
  (array/push (world :reusable-ids) ent))

(defn register-system [world sys]
  "Register a system to be run on world update."
  (array/push (get world :systems) sys))

(defn- smallest-pool [pools]
  "Length (n) of smallest pool."
  (reduce2 |(if (< (get-in $0 [1 :n])
                   (get-in $1 [1 :n]))
              $0 $1)
           pools))

(defn- every-has? [pools eid]
  "True if every pool has the entity id, false otherwise."
  (every? (map |(not= -1 (:search $ eid)) pools)))

(defn- intersection-entities [pools]
  "List of entities which all pools contain."
  (let [small-pool (smallest-pool pools)]
    (mapcat
     |(let [eid (get-in small-pool [:entities $])]
        (if (every-has? pools eid) [eid] []))
     (range 0 (small-pool :n)))))

(defn- view-entry [pools eid]
  "Tuple of all component data for eid from pools (eid cmp-data cmp-data-2 ...)."
  (tuple ;(map |(:get-component $ eid) pools)))

(defn- view [{:database database :view-cache view-cache :capacity capacity} query]
  "Result of query as list of tuples [(eid cmp-data cmp-data-2 ...)]."
  (if-let [cached-view (:get view-cache query)]
    cached-view
    (if-let [pools (map |(match $
                           :entity {:get-component (fn [self eid] eid)
                                    :search (fn [self eid] 0)
                                    :n (+ 1 capacity)
                                    :debug-print (fn [self] (print "entity patch"))}
                           (database $)) query)
             all-empty? (empty? (filter nil? pools))
             view-result (map |(view-entry pools $) (intersection-entities pools))]
      (:insert view-cache query view-result)
      (:insert view-cache query []))))

(defn- query-result [world query]
  "Either return a special query, or the results of ECS query."
  (match query
    :world world
    [_] (view world query)))

(defn- update [self dt]
  "Call all registers systems for entities matching thier queries."
  (loop [(queries func)
         :in (self :systems)
         :let [queries-results (map |(query-result self $) queries)]]

    (when (some |(not (empty? $)) queries-results)
      (func ;queries-results dt))))

(defn create-world [&named capacity]
  "Instantiate new world."
  (default capacity 1000)
  @{:capacity capacity
    :id-counter 0
    :reusable-ids @[]
    :database @{}
    :view-cache (cache/init)
    :systems @[]
    :update update
    :view view})
