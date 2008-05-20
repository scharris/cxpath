; This code is Public Domain.
; It's based on SXPath by Oleg Kiselyov, and multiple improvements 
; implemented by Dmitry Lizorkin.  It has been ported to Clojure,
; refactored  and extended by Steve Harris.  Comments, questions and
; bug reports are welcome, please send to steveOfAR at gmail.com.
; Links:
;   - Oleg's original SXML/SXPath is available at:
;         http://okmij.org/ftp/Scheme/xml.html
;   - Dmitry Lizorkin's sxml-tools distribution, which includes a modified SXPath distribution, is at:
;         http://ssax.sourceforge.net/
;     This distribution was the starting point for the port to Clojure.

; Differences in this Clojure version vs. the original Scheme implementation from sxml-tools:
;   - Refactored
;          o Production of converters from brief syntax has been separated into its own function.
;          o Main cxpath function is a simple node-reduce.
;          o Subpath handling simplified and also relies on node-reduce now.
;   - Added support in brief syntax for parent (..) and ancestor (..*) axes.
;   - A pair of more powerful replacements for the original *or* operator
;       i)  |or| - allows node types and full subpath expressions as existence test filters, as well tags.
;       ii) |alt| - like the new |or| operator but subpaths project results instead of just providing existence tests.
;   - |not| operator extended to be complement of the new |or| operator, allowing subpaths and node types as well as tags.
;   - New prefix expansion feature allowing clojure namespaced symbols or keywords to be expanded to [uri symbol] qualified
;      tags prior to processing.
;   - This version will never return nil, throwing a RuntimeException instead.
;   - Traditional w3c xpath strings are not supported as location steps.
;   - Removed variable bindings everywhere, which were only needed for tradtional xpath support.


(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxml.clj")
(load-file "cxpathlib.clj")
(load-file "utils.clj")

(def converters-for-path)
(def expand-ns-prefixes)
(def any)

(defn cxpath
  "cxpath:: [PathComponent] (,NS_Bindings)? -> Node|Nodelist -> Nodelist
ie cxpath:: [PathComponent] (,NS_Bindings)? -> Converter"
  ([path]
     (cxpath path nil))
  ([path ns-bindings]
     (fn [n-nl] 
         (let [converters (converters-for-path (expand-ns-prefixes path ns-bindings)
                                               n-nl)]
           ((apply node-reduce converters) n-nl)))))


(defn converters-for-path 
  [path root-nodes]
      
      (loop [converters nil
             path (seq path)]
        (let [loc-step (first path)]
      
          (cond
            ;; parsing finished
            (nil? path) (reverse converters)

            ;; descendant-or-self
            (= DESCENDANT-OR-SELF-SYM loc-step)
              (if (or (not (tag-or-ntype? (frest path)))
                      (= (frest path) NTYPE-ATTRIBUTES-SYM)) ; attrs nodetype symbol doubles as indicator to switch to attr axis
                (recur (cons (descendant-or-self any) converters)            ; general case
                       (rest path))
                (recur (cons (descendant (ntype?? (frest path))) converters) ; optimized case
                       (rrest path)))
            
            ;; ancestor-or-self
            (= ANCESTOR-OR-SELF-SYM loc-step)
                (recur (cons (ancestor-or-self any root-nodes) converters)
                       (rest path))
            
            ;; parent
            (= PARENT-SYM loc-step)
              (recur (cons (parent any root-nodes) converters)
                     (rest path))

            ;; tags and node types
            (tag-or-ntype? loc-step)
              (recur (cons (select-kids (ntype?? loc-step)) converters)
                     (rest path))

            ;; custom converter functions
            (instance? clojure.lang.IFn loc-step)
              (recur (cons loc-step converters)
                     (rest path))
          
            ;; sequences
            (seq loc-step)
              (let [fst-sstep (first loc-step)]
                (cond
                   ;; alternatives, subpath projecting 
                   (= OR-SUBPATH-PROJECTING-SYM fst-sstep)
                     (let [[paths ntypes] (pass-fail-lists seq? (rest loc-step))
                           ntypes-conv (if ntypes (select-kids (ntype-in?? ntypes))) ; converter filtering by node type alternatives
                           paths-conv (if paths (apply node-or (map cxpath paths)))  ; converter unioning results of cxpath path expressions
                           conv (apply node-or (cons-if ntypes-conv (cons-if paths-conv nil)))] ; union results of the converters
                       (recur (cons conv converters)
                              (rest path)))
                   ;; alternatives and negation of alternatives
                   (or (= OR-SYM  fst-sstep)
                       (= NOT-SYM fst-sstep))
                     (let [[paths ntypes] (pass-fail-lists seq? (rest loc-step))
                           preds (cons-if* ntypes (ntype-in?? ntypes)          ; node type alternatives predicate
                                   (map #(as-predicate (cxpath %)) paths))     ; cxpath expressions as predicates
                           combined-pred (if (= OR-SYM fst-sstep)
                                           (or-predicates preds)
                                           (complement (or-predicates preds)))]
                       (recur (cons (select-kids combined-pred) converters)
                              (rest path)))
                   ;; node equal
                   (= EQUAL-SYM  fst-sstep)
                     (recur (cons (select-kids (apply node-equal? (rest loc-step))) converters)
                            (rest path))
                   ;; node identical
                   (= IDENTICAL-SYM fst-sstep)
                     (recur (cons (select-kids (apply node-eq? (rest loc-step))) converters)
                            (rest path))
                   ;; namespace
                   (= NAMESPACE-ID-SYM fst-sstep)
                     (recur (cons (select-kids (ntype-namespace-id?? (first (rest loc-step)))) converters)
                            (rest path))
                   ;; general front-projecting sub-paths
                   :else
                     (let [as-path (fn [x] (if (seq? x) x (list x)))
                           ;; selector for the initial substep
                           selector (if (tag-or-ntype? fst-sstep)
                                      (select-kids (ntype?? fst-sstep))
                                      (cxpath (as-path fst-sstep)))
                           ;; converters for the remaining substeps, which act only as successive filters on the initial selector output
                           filters (map (fn [sstep]
                                            (if (number? sstep) 
                                              (node-pos sstep)
                                              (filter-nodes (cxpath sstep))))
                                        (rest loc-step))
                           ;; composition of initial selector and the converters
                           filtered-selector (apply node-reduce (cons selector filters))]
                       (recur (cons (fn [n-nl] (map-union filtered-selector (as-nodelist n-nl)))
                                    converters)
                              (rest path)))))
            
            ;; invalid
            :else 
              (throw (new java.lang.RuntimeException (str "Invalid path step: " loc-step)))))))


(defn expand-ns-prefixes
  [path prefix->uri]
    (if prefix->uri
      (let [is-namespaceable-symbolic-tag? #(and (simple-tag? %)
                                                 (not (special-nonelement-tag? %))
                                                 (not= cxml/DOCROOT-TAG %))
            maybe-expand-tag (fn [tag]
                                 (let [clj-ns (namespace tag)
                                       prefix-mapped? (contains? prefix->uri clj-ns)
                                       uri (if prefix-mapped? (prefix->uri clj-ns))]
                                   (if prefix-mapped?
                                     (if uri
                                       (with-meta [uri (if clj-ns (without-clj-ns tag) tag)] {:xmlns-prefix clj-ns})
                                       (without-clj-ns tag))  ; prefix mapped to nil, just remove the prefix
                                     tag)))] ; prefix not found in map, leave the tag alone
        (map (fn [step]
                 (cond
                    (seq? step)
                      (expand-ns-prefixes step prefix->uri)
                    (is-namespaceable-symbolic-tag? step)
                      (maybe-expand-tag step)
                    :else step))
             path))
    path))

(def any (ntype?? NTYPE-ANY-SYM))

; (load-file "cxpath.clj") (in-ns 'cxpath)
