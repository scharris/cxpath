
(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxpathlib.clj")


;; [Notes from the original Scheme implementation]
;; $Id: sxpath.scm,v 1.5 2005/09/07 09:27:34 lizorkin Exp $
;; Highghest level SXPath 
;; Refactored from sxml-tools.scm and sxpathlib.scm
;==============================================================================
; Abbreviated SXPath

; Evaluate an abbreviated SXPath
;	sxpath:: AbbrPath -> Converter, or
;	sxpath:: AbbrPath -> Node|Nodelist -> Nodelist
; AbbrPath is a list. It is translated to the full SXPath according
; to the following rewriting rules
; (sxpath '()) -> (node-join)
; (sxpath '(path-component ...)) ->
;		(node-join (sxpath1 path-component) (sxpath '(...)))     ; SCH - OK - this impl uses node-reduce directly on the converters produced by sxpath1
; (sxpath1 '//) -> (descendant-or-self xpath-node?)              ; SCH - this impl checks next step and sometimes omits self (optimization?)
; (sxpath1 '(equal? x)) -> (select-kids (node-equal? x))
; (sxpath1 '(eq? x))    -> (select-kids (node-eq? x))
; (sxpath1 '(:or ...))  -> (select-kids (ntype-names??
;                                          (cdr '(:or ...))))
; (sxpath1 '(:not ...)) -> (select-kids (complement (ntype-names?? (cdr '(:not ...)))))
; (sxpath1 '(ns-id|* x)) -> (select-kids 
;                                      (ntype-namespace-id?? x))
; (sxpath1 ?symbol)     -> (select-kids (ntype?? ?symbol))
; (sxpath1 ?string)     -> (txpath ?string)                     ; SCH - omitted
; (sxpath1 procedure)   -> procedure
; (sxpath1 '(?symbol ...)) -> (sxpath1 '((?symbol) ...))        
; (sxpath1 '(path reducer ...)) ->
;		(node-reduce (sxpath path) (sxpathr reducer) ...)
; (sxpathr number)      -> (node-pos number)
; (sxpathr path-filter) -> (filter (sxpath path-filter))


; Differences vs. the original Scheme implementation from sxml-tools:
;   - This version will never return nil, throwing a RuntimeException instead.
;   - Traditional w3c xpath strings are not supported as location steps.
;   - Removed variable bindings everywhere, as they are only needed for tradtional xpath support.
;   - Converters production has been separated into its own function.
;   - Refactored, especially sub-path handling.

; TODO: Implement syntax for parent (^), ancestor(^..), and maybe other axes to make them easier to use (no need to pass root node).

(def converters-for-path)

(defn cxpath
  "cxpath:: [PathComponent] (,NS_Bindings)? -> Node|Nodelist -> Nodelist
ie cxpath:: [PathComponent] (,NS_Bindings)? -> Converter"
  ([path]
     (cxpath path nil))
  ([path ns-bindings]
     (let [ converters (converters-for-path path ns-bindings) ]
	   (fn [n-nl] 
		  ((apply node-reduce converters) n-nl)))))


(defn converters-for-path 
  [path ns-binding]
    (let [symbolic-or-tag? (fn [x] (or (symbolic? x) (compound-tag? x)))]
      
      (loop [converters nil
             path (seq path)]
        (let [loc-step (first path)]
      
          (cond
            ;; parsing finished
            (nil? path) (reverse converters)

		    ;; descendant handler
            (= DESCENDANT-SYM loc-step)
		      (if (or (nil? (rest path))
                      (not (symbolic-or-tag? (frest path)))
                      (= (frest path) NTYPE-ATTRIBUTES-SYM))
                (recur (cons (descendant-or-self xpath-node?) converters)
                       (rest path))
                (recur (cons (descendant (ntype?? (frest path))) converters)
                       (rrest path)))

            ;; handler for element-like nodes (including attributes and special elements), and attribute collections
            (symbolic-or-tag? loc-step)
              (recur (cons (select-kids (ntype?? loc-step)) converters)
                     (rest path))

            ;; function handler
            (instance? clojure.lang.IFn loc-step)
		      (recur (cons loc-step converters)
                     (rest path))
		  
		    ;; sequence handler
            (seq loc-step)
		      (let [ fst-sstep (first loc-step) ]
			    (cond
			       (= :or fst-sstep)
				     (recur (cons (select-kids (ntype-names?? (rest loc-step))) converters)
					   	    (rest path))
                   (= :not fst-sstep)
				     (recur (cons (select-kids (complement (ntype-names?? (rest loc-step)))) converters)
                            (rest path))
				   (or (= 'equal? fst-sstep)
                       (= '=      fst-sstep))
				     (recur (cons (select-kids (apply node-equal? (rest loc-step))) converters)
                            (rest path))
                   (= 'eq? fst-sstep)
				     (recur (cons (select-kids (apply node-eq? (rest loc-step))) converters)
                            (rest path))
				   (= 'ns-id|* fst-sstep)
				     (recur (cons (select-kids (ntype-namespace-id?? (first (rest loc-step)))) converters)
                            (rest path))
                   ;; general sub-path handler
                   :else
				     (let [;; selector for the initial substep
                           selector (if (symbolic-or-tag? fst-sstep)
                                      (select-kids (ntype?? fst-sstep))
                                      (cxpath fst-sstep ns-binding)) ; first substep should be a sequence
						   ;; converters for the remaining substeps, which act only as successive filters on the initial selector output
                           filters (map (fn [sstep] 
                                            (if (integer? sstep) 
                                              (node-pos sstep)
                                              (filter-nodes (cxpath sstep ns-binding)))) ; TODO: sstep should be in a singleton sequence here?
                                        (rest loc-step))
						   ;; composition of initial selector and the converters
                           filtered-selector (apply node-reduce (cons selector filters))]
                       (recur (cons (fn [n-nl] (map-union filtered-selector (as-nodelist n-nl)))
                                    converters)
                              (rest path)))))
            
            ;; invalid
            :else 
		      (throw (new java.lang.RuntimeException (str "Invalid path step: " loc-step))))))))

; (load-file "cxpath.clj")

