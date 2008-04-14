; This code Public Domain.
; It's based on SXPath by Oleg Kiselyov, and multiple improvements 
; implemented by Dmitry Lizorkin.  It has been refactored, modified, 
; and ported to Clojure from the original Scheme sxml-tools distribution
; by Steve Harris, available at gmail.com as user steveOfAR.  Comments
; and bug reports are welcome.
; Links:
;   - Oleg's original SXML/SXPath is available at:
;         http://okmij.org/ftp/Scheme/xml.html
;   - Dmitry Lizorkin's sxml-tools distribution, which includes a modified SXPath distribution, is at:
;         http://ssax.sourceforge.net/

; Differences vs. the original Scheme implementation from sxml-tools:
;   - This version will never return nil, throwing a RuntimeException instead.
;   - Converters production has been separated into its own function.
;   - Refactored, especially sub-path handling.  Node-reduce used where possible.
;   - Traditional w3c xpath strings are not supported as location steps.
;   - Removed variable bindings everywhere, as they are only needed for tradtional xpath support.
;   - New prefix expansion feature allowing prefix/symbol to be expanded to [uri symbol] qualified tags.
;   - (TODO) New parent, ancestor operators.

(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxml.clj")
(load-file "cxpathlib.clj")


; TODO: Implement syntax for parent (^), ancestor(^..), and maybe other axes to make them easier to use (no need to pass root node).

(def converters-for-path)
(def expand-ns-prefixes)


(defn cxpath
  "cxpath:: [PathComponent] (,NS_Bindings)? -> Node|Nodelist -> Nodelist
ie cxpath:: [PathComponent] (,NS_Bindings)? -> Converter"
  ([path]
     (cxpath path nil))
  ([path ns-bindings]
	 (fn [n-nl] 
		 (let [converters (converters-for-path (expand-ns-prefixes path ns-bindings) n-nl)]
		   ((apply node-reduce converters) n-nl)))))


(defn converters-for-path 
  [path root-node]
    (let [symbolic-or-tag? (fn [x] (or (symbolic? x) (compound-tag? x)))]
      
      (loop [converters nil
             path (seq path)]
        (let [loc-step (first path)]
      
          (cond
            ;; parsing finished
            (nil? path) (reverse converters)

		    ;; descendant handler
            (= DESCENDANT-SYM loc-step)
		      (if (or (nil? (rest path))                     ;| Don't try to optimize based on following step if any of these three are true
                      (not (symbolic-or-tag? (frest path)))  ;|
                      (= (frest path) NTYPE-ATTRIBUTES-SYM)) ;|
                (recur (cons (descendant-or-self xpath-node?) converters)    ; general case
                       (rest path))
                (recur (cons (descendant (ntype?? (frest path))) converters) ; optimized case
                       (rrest path)))

			;; parent handler TODO: test
            (= PARENT-SYM loc-step)
			  (if (or (nil? (rest path))                     ;| Don't try to optimize based on following step if any of these three are true
					  (not (symbolic-or-tag? (frest path)))  ;|
					  (= (frest path) NTYPE-ATTRIBUTES-SYM)) ;|
				(recur (cons (parent (ntype?? NTYPE-ANY-SYM) root-node) converters) ; general case
					   (rest path))
			    (recur (cons (parent (ntype?? (frest path)) root-node) converters)  ; optimized case
					   (rrest path)))
			
            ;; ancestor handler TODO: test
            (= ANCESTOR-SYM loc-step)
			  (if (or (nil? (rest path))                     ;| Don't try to optimize based on following step if any of these three are true
					  (not (symbolic-or-tag? (frest path)))  ;|
					  (= (frest path) NTYPE-ATTRIBUTES-SYM)) ;|
				(recur (cons (ancestor-or-self (ntype?? NTYPE-ANY-SYM) root-node) converters) ; general case
					   (rest path))
			    (recur (cons (ancestor (ntype?? (frest path)) root-node) converters)          ; optimized case
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
			       (= OR-SYM fst-sstep)
				     (recur (cons (select-kids (ntype-names?? (rest loc-step))) converters)
					   	    (rest path))
                   (= NOT-SYM fst-sstep)
				     (recur (cons (select-kids (complement (ntype-names?? (rest loc-step)))) converters)
                            (rest path))
				   (or (= EQUAL-LONG-SYM  fst-sstep)
                       (= EQUAL-SHORT-SYM fst-sstep))
				     (recur (cons (select-kids (apply node-equal? (rest loc-step))) converters)
                            (rest path))
                   (or (= IDENTICAL-LONG-SYM fst-sstep)
                       (= IDENTICAL-SHORT-SYM fst-sstep))
                     (recur (cons (select-kids (apply node-eq? (rest loc-step))) converters)
                            (rest path))
				   (= NAMESPACE-ID-SYM fst-sstep)
				     (recur (cons (select-kids (ntype-namespace-id?? (first (rest loc-step)))) converters)
                            (rest path))
                   ;; general sub-path handler
                   :else
				     (let [;; selector for the initial substep
                           selector (if (symbolic-or-tag? fst-sstep)
                                      (select-kids (ntype?? fst-sstep))
                                      (cxpath fst-sstep)) ; first substep should be a sequence
						   ;; converters for the remaining substeps, which act only as successive filters on the initial selector output
                           filters (map (fn [sstep] 
                                            (if (integer? sstep) 
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
		      (throw (new java.lang.RuntimeException (str "Invalid path step: " loc-step))))))))


(defn expand-ns-prefixes
  [path prefix->uri]
    (if prefix->uri
      (let [is-namespaceable-symbolic-tag? #(and (symbolic? %)
                                                 (not (is-syntax? %))
                                                 (not (special-nonelement-tag? %))
                                                 (not= cxml/DOCROOT-TAG %))
            maybe-expand-tag (fn [tag]
                                 (let [clj-ns (namespace tag)
                                       uri (prefix->uri clj-ns)]
                                   (if uri
                                     (with-meta [uri (if clj-ns (without-clj-ns tag) tag)] {:xmlns-prefix clj-ns})
                                     tag)))] ; found no uri for prefix, leave it alone
        (map (fn [step]
                 (cond
                    (seq? step)
                      (expand-ns-prefixes step prefix->uri)
                    (is-namespaceable-symbolic-tag? step)
                      (maybe-expand-tag step)
                    :else step))
             path))
    path))

; (load-file "cxpath.clj")

