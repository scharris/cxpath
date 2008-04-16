; This code is Public Domain.
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

; Terminology for this port:
;   Converter :: Node|Nodelist -> Nodelist
;   Selector :: Node -> Nodelist (note: the original Scheme impl. allowed Node output as well)
;   Predicate :: Node -> Boolean
;   element: A traditional xml element, the document root, or attribute/value map entry (vector).  
;            Does not include *PI*, *COMMENT*, or *ENTITY* nodes.
;   tag: The symbol at head position in an element as defined above, thus including attribute name symbols.
;   node: Includes any sexp in the cxml other than tags.  If prefixed with "xpath-" then the narrower
;         w3c xpath meaning is meant in which attribute collections are not included.
;   kid: A node directly contained in an element (as defined above)
;   child: An element or data kid.


(in-ns 'cxpath)
(clojure/refer 'clojure)
(load-file "cxml.clj")

; Syntax symbols
(def DESCENDANT-SYM '**)
(def PARENT-SYM '..)
(def ANCESTOR-SYM '..*)
(def OR-SYM '*or*)
(def NOT-SYM '*not*)
(def EQUAL-LONG-SYM 'equal?)
(def EQUAL-SHORT-SYM '=)
(def IDENTICAL-LONG-SYM 'identical?)
(def IDENTICAL-SHORT-SYM '==)
(def NAMESPACE-ID-SYM 'ns-id|*)
(def NTYPE-ELEMENT-SYM '*)
(def NTYPE-ATTRIBUTES-SYM '*at*)
(def NTYPE-TEXT-SYM '*text*)
(def NTYPE-ANY-SYM  '*any*)
(def NTYPE-DATA-SYM  '*data*)

(defn ntype-symbol? [x]
  (or (= x NTYPE-ELEMENT-SYM)
      (= x NTYPE-ATTRIBUTES-SYM)
      (= x NTYPE-TEXT-SYM)
      (= x NTYPE-ANY-SYM)
      (= x NTYPE-DATA-SYM)))

(defn syntax-symbol?
  "Tests whether the passed item is one of cxpath's syntax symbols."
  [x]
    (or (= x DESCENDANT-SYM)
        (= x PARENT-SYM)
        (= x ANCESTOR-SYM)
        (= x OR-SYM)
        (= x NOT-SYM)
        (= x EQUAL-LONG-SYM)
        (= x EQUAL-SHORT-SYM)
        (= x IDENTICAL-LONG-SYM)
        (= x IDENTICAL-SHORT-SYM)
        (= x NAMESPACE-ID-SYM)
        (ntype-symbol? x)))


; ------------------------------------------------
; tag related functions for element like nodes

(defn symbolic? 
  "Tests whether the passed item is either a symbol or keyword."
  [x]
    (or (symbol? x)
        (keyword? x)))

(defn simple-tag?
  "simple-tag?:: Anything -> Boolean
A predicate testing whether the passed object is a simple (unqualified) tag.
A symbol or keyword is considered a tag if it is not a syntax symbol."
  [x]
    (and (symbolic? x)
         (not (syntax-symbol? x))))

(defn compound-tag? 
  "compound-tag?:: Anything -> Boolean
A predicate testing whether the passed object is a namespace qualified tag,
of the form [uri symbol-or-keyword]."
  [x]
    (and (vector? x)
         (= 2 (count x))
         (string? (nth x 0))
         (symbolic? (nth x 1))))

(defn tag?
  "tag?:: Anything -> Boolean
A predicate testing whether the passed object is a simple or compound tag.
A tag is either a symbol, keyword, or [uri-string symbol/keyword] pair vector.
Syntax symbols are not considered tags by this predicate."
  [x]
    (or (simple-tag? x)
        (compound-tag? x)))

(defn tag
  "tag :: Node -> Symbol|Nil
Returns the leading symbol/keyword or [namespace symbol/keyword] pair of a traditional element,
special-element, or attribute/value pair vector, or nil for any other type of node.
This function can also be used as a predicate to test for an element-like node."
   [node] 
     (if (or (seq? node) 
             (vector? node))
       (when-first hd node
         (if (or (symbolic? hd) ; no need to check for syntax symbols here since this is cxml content
                 (compound-tag? hd)) hd))))

(defn special-nonelement-tag?
  "Determines whether the passed object is the special tag for processing instructions, comments or entities."
  [t]
    (or (= t cxml/PI-TAG)
        (= t cxml/COMMENT-TAG)
        (= t cxml/ENTITY-TAG)))

(defn element?-tag
  "Returns the tag for the node if the node is an element node, including the document root
and attribute/value pairs, or nil for any other node.  As a predicate this function is 
identical to the element? function, but more usefully returns the tag instead of true and
nil instead of false."
  [node]
    (let [ tag (tag node) ]
      (if (and tag
               (not (special-nonelement-tag? tag)))
		tag)))

;; The following is an important function in cxpath.clj for testing whether we can combine two location
;; steps into one for efficiency.
(defn tag-or-ntype?
  "Tests whether its argument is either a tag or a nodetype, and thus suitable for passing to
the ntype?? function."
  [x]
    (or (ntype-symbol? x)
        (tag? x)))


(defn xmlns 
  "Returns the xml namespace uri of an element node if any, or nil for any other type of node."
  [node]
    (let [ el-tag (element?-tag node) ]
      (if (vector? el-tag)
        (el-tag 0)
        nil)))

(def tags-equal? =)

(defn without-clj-ns 
  "Returns the argument symbol or keyword without its clojure namespace if any."
  [sym-or-kwd]
    (let [nm (name sym-or-kwd)]
      (if (symbol? sym-or-kwd)
        (symbol nm)
        (keyword nm))))

(def nodelist?)

(defn with-xmlns
  "Applies namespace uri's to element and attribute tags within the passed node or nodelist which have no xml
namespace metadata already, and for which a mapping for their clojure namespace prefix is present in the
passed prefix->uri mapping.  For tags whose xml namespace is set in this way, the prefix is removed from
the symbol or keyword in the resulting node. All other nodes are left alone."
  [prefix->uri n-nl]
    (if (nodelist? n-nl)
      (map #(with-xmlns prefix->uri %) n-nl)
      ; single node
      (let [node n-nl
            map-mapentries (fn [f m]
                               (reduce (fn [res [key val]] (let [[new-key new-val] (f key val)] (assoc res new-key new-val)))
                                       nil
                                       m))

            maybe-expand-tag (fn [tag allow-default-ns]
                                 (if (or (vector? tag)             ; already has a namespace
                                         (= cxml/DOCROOT-TAG tag)) ; document root
                                   tag
                                   (let [clj-ns (namespace tag)
                                         uri (if allow-default-ns
                                               (prefix->uri clj-ns)
                                               (if clj-ns (prefix->uri clj-ns)))]
                                     (if uri
                                       (with-meta [uri (if clj-ns (without-clj-ns tag) tag)] {:xmlns-prefix clj-ns})
                                       tag))))] ; found no uri for prefix, leave it alone
        (if (map? node) ; attributes collection?
          (map-mapentries (fn [attr-tag val] [(maybe-expand-tag attr-tag false) val])
                          node)
          (let [ el-tag (element?-tag node) ] ; element?
            (if (not el-tag)
              node
              (let [new-tag (maybe-expand-tag el-tag true)]
                (if (seq? node)
                  (lazy-cons new-tag (map #(with-xmlns prefix->uri %) (rest node)))
                  (apply vector new-tag (map #(with-xmlns prefix->uri %) (rest node)))))))))))

; ------------------------------------------------


; ----------------------------------
; Nodelist functions

; nodelist helper functions
(defn make-nodelist
  "make-nodelist:: Node* -> Nodelist
Make a nodlist of the zero or more node arguments"
     [& nodes]
       (when (seq nodes) (apply list nodes)))

(defn empty-nodelist?
  "empty-nodelist?:: Node|Nodelist -> Boolean
Tests whether a nodelist is empty."
  [x]
    (or (nil? x)
        (= () x))) ; nil is the canonical empty nodelist; () is also recognized though not produced here.


(defn nodelist?
  "nodelist?:: Node|Nodelist -> Boolean
Returns whether given object is a nodelist.  A nodelist is identified as either
nil, (), or a sequence without a tag."
  [x]
    (or (empty-nodelist? x)
        (and (seq? x)
             (nil? (tag x)))))

(defn as-nodelist
  "as-nodelist:: Node|Nodelist -> Nodelist
If the argument is a nodelist then it is returned as is, otherwise a nodelist is made of it."
  [n-nl]
    (if (nodelist? n-nl) n-nl (make-nodelist n-nl)))

; ----------------------------------



(defn integer? [x] (instance? clojure.lang.IntegerNum x))


;=============================================================================
; A converter is a function
;   type Converter = Node|Nodelist -> Nodelist
; A converter can also play a role of a predicate: in that case, if a
; converter, applied to a node or a nodelist, yields a non-empty
; nodelist, the converter-predicate is deemed satisfied. Throughout
; this file an empty nodelist is equivalent to false in denoting a failure.


(defn as-predicate
  "as-predicate:: Converter|Predicate -> Predicate
Returns a predicate based on the passed converter or predicate, with logically
true and false values mapped to true and false respectively, except with empty
nodelist results (from a converter) translated to false.
as-predicate:: Predicate|Converter -> Predicate"
  [conv-pred]
    (fn [x]
        (let [ res (conv-pred x) ]
          (boolean (and res (not (empty-nodelist? res)))))))



;-----------------------------------------------------------------------------
; Node tests (Predicates)
; The following functions implement 'Node test's as defined in
; Sec. 2.3 of XPath document. A node test is one of the components of a
; location step. It is also a converter-predicate in SXPath.

; Was sxml:node?
; The name was qualified with "xpath-" because in sxml/cxml the attribute collections
; (maps) are considered to be nodes, as opposed to XPath.  E.g., they appear in 
; nodelists (see e.g. calls to cxml-filter, which takes a node or nodelist, applied to
; the "rest" of an element node which would include the attributes collection as a node).
(defn xpath-node?
  "xpath-node?:: Node -> Boolean
According to XPath specification 2.3, this test is true for any XPath node.
For cxml, attribute collections have to be excluded since there is no
such entity in XPath, only individual attributes."
  [node]
    (not (map? node)))


; Was sxml:element?
(defn element?
  "element?:: Node -> Boolean,
ie element?:: Predicate
Predicate which returns true for traditional elements, the document root,
and attribute/value pairs (not attribute maps), and false for *PI*, *COMMENT*,
*ENTITY* and other nodes."
  [node]
    (boolean (element?-tag node)))


(defn ntype-names??
  "ntype-names??:: [Symbol] -> Node -> Boolean,
ie ntype-names??:: [Symbol] -> Predicate
Takes a list of acceptable node tag symbols as a criterion and returns a function
which tests nodes for having a tag (beginning symbol) and which is in the list."
  [test-tags]
    (fn [node]
        (let [ tag (tag node) ]
		  (boolean (and tag
						(some #(tags-equal? tag %) test-tags))))))


(defn ntype??
  "ntype??:: Symbol -> Node -> Boolean,
ie ntype??:: Symbol -> Predicate
Takes a type criterion symbol and returns a function which tests nodes for being
of the required type.
The criterion 'crit' is one of the following symbols:
 id     - tests if the Node has the right name (id)
 *a*    - tests if the Node is an attributes map
 *      - tests if the Node is an element or attribute/value pair
 *text* - tests if the Node is a text node
 *data* - tests if the Node is a data node 
           (text, number, boolean, etc., but not pair)
 *PI*   - tests if the Node is a PI node
 *COMMENT*  - tests if the Node is a COMMENT node
 *ENTITY*   - tests if the Node is a ENTITY node
 *any*      - true for any type of Node"
  [t]
    (cond 
     (= t NTYPE-ELEMENT-SYM) element?
     (= t NTYPE-TEXT-SYM) string?
     (= t NTYPE-ANY-SYM) (constantly true)
     (= t NTYPE-DATA-SYM) (fn [node] (not (or (tag node)
                                              (map? node))))
     (= t NTYPE-ATTRIBUTES-SYM) map?
     :else (fn [node]
               (let [ tag (tag node) ]
                 (boolean (and tag (tags-equal? tag t)))))))



(defn ntype-namespace-id??
  "ntype-namespace-id??:: String -> Node -> Boolean,
ie ntype-namespace-id??:: String -> Predicate
Takes a namespace uri string, and returns a predicate testing for element nodes
(according to element?)  with the given namespace uri.  Always false for non-element
nodes. (ntype-namespace-id?? nil) will be true for element nodes with no xml namespace."
  [#^String uri]
    (fn [node]
        (= (xmlns node) uri)))

(defn node-eq?
  "node-eq?:: Node -> Node -> Boolean
ie node-eq?:: Node -> Predicate
Takes a node and returns a predicate testing nodes for being identical to the original node."
  [other]
    (fn [node]
        (identical? other node)))


; Structural comparison of nodes 
(defn node-equal?
  "node-equal?:: Node -> Node -> Boolean
ie node-equal?:: Node -> Predicate
Takes a node and returns a predicate testing nodes for being structurally equal to the original node."
  [test-node]
    (fn [node] (= test-node node)))


; Node tests
;-----------------------------------------------------------------------------


(defn node-pos
  "node-pos:: Integer -> Node|Nodelist -> Nodelist,
ie node-pos:: Integer -> Converter
Selects an element of a nodelist by numerical position (starting with 1)
if the argument is a nodelist and an element exists at the position, returned
as a singleton nodelist, or else the empty nodelist is returned. If the argument
is negative then the counting is done from the back of the list."
  [p]
    (fn [nl]
	   (cond
		  (or (not (nodelist? nl)) 
			  (empty-nodelist? nl)) nil
			  
			  (neg? p) (let [ix (+ p (count nl))]
						 (if (>= ix 0)
						   (make-nodelist (nth nl ix))
						   nil))

			  :else (let [ix (dec p)]
					  (if (< ix (count nl))
						(make-nodelist (nth nl ix))
						nil)))))

; Was: sxml:filter
(defn filter-nodes
  "filter-nodes:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie filter-nodes:: Converter|Predicate -> Converter
A filter applicator, which introduces a filtering context. The argument
converter is considered a predicate, with either false or empty nodelist
result meaning failure."
  [conv-pred]
    (fn [n-nl] ; node or nodelist
	   (filter (as-predicate conv-pred)
			   (as-nodelist n-nl))))

; Was: take-until
(defn take-nodes-until
  "take-nodes-until:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie take-nodes-until:: Converter|Predicate -> Converter
Returns a function which when applied to a nodelist (or node considered as such),
returns the largest prefix of its nodelist argument for which each node satisfies
the converter/predicate."
  [conv-pred]
    (fn [n-nl] ; node or nodelist
	   (take-while (complement (as-predicate conv-pred))
				   (as-nodelist n-nl))))


; Was: take-after
(defn take-nodes-after
  "take-nodes-after:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie take-nodes-after:: Converter|Predicate -> Converter
Returns a function which when applied to a nodelist (or node considered as such),
returns the elements of the input nodelist that follow the first node that satisfies
the predicate."
  [conv-pred]
    (fn [n-nl] ; node or nodelist
	   (rest (drop-while (complement (as-predicate conv-pred))
						 (as-nodelist n-nl)))))


(defn map-union
  "map-union:: Selector|Converter, Nodelist  -> Nodelist
Synonym for mapcat.  In this context, it applies a selector or converter to each
element of a nodelist and returns the nodelist of concatenated results."
  [sel-conv nl]
    (mapcat sel-conv nl))


(defn node-reverse 
  "node-reverse:: Node|Nodelist -> Nodelist,
ie node-reverse:: Converter
Reverses the order of nodes in the nodelist.  This basic converter is needed to
implement a reverse document order (see the XPath Recommendation)."
  [n-nl] ; node or nodelist
    (reverse (as-nodelist n-nl)))

; TODO: need a real pretty printer
(def pp print)


(defn node-trace
  "node-trace:: String -> Converter
Returns an identity Converter which when called prints the passed text and
the pretty-printed argument node, for debugging."
  [title]
    (fn [n-nl] ; node or nodelist
		(print "\n-->" title " :")
		(pp n-nl)
		n-nl))


;------------------------------------------------------------------------------
; Converter combinators
;
; Combinators are higher-order functions that transmogrify a converter
; or glue a sequence of converters into a single, non-trivial
; converter. The goal is to arrive at converters that correspond to
; XPath location paths.
;
; From a different point of view, a combinator is a fixed, named
; _pattern_ of applying converters. Given below is a complete set of
; such patterns that together implement XPath location path
; specification. As it turns out, all these combinators can be built
; from a small number of basic blocks: regular functional composition,
; mapcat and filter applicators, and the nodelist union.

(defn select-kids 
  "select-kids:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie select-kids:: Converter|Predicate -> Converter
Given a node or nodes, return the nodelist of their immedediately contained nodes
of any type which satisfy the predicate, including possibly attributes collections
for element nodes."
  [conv-pred]
    (fn [n-nl] ; node or nodelist
		  (cond 
			(tag n-nl) ((filter-nodes conv-pred) (rest n-nl)) ; it's a single element-like node, apply test to all contents
			(map? n-nl) ((filter-nodes conv-pred) (seq n-nl)) ; apply test to attribute key/value elements
			(not (seq? n-nl)) nil ; atomic data or nil (empty nodelist), no kids
			:else (mapcat (select-kids conv-pred) n-nl)))) ; nodelist


(defn node-self ; synonym for filter-nodes
  "node-self:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie node-self:: Converter|Predicate -> Converter
Similar to select-kids but applies the predicate to the node or nodes themselves
rather than to the kids."
  [conv-pred]
    (filter-nodes conv-pred))


(defn node-join
  "node-join:: Selector* -> Node|Nodelist -> Nodelist,
ie node-join:: Selector* -> Converter
Returns a converter which applies the sequence of selectors in order to its argument
considered as a nodelist, using mapcat to apply each selector to its input nodelist."
  [& selectors]
    (fn [n-nl] ; node or nodelist
		(reduce (fn [nl sel] (mapcat sel nl))
				(as-nodelist n-nl)
				selectors)))

(defn node-reduce
  "node-reduce:: Converter* -> Node|Nodelist -> Nodelist,
ie node-reduce:: Converter* -> Converter,
or node-reduce:: Selector,Converter* -> Node -> Nodelist
ie node-reduce:: Selector,Converter* -> Selector
A regular functional composition of converters, or a selector and converters.
This function is like node-join but in this case the functions are composed
directly, without the mapcat intermediary for handling nodelist arguments.
The first argument may be a selector (accepting only single node arguments)
instead of a converter, in which case a selector is produced instead of
a converter."
  [& converters]
      (fn [n-nl]
	      (reduce (fn [n-nl conv] (conv n-nl))
		    	   n-nl
				   converters)))


(defn node-or
  "node-or:: Converter* -> Converter
This combinator applies all converters to a given node and
produces the union of their results.
This combinator corresponds to a union, '|' operation for XPath
location paths."
  [& converters]
    (fn [n-nl] ; node or nodelist
		(mapcat #(% n-nl) converters)))

; Was: sxml:child-elements
(defn child-elements
  "child-elements:: Node|Nodelist -> Nodelist
ie child-elements:: Converter
Returns a nodelist of child elements of the passed node or nodes,
not including attribute elements except when the passed node is
itself an attributes map.  Equivalent to (select-kids element?)."
  [n-nl] ; node or nodelist
    ((select-kids element?) n-nl))


(defn node-closure
  "node-closure:: Converter|Predicate -> Node|Nodelist -> Nodelist,
ie node-closure:: Converter|Predicate -> Converter
Select child nodes of any type that satisfy a converter-predicate, and
recursively any child nodes reachable by searching child elements themselves
in the same way.  This combinator is similar to the 'descendant::' XPath axis,
though it permits more selections (within non-elements at the top level)
and returns results in breadth-first order instead of document order."
; [Notes from the original]
; Conceptually, this combinator can be expressed as
; (define (node-closure f)
;      (node-or
;        (select-kids f)
;  (node-reduce (select-kids (ntype?? '*)) (node-closure f))))
; This definition, as written, looks somewhat like a fixpoint, and it
; will run forever. It is obvious however that sooner or later
; (select-kids (ntype?? '*)) will return an empty nodelist. At
; this point further iterations will no longer affect the result and
; can be stopped.
  [conv-pred]
    (let [ kid-selector (select-kids conv-pred) ]
      (fn [n-nl] ; node or nodelist
		  (loop [ sels nil 
				  parents (as-nodelist n-nl) ]
			(if (empty-nodelist? parents)
			  sels
			  (recur (concat sels (kid-selector parents))
					 (child-elements parents)))))))

; Was: sxml:attr-list
(defn attr-list
  "attr-list:: Node -> Nodelist
Returns the sequence of attribute nodes (attribute/value vectors) for a 
given cxml node if it is an element and has attributes, otherwise nil.
The attributes in the result sequence are elements in cxml (ie. they 
satisfy the element? predicate), as opposed to w3c XPath."
  [n]
    (when (element? n)
      (let [ snd (second n) ]
        (when (map? snd) (seq snd)))))


(defn attr-map
  "attr-map:: Node -> AttributesMap
Returns the attributes map for a given cxml node if it is an element and has attributes,
otherwise nil.  The keys of this map may be simple symbols or [uri name-sym] vectors."
  [n]
    (if (element? n)
      (let [ snd (second n) ]
        (if (map? snd) snd nil))
      nil))


;; ------------------------------------------------------------------------------------
;; Axes

; Attribute axis

; Was: sxml:attribute
(defn attribute
  "attribute:: Converter|Predicate -> Node|Nodelist -> Nodelist
ie attribute:: Converter|Predicate -> Converter
Represents the attribute axis.  Returns a converter which produces attributes of its
argument node or nodes which satisfy the passed convertor or predicate."
  [conv-pred]
    (let [ attrs-selector (fn [node] ((filter-nodes conv-pred) (attr-list node))) ]
      (fn [n-nl] 
		  (mapcat attrs-selector
				  (as-nodelist n-nl)))))


;Child axis

; Was: sxml:child
(defn child
  "child:: Converter|Predicate -> Node|Nodelist -> Nodelist
ie child:: Converter|Predicate -> Converter
Represents the child axis.
Returns a converter which selects from its argument node or nodes all the element
and data child nodes satisfying the passed converter/predicate. This function
is similar to 'select-kids', but it returns an empty nodelist for the special
*PI*, *COMMENT* and *ENTITY* elements, and will only select element or data
nodes into the results, unlike the original Scheme implementation.  This was done
for XPath conformance and for consistency with the descendant axis implmentation
below.  Use select-kids instead if more inclusive selection is wanted."
  [conv-pred]
    (fn [n-nl]       ; node or nodelist
		(let [ tag (tag n-nl) ]
		  (cond
			 ;; Element-like node
		     tag (if (special-nonelement-tag? tag)
                   nil
                   ;; element node, apply test to child element and data content as per XPath
                   (let [sel (fn [n] 
                                 (if (or ((ntype?? NTYPE-ELEMENT-SYM) n)
                                         ((ntype?? NTYPE-DATA-SYM) n))
                                   (conv-pred n)
                                   nil))]
                     ((filter-nodes sel) (rest n-nl))))
             ;; Nodelist
			 (seq? n-nl) (mapcat (child conv-pred) n-nl) 
			 ;; Anything else
			 :else nil))))

; Convenience wrapper
; Was sxml:child-nodes
(defn child-nodes
  "child-nodes:: Node|Nodelist -> Nodelist,
ie child-nodes:: Converter
A converter selecting all element and data child nodes of its argument node or nodes."
  [n-nl] 
    ((child identity) n-nl))


;Parent axis

; TODO: how about checking in metadata for a direct parent link (and modify the parser accordingly)?
; Was: sxml:parent
(defn parent
  "parent:: Converter|Predicate -> Node|Nodelist -> Node|Nodelist -> Nodelist,
ie parent:: Converter|Predicate -> Node|Nodelist -> Converter
Represents the parent axis.
Applied to a convertor or predicate and then a root node or nodes, it returns
a converter which selects the parent nodes of its argument nodes if they exist
as descendants of the root nodes. As in XPath, the parent of individual
attributes (symbol/value vectors) is their containing element, not the collection
of attributes of which they are an entry, even though the attribute is not
considered a child of the element (again, as in XPath). The attribute collection
has no parent in the sense of this function.  The root nodes do not have to be the
root nodes of whole cxml trees -- they may be root nodes of branches of interest.
This parent axis can be used with any cxml node."
  [conv-pred]
    (fn [root-n-nl] ; node or nodelist
        (fn [n-nl]  ; node or nodelist
            (if (nodelist? n-nl)
              (mapcat ((parent conv-pred) root-n-nl) n-nl)
			  ; single node
			  (let [ =n-nl? (node-equal? n-nl) ]
				(loop [ parent-child-pairs (mapcat (fn [parent-n]
													   (map #(vector parent-n %)
															(concat (attr-list parent-n)
																	(child-nodes parent-n))))
												   (as-nodelist root-n-nl)) ]
				  (if (nil? parent-child-pairs)
					nil ; parent of our node n-nl not found
					(let [ fst-pc (first parent-child-pairs)
                           fst-c (fst-pc 1) ]
					  (if (=n-nl? fst-c)
						;; found it: test the parent and we're done
						(if ((as-predicate conv-pred) (fst-pc 0))
						  (make-nodelist (fst-pc 0))
						  nil)
						;; not found - check our child node's own children before searching the rest (depth first search)
						(recur (concat (map #(vector fst-c %)
											(concat (attr-list fst-c)
													(child-nodes fst-c)))
									   (rest parent-child-pairs))))))))))))


; Ancestor axis

; Implementation function for the following ancestor aces functions, parameterized over whether to include the nodes themselves or proper ancestors only.
(defn- ancestor-impl
  [include-selves]
  (fn [conv-pred]
    (fn [root-n-nl]   ; node or nodelist
      (fn [n-nl]      ; node or nodelist
		 (if (nodelist? n-nl)
		   (mapcat (((ancestor-impl include-selves) conv-pred) root-n-nl) n-nl)
		   ; n-nl is a single node
		   (let [ pred? (as-predicate conv-pred)
				  =n-nl? (node-equal? n-nl) ]
			 (loop [ paths (map make-nodelist (as-nodelist root-n-nl)) ] ; paths is a collection of nodelists, each ending at a root
			   (if (nil? paths)
				 ; no ancestor, maybe return self
				 (if (and include-selves (pred? n-nl))
				   (make-nodelist n-nl)
				   nil)
				 ; more paths to search
				 (let [ path (first paths)
						leaf-node (first path) ]
				   (if (=n-nl? leaf-node)                                            ; Does this path to a root start at our node?
					 ((filter-nodes conv-pred) (if include-selves path (rest path))) ; - yes, and so we have our ancestors
					 (recur (concat (map #(cons % path)                              ; - no, continue searching
										 (concat (attr-list leaf-node)
												 (child-nodes leaf-node)))
									(rest paths)))))))))))))


; Was sxml:ancestor
(def 
 #^{:arglists '([conv-pred])
    :doc
  "ancestor:: Converter|Predicate -> Node|Nodelist -> Node|Nodelist -> Nodelist,
ie ancestor:: Converter|Predicate -> Node|Nodelist -> Converter
Represents the ancestor axis.
Applied to a convertor or predicate and then a root node or nodes, it returns a
converter which selects the ancestor nodes of its argument nodes if they exist as
descendants of the root nodes and satisfy the predicate.  Attribute/value pairs
(vectors) have ancestors starting with the element containing the attribute
collection they are a part of, but the attribute collections (maps) do not
themselves have any ancestors in the sense of this function. The root nodes do not
have to be the root nodes of whole cxml trees -- they may be root nodes of branches
of interest.  This ancestor axis can be used with any cxml node."}
 ancestor (ancestor-impl false))


; Ancestor-or-self axis

; Was: sxml:ancestor-or-self
(def
 #^{:arglists '([conv-pred])
    :doc
  "ancestor-or-self:: Converter|Predicate -> Node|Nodelist -> Node|Nodelist -> Nodelist,
ie ancestor-or-self:: Converter|Predicate -> Node|Nodelist -> Converter
Represents the ancestor-or-self axis.
Applied to a convertor or predicate and then a root node or nodes, it returns a converter
which selects the argument nodes themselves that satisfy the predicate, together with
ancestor nodes of the argument nodes if they exist as descendants of the root nodes and
satisfy the predicate.  Attribute/value pairs (vectors) have ancestors starting with the
element containing the attribute collection they are a part of, but the attribute
collections (maps) do not themselves have any ancestors in the sense of this function.
The root nodes do not have to be the root nodes of whole cxml trees - they may be root
nodes of branches of interest.  This ancestor-or-self axis can be used with any cxml node."}
  ancestor-or-self (ancestor-impl true))

                                                                      

; Descendant axis

; Implementation function, parameterized over whether to include the nodes themselves or proper descendants only.
(defn- descendant-impl
  [include-selves]
  (fn [conv-pred]
    (fn [n-nl]       ; node or nodelist
	   (cond
		 ; Nodelist
		 (nodelist? n-nl) 
		   (mapcat ((descendant-impl include-selves) conv-pred) n-nl)
		 ; Element
		 (element? n-nl)
		   (let [ pred? (as-predicate conv-pred) ]
			 (loop [ sels nil
					 candidate-nodes (if include-selves (make-nodelist n-nl) (child-nodes n-nl)) ]
			   (if (nil? candidate-nodes)
				 (reverse sels)
				 (let [ node (first candidate-nodes) ]
				   (recur (if (pred? node)
							(cons node sels)
							sels)
						  (concat (child-nodes node)
								  (rest candidate-nodes)))))))
		 ; Anything else
		 :else nil))))

(def
 #^{:arglists '([conv-pred])
    :doc
  "descendant:: Converter|Predicate -> Node|Nodelist -> Nodelist
ie descendant:: Converter|Predicate -> Converter
Represents the descendant axis.
Returns a converter which selects for its argument node or nodes the
element and data nodes satisfying the passed converter/predicate which are 
reachable from them by recursively traversing child elements before child
node selection.  The nodes are returned in document (depth first) order
within the context of each input node. This function represents the
transitive closure of the child axis function.  For XPath conformance,
this implementation is more restrictive than the original Scheme version
(which had been marked to fix in this regard), which would descend into
attribute collections at any level, and *PI*, *COMMENT* and *ENTITY* nodes
at the top level.  For a slightly less restrictive (and breadth-first)
implementation of a similar idea, see node-closure."}
   descendant (descendant-impl false))


; Descendant-or-self axis

(def
 #^{:arglists '([conv-pred])
    :doc
  "descendant-or-self:: Converter|Predicate -> Node|Nodelist -> Nodelist
ie descendant-or-self:: Converter|Predicate -> Converter
Represents the descendant-or-self axis.
Returns a converter which selects from its argument node or nodes those which
satisfy the passed converter/predicate, plus those element and data nodes
satisfying the passed converter/predicate which are either direct children of
an argument node or are reachable from an argument node by recursively traversing
child elements before child node selection.  The result nodes for each input node
are returned in document (depth first) order.  For XPath conformance, this
implementation is more restrictive than the original Scheme version (which had
been marked to fix in this regard), which would descend into attribute collections
at any level, and *PI*, *COMMENT* and *ENTITY* nodes at the top level. For a slightly
less restrictive (and breadth-first) implementation of a similar idea, see node-closure."}
   descendant-or-self (descendant-impl true))



; TODO: examine the following for correctness (check w3c defs) and add unit tests

; Following axis
(defn following [conv-pred]
  (fn [root-n-nl]   ; node or nodelist
    (fn [n-nl]      ; node or nodelist
      (if (nodelist? n-nl)
		(mapcat ((following conv-pred) root-n-nl) n-nl)
		; n-nl is a single node
		(let [ =n-nl? (node-equal? n-nl) ]
		  (loop [ seq (map make-nodelist (as-nodelist root-n-nl)) ]
			(cond
			   (nil? seq) nil

			   (nil? (first seq)) (recur (rest seq))

			   ;; found our node as an element
			   (=n-nl? (ffirst seq))
			     (reduce (fn [res node] (concat res ((descendant-or-self conv-pred) node)))
						 nil
						 (rest (apply concat seq)))
		   
			   ;; found our node as an attribute
			   (and (element? (ffirst seq))
					(some =n-nl? (attr-list (ffirst seq))))
			     (reduce (fn [res node] (concat res ((descendant-or-self conv-pred) node)))
						 ((descendant conv-pred) (ffirst seq)) ; TODO: Watch out here, the attributes are not considere descendants anymore
						 (rest (apply concat seq)))

			   :else
			     (recur (cons (child-nodes (ffirst seq))
							  (cons (rfirst seq) (rest seq)))))))))))

; TODO: finish from here
(comment

; Following-sibling axis
(define (following-sibling conv-pred)
  (lambda (root-n-nl)   ; node or nodelist
    (lambda (n-nl)   ; node or nodelist
      (if (nodelist? n-nl)
	(mapcat ((following-sibling conv-pred) root-n-nl) n-nl)
	(let loop ((seqs (if (nodelist? root-n-nl)
			   (list root-n-nl)
			   (list (list root-n-nl)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map child-nodes
			      (car seqs))
			 (cdr seqs))))
		((eq? (car seq) n-nl) ((filter-nodes conv-pred) (cdr seq)))
		(:else (rpt (cdr seq)))))))))))

; Namespace axis
(define (namespace conv-pred)
  (lambda (n-nl)   ; node or nodelist
    ((filter-nodes conv-pred) 
     (ns-list n-nl)))) ; TODO

; Preceding axis
(define (preceding conv-pred)
  (lambda (root-n-nl)   ; node or nodelist
    (lambda (n-nl)   ; node or nodelist
      (if (nodelist? n-nl)
	(mapcat ((preceding conv-pred) root-n-nl) n-nl)
	(let loop ((seq (if (nodelist? root-n-nl)
			  (list (reverse root-n-nl))
			  (list (list root-n-nl)))))
	  (cond
	    ((null? seq) '())
	    ((null? (car seq)) (loop (cdr seq)))
	    ((or (eq? (ffirst seq) n-nl)
		 (not (null? ((attribute 
				(lambda (n)
				  (eq? n n-nl))) 
			      (ffirst seq)))))
	     (let rpt ((seq (cdr (apply append seq)))
		       (res '()))
	       (if (null? seq)
		 res
		 (rpt (cdr seq)
		      (append res
			      (reverse ((descendant-or-self conv-pred) 
					(car seq))))))))
	    (:else (loop (cons (reverse (child-nodes (ffirst seq)))
			      (cons (rfirst seq) (cdr seq)))))))))))

; Preceding-sibling axis
(define (preceding-sibling conv-pred)
  (lambda (root-n-nl)   ; node or nodelist
    (lambda (n-nl)   ; node or nodelist
      (if(nodelist? n-nl)
	(mapcat ((preceding-sibling conv-pred) root-n-nl) n-nl)
	(let loop ((seqs (if (nodelist? root-n-nl)
			   (list root-n-nl)
			   (list (list root-n-nl)))))
	  (if (null? seqs)
	    '()
	    (let rpt ((seq (car seqs)))
	      (cond
		((null? seq)
		 (loop (append
			 (map
			   (lambda (n)
			     (reverse (child-nodes n)))
			   (car seqs))
			 (cdr seqs))))
		((eq? (car seq) n-nl) ((filter-nodes conv-pred) (cdr seq)))
		(:else (rpt (cdr seq)))))))))))

) ; commented region




; node-parent:: RootNode -> Converter
; (node-parent rootnode) yields a converter that returns a parent of a
; node it is applied to. If applied to a nodelist, it returns the list
; of parents of nodes in the nodelist.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;  parent(x) = { y | y=subnode*(root), x=subnode(y) }
; Therefore, node-parent is not the fundamental converter: it can be
; expressed through the existing ones.  Yet node-parent is a rather
; convenient converter. It corresponds to a parent:: axis of SXPath.
;
; Please note: this function is provided for backward compatibility 
; with SXPath/SXPathlib ver. 3.5.x.x and earlier.
; Now it's a particular case of 'parent' application: 
(def node-parent (parent (ntype?? NTYPE-ANY-SYM)))

