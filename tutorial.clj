(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxpath.clj")


(def doc-node '(*TOP*
                  (*PI* "myapp" "a processing instruction")
                  (account {title "Savings 1" created "5/5/2008"}
                    (owner "12398")
                    (balance {currency "USD"} "3212.12")
                    (descr-html "Main " (b "short term savings") " account.")
                    (report-separator "  ")
                    (*PI* "myapp" "another processing instruction"))))

;; Obtaining the same document by parsing:
(let [xml-str 
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<account title=\"Savings 1\" created='5/5/2008'>
  <owner>12398</owner>
  <balance currency=\"USD\">3212.12</balance>
  <descr-html>Main <b>short term savings</b> account.</descr-html>
  <report-separator>  </report-separator>
  <?myapp another processing instruction?>
</account>"]
  (assert (= doc-node
             (cxml/parse-to-list (new java.io.StringReader xml-str)))))


;; Simple element selection.
;; A path step which is a tag just selects the child elements from its input nodes
;; which have the required tag.
(def v1
     ((cxpath '(account balance))
        doc-node))
;; ==>  ((balance {currency "USD"} "3212.12"))


;; Element wildcard - *.
;; The * step selects any child node of type element.
;; Other node types can be used as path steps as well, which will 
;; select any kid nodes (ie. child nodes or attribute collections) of
;; the proper type.  The valid node types are:
;;    *      - element node
;;    <a>    - attribute collection (map)
;;    <text> - text node
;;    <data> - data node: text, number, boolean (latter 2 not produced by parser)
;;    <any>  - any node
(def v2a
     ((cxpath '(account *)) 
        doc-node))
;; ==> ( (owner "12398")
;;       (balance {currency "USD"} "3212.12")
;;       (descr-html "Main " (b "short term savings") " account.")
;;       (report-separator "  ") )


;; Text selection - <text>
;; The <text> step selects any child nodes which are text nodes.
(def v2b 
     ((cxpath '(account owner <text>))
        doc-node))
;; ==>  ("12398")


;; The attribute axis - <a>
;; This axis selects any kid nodes which are attribute collections (maps).
(def v2c
     ((cxpath '(account <a>))
        doc-node))
;; ==> ( {title "Savings 1" created "5/5/2008"} )


;; Descendants along the child axis - /
;; The / step selects the input nodes themselves and their child nodes
;; (elements and data nodes, excluding attribute collections and attributes),
;; and any nodes reachable recursively in this same way through child elements.
;; This example shows all descendants of the account element.
(def v3b
     ((cxpath '(account /))
        doc-node))
;; ==> '( (account ... <entire account element here> ...)
;;        (owner "12398") 
;;        "12398" 
;;        (balance {currency "USD"} "3212.12")
;;        "3212.12"
;;        (descr-html "Main " (b "short term savings") " account.")
;;        "Main "
;;        (b "short term savings")
;;        "short term savings"
;;        " account."
;;        (report-separator "  ")
;;        "  " )

;; This example selects all element descendants of the account element.
(def v3b
     ((cxpath '(account / *))
        doc-node))
;; ==> ( (owner "12398")
;;       (balance {currency "USD"} "3212.12")
;;       (descr-html "Main " (b "short term savings") " account.")
;;       (b "short term savings")
;;       (report-separator "  ") )


;; Selecting among alternatives: |or| and |alt|


;; Non-projecting alternatives - |or|
;; The |or| operator selects kids that match any number of listed tags,
;; node types, or cxpath expressions.  A node is considered to match a cxpath
;; expression if the path yields at least one result when applied to the node.
;; An example of the latter follows this one. This example just selects nodes 
;; matching either of two tag alternatives.
(def v4a
     ((cxpath '(account (|or| owner balance)))
        doc-node))
;; ==>  ( (owner "12398") (balance {currency "USD"} "3212.12") )


;; The |or| operator can also select nodes based on their general types (*, <a>,
;; <text>, <data>, <any>).  And if cxpath expressions are supplied, |or| will select
;; any nodes which yield results when at least one of the path expressions yields a
;; non-empty nodelist when applied to the node. This example selects any descendants
;; of account which are either text nodes, an owner element, an element having a currency
;; attribute (a subpath), or an element having a "b" subelement (also a subpath).
(def v4b
     ((cxpath '(account / (|or| <text> owner (<a> currency) (b))))
        doc-node))
;; ==>  ( (owner "12398")
;;        (balance {currency "USD"} "3212.12")
;;        (descr-html "Main " (b "short term savings") " account.")
;;        "12398" "3212.12" "Main " " account." "short term savings" "  ")


;; Projecting alternatives - |alt|
;; The |alt| operator is like |or|, but with subpaths projecting their results into the
;; result, instead of just being existence tests for selecting among the input nodes.
;; Notice that in the example below, the currency attribute itself is yielded from the
;; (<a> currency) subpath, instead of the account/balance element the subpath was
;; applied to.
(def v4c
     ((cxpath '(account / (|alt| <text> owner (<a> currency))))
        doc-node))
;; ==>  ( (owner "12398")
;;        "12398" "3212.12" "Main " " account." "short term savings" "  "
;;        [currency "USD"] )


;; Node equality - =
;; The (= <node>) path step selects kids which are equal to the given node.
;; Node equality tests are mainly useful as filters in subpath expressions.
;; The next example is the same as the |or| example from above, but this time
;; we require a currency = "USD" within the subpath alternative (giving the
;; same results).
(def v5
     ((cxpath '(account / (|or| <text> owner (<a> currency (= "USD")))))
        doc-node))
;; ==>  ( (owner "12398") (balance {currency "USD"} "3212.12") "12398" "3212.12" "Main " " account." "short term savings" "  " )


;; Negation of alternatives - |not|
;; The |not| operator is the complement of the |or| operator: it selects
;; a node whenever the |or| operator would not have.
(def v6
     ((cxpath '(account / (|not| <text> owner (<a> currency (= "USD")))))
        doc-node))
;; ==> ( {title "Savings 1", created "5/5/2008"}
;;       (descr-html "Main " (b "short term savings") " account.")
;;       (report-separator "  ")
;;       (*PI* "myapp" "another processing instruction")
;;       {currency "USD"}
;;       (b "short term savings") )



;; Attribute "elements" - * (again)
;; Once we are descended into the attribute axis, attributes are treated as regular cxml
;; elements, and will match the * node type.  Attributes are always map entries (vectors).
;; Select all attribute nodes in the document.
(def v7a
     ((cxpath '(/ <a> *))
        doc-node))
;; ==> ( [title "Savings 1"] [created "5/5/2008"] [currency "USD"] )

(def v7b
     ((cxpath '(account <a> title))
        doc-node))
;; ==> ( [title "Savings 1"] )


;; Attribute values
(def v7c 
     ((cxpath '(account <a> title <text>))
        doc-node))
;; ==> ("Savings 1")



;; Parent selection - ..
;; The .. operator selects the parent nodes of its input nodes.
(def v8a
     ((cxpath '(/ b ..)) 
        doc-node))
;; ==> ( (descr-html "Main " (b "short term savings") " account.") )


;; As in w3c XPath, the parent of an attribute is its containing element, not the
;; attribute collection. There's an asymmetry here, just as in w3c XPath, because
;; the attributes are not reachable on the child axis from the parent.
(def v8b
     ((cxpath '(account <a> title ..))
        doc-node))
;; ==> ( (account ...) )


;; Ancestor selection - ..*
;; The ..* operator selects the input nodes themselves plus any nodes
;; reachable through some number of applications of the parent operator.
;; The parent of the document element is the root element, *TOP*.
(def v9
     ((cxpath '(account <a> title ..*))
        doc-node))
;; ==> ( [title "Savings 1"] ; self
;;       (account ...)       ; element containing the attribute
;;       (*TOP* ...) )       ; entire document



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subpath expressions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subpath expressions are a powerful query tool, allowing arbitrary cxpath expressions
;; to be used to filter nodes based on their content, without actually descending into
;; (projecting) the tested content itself.  Subpath expressions have the special form:
;; '(symbol|path path*), where path means any valid cxpath. The initial symbol or path
;; does the initial selection and projection, determining the type of nodes returned.
;; The subsequent path entries are only applied as filters independently to the nodes
;; returned by the _initial_ symbol or path in the subpath expression.

;; In this example we choose any elements having a balance subelement with a currency
;; attribute value of "USD", and from these elements choose their owner text.  Here
;; (* (balance <a> currency (= "USD"))) is a subpath expression, with * as the initial
;; selector (determining the type of results for the subpath), and
;; (balance <a> currency (= "USD"))) as a filter.  Finally, the subsequent "owner <text>"
;; after the subpath acts on the result elements of the subpath.
(def v10a
     ((cxpath '( (* (balance <a> currency (= "USD"))) owner <text> )) 
        doc-node))
;; ==> ("12398")
           

;; This example is similar but illustrates using two filters in a subpath expression.
;; We retrieve the descr-html/b elements from under any elements having balance text of
;; "3212.12" and owner text of "12398".
(def v10b
     ((cxpath '( (* (balance (= "3212.12")) (owner (= "12398"))) descr-html b ))
        doc-node))
;; ==> ( (b  "short term savings") )

;; The initial item in a subpath may itself be a cxpath expression.
;; In this example we get the currency attribute text of any element having a certain balance
;; subelement. Here ((/ balance) ((= "3212.12"))) is a supbath expression.  Its results will
;; be a list of balance element nodes, since the head of the subpath is the cxpath (/ balance).
;; These balance elements are then filtered by the ((= "3212.12")) cxpath, to yield the
;; result values of the subpath expression.  The remaining path steps after the subpath,
;; <a> currency <text>, then select the currency from the subpath's balance results.
(def v10c
     ((cxpath '( ((/ balance) ((= "3212.12"))) <a> currency <text> ))
        doc-node))
;; ==> ( "USD" )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespace support
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define a mapping of prefixes to uri's.  Not strictly necessary but allows for convenient notation.
(def ns-uris {nil    "http://some.bank.com/ns",
              "html" "http://www.w3.org/HTML/1998/html4",
              "sb"   "http://standards.org/banking"})


;; The with-xmlns function expands tags having namespaces to the form [uri symbol].
;; Parsing would produce the same (expanded) form.
(def doc-node-ns 
     (with-xmlns ns-uris
        '(*TOP*
          (*PI* "myapp" "a processing instruction")
          (account {title "Savings 1" created "5/5/2008"}
            (owner "12398")
            (balance {sb/currency "USD"} "3212.12") ; namespace on attribute
            (descr-html "Main " (html/b "short term savings") " account.") ; different namespace on subelement here
            (report-separator "  ")
            (*PI* "myapp" "another processing instruction")))))

;; The true expanded form of the above.
(assert 
 (= doc-node-ns
    '(*TOP* 
      (*PI* "myapp" "a processing instruction")
      (["http://some.bank.com/ns" account] {created "5/5/2008", title "Savings 1"}
         (["http://some.bank.com/ns" owner] "12398")
         (["http://some.bank.com/ns" balance] {["http://standards.org/banking" currency] "USD"} "3212.12")
         (["http://some.bank.com/ns" descr-html] "Main " (["http://www.w3.org/HTML/1998/html4" b] "short term savings") " account.")
         (["http://some.bank.com/ns" report-separator] "  ")
         (*PI* "myapp" "another processing instruction")))))

;; Long form
;; Long-form qualified tags can be used anywhere unqualified tags could be used.
(def v11a
     ((cxpath '(["http://some.bank.com/ns" account] ["http://some.bank.com/ns" owner]))
        doc-node-ns))
;; ==> ( (["http://some.bank.com/ns" owner] "12398") )

;; Short form - using namespace prefixes
;; By passing in ns-uri's to cxpath as a second argument, namespace prefixes may be used on path
;; elements instead of using long-form tags. Only the default namespace is used in this case.
;; The prefixes are expanded prior to any processing, as if long-form tags ([uri sym]) tags had
;; been entered.
(def v11b
     ((cxpath '(account owner) 
              ns-uris) 
        doc-node-ns))
;; ==> ( (["http://some.bank.com/ns" owner] "12398") )

;; An explict namespace prefix on one element, with the namespace of the account element defaulting.
(def v11c
     ((cxpath '(account / html/b) 
              ns-uris) 
        doc-node-ns))
;; ==> ((["http://www.w3.org/HTML/1998/html4" b] "short term savings"))

;; Namespace qualified attribute
(def v11d
     ((cxpath '(account balance <a> sb/currency <text>)
              ns-uris)
        doc-node-ns))
;; ==> ( "USD" )


;; NOTE: The namespace prefix expansion done within cxpath expressions will currently apply a default
;;       namespace, if one is provided in the prefix->uri map (under nil key), to unprefixed _attribute_
;;       tags as well as elements within the cxpath.  This differs from xml which will not apply a default
;;       namespace to attributes (but where it is possible to tell what's an attribute tag).  To prevent
;;       this when using a default namespace, just use a prefix on attributes that is mapped to nil in 
;;       the prefix->uri map passed to the cxpath function.
;; In this example, account gets the default uri of "http://some.bank.com/ns", while the no-ns prefix on
;; the title attribute, which is mapped to nil, assures no namespace for the attribute.
(def v11e
     ((cxpath '(account <a> no-ns/title)
              (assoc ns-uris "no-ns" nil))
        doc-node-ns))
;; ==> '( [title "Savings 1"] )



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using clojure functions in paths
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The next example illustrates using a regular clojure function as a filter in the middle of a path.
;; Select all the attribute values from elements having at least 2 attributes.
(def v12a
     (let [f (fn [nodelist] 
                 (filter (fn [attrs-coll] (>= (count attrs-coll) 2)) nodelist))]
       ((cxpath (list '/ '<a> f '* '<text>)
                ns-uris)
          doc-node-ns)))
;; ==> ( "5/5/2008"  "Savings 1" )

;; The function f in the above is a "converter", or a function that transforms a
;; nodelist to a nodelist.  The built-in path elements are themselves just shorthand
;; forms for converters.  Most builtin converters implictly select child nodes of the
;; passed nodes for filtering, transforming, etc: this is not the case for custom
;; converters, which are given as input the exact nodes produced from the previous parts
;; of the path, and from these may produce output nodes in any way whatsoever. In this
;; case we are filtering the (attribute collection) nodes without descending into them
;; in the custom converter, then descending again in the rest of the path.

;; TODO: do a super-duper example here, of using a custom converter to alter text in nodes after some complex selection, then 
;; doing further path operations after that.


;; Note: Using keywords instead of symbols in the above path expression would have allowed use of syntax
;; quote (with symbols syntax quote will apply clojure namespaces which we don't want here).  The syntax
;; (/, <a>, <text>, etc) may be changed to use keywords instead of symbols for this reason, and/or a 
;; special replacement for syntax quote may be done instead which doesn't do Clojure namespace lookup.
;; The xml data itself already supports keywords for tags and attribute names as a parsing option and the
;; cxpath library already supports both.

;; (load-file "tutorial.clj") (in-ns 'cxpath)
