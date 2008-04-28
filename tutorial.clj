(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxpath.clj")


(def doc-node '(*TOP*
                  (*PI* "myapp" "a processing instruction")
                  (customer
                    (account {title "Savings 1" created "3/5/2008"}
                      (owner "12398")
                      (balance {currency "USD"} "3212.12")
                      (descr-html "Main " (b "short term savings") " account.")
                      (report-separator "  ")
                      (*PI* "myapp" "another processing instruction"))
                    (account {title "Checking" created "4/27/2008"}
                      (owner "12398")
                      (balance {currency "PND"} "123.00")
                      (descr-html "Primary" (b "checking") " account.")))))

(comment - TODO are the attribute maps causing the difference here?
;; Obtaining the same document by parsing:
(let [xml-str 
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<customer>
 <account title=\"Savings 1\" created='3/5/2008'>
   <owner>12398</owner>
   <balance currency=\"USD\">3212.12</balance>
   <descr-html>Main <b>short term savings</b> account.</descr-html>
   <report-separator>  </report-separator>
   <?myapp another processing instruction?>
 </account>
 <account title=\"Checking\" created='4/27/2008'>
   <owner>12398</owner>
   <balance currency=\"PND\">123.00</balance>
   <descr-html>Primary <b>checking</b> account.</descr-html>
 </account>
</customer>"]
  (assert (= doc-node
             (cxml/parse-to-list (new java.io.StringReader xml-str)))))
)

;; Simple element selection
;; A path step which is a tag just selects the child elements from its input nodes
;; which have the required tag.
(def v1
     ((cxpath '(customer account balance))
        doc-node))

;; Make result tests a little more readable.
(defn results [val1 val2] (assert (= val1 val2)))

(results v1 
        '( (balance {currency "USD"} "3212.12")
           (balance {currency "PND"} "123.00") ))



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
     ((cxpath '(customer account descr-html *)) 
        doc-node))


(results v2a
         '( (b "short term savings")
            (b "checking") ))

;; Text selection - <text>
;; The <text> step selects any child nodes which are text nodes.  This is *not* the concatenated text of
;; all descendants, we would use concatenated-descendants-text for that (example below).
(def v2b 
     ((cxpath '(customer account descr-html <text>))
        doc-node))

(results v2b
         '( "Main " " account." "Primary" " account." ))


;; The attribute axis - <a>
;; This axis selects any kid nodes which are attribute collections (maps).
(def v2c
     ((cxpath '(customer account <a>))
        doc-node))

(results v2c 
         '( {title "Savings 1", created "3/5/2008"}
            {title "Checking", created "4/27/2008"} ))


;; Attribute "elements"
;; Once we are descended into the attribute axis, attributes are treated as regular cxml
;; elements, and will match the * node type.  Attributes are always map entries (vectors).
(def v2d
     ((cxpath '(customer account <a> *))
        doc-node))

(results v2d
         '( [title "Savings 1"]
            [created "3/5/2008"]
            [title "Checking"]
            [created "4/27/2008"] ))

;; Specific attribute "element"
(def v2e
     ((cxpath '(customer account <a> title))
        doc-node))

(results v2e
         '( [title "Savings 1"]
            [title "Checking"] ))

;; Attribute values
(def v2f
     ((cxpath '(customer account <a> title <text>))
        doc-node))

(results v2f
         '( "Savings 1" "Checking" ))


;; Descendants along the child axis - /
;; The / step selects the input nodes themselves and their child nodes
;; (elements and data nodes, excluding attribute collections and attributes),
;; and any nodes reachable recursively in this same way through child elements.
;; This example shows all descendants of the customer/account/descr-html elements.
(def v3a
     ((cxpath '(customer account descr-html /))
        doc-node))

(results v3a
         '( (descr-html "Main " (b "short term savings") " account.")
            "Main "
            (b "short term savings")
            "short term savings"
            " account."
            (descr-html "Primary" (b "checking") " account.")
            "Primary" 
            (b "checking")
            "checking"
            " account." ) )


;; This example selects all element descendants of the customer/account/descr-html elements.
(def v3b
     ((cxpath '(customer account descr-html / *))
        doc-node))

(results v3b
         '( (b "short term savings")
            (b "checking") ))


;; Selecting among alternatives: |or| and |alt|


;; Alternatives without subpath projection - |or|
;; The |or| operator selects kids that match any number of listed tags,
;; node types, or cxpath expressions.  A node is considered to match a cxpath
;; expression if the path yields at least one result when applied to the node.
;; An example of the latter follows this one. This example just selects nodes 
;; matching either of two tag alternatives.
(def v4a
     ((cxpath '(/ account (|or| owner balance)))
        doc-node))

(results v4a
         '( (owner "12398") (balance {currency "USD"} "3212.12")
            (owner "12398") (balance {currency "PND"} "123.00") ))

;; The |or| operator can also select nodes based on their general types (*, <a>,
;; <text>, <data>, <any>).  And if cxpath expressions are supplied, |or| will select
;; any nodes which yield results when at least one of the path expressions yields a
;; non-empty nodelist when applied to the node. This example selects any descendants
;; of account which are either text nodes, an owner element, an element having a currency
;; attribute (a subpath), or an element having a "b" subelement (also a subpath).
(def v4b
     ((cxpath '(customer account / (|or| <text> owner (<a> currency) (b))))
        doc-node))

(results v4b
         '( (owner "12398")
            (balance {currency "USD"} "3212.12")
            (descr-html "Main " (b "short term savings") " account.")
            "12398" "3212.12" "Main " " account." "short term savings" "  " 
            (owner "12398")
            (balance {currency "PND"} "123.00")
            (descr-html "Primary" (b "checking") " account.")
            "12398" "123.00" "Primary" " account." "checking" ))


;; Alternatives with subpath projection - |alt|
;; The |alt| operator is like |or|, but with subpaths projecting their results into the
;; result, instead of just being existence tests for selecting among the input nodes.
;; Notice that in the example below, the currency attribute itself is yielded from the
;; (<a> currency) subpath, instead of the account/balance element the subpath was
;; applied to.
(def v4c
     ((cxpath '(customer account / (|alt| <text> owner (<a> currency))))
        doc-node))

(results v4c
         '( (owner "12398")
            "12398" "3212.12" "Main " " account." "short term savings" "  "
            (owner "12398")
            "12398" "123.00" "Primary" " account." "checking"
            [currency "USD"]
            [currency "PND"] ))


;; Node equality - =
;; The (= <node>) path step selects kids which are equal to the given node.
;; Node equality tests are mainly useful as filters in subpath expressions.
;; The next example is the same as the |or| example from above, but this time
;; we require a currency = "USD" within the subpath alternative.
(def v5
     ((cxpath '(customer account / (|or| owner (<a> currency (= "USD")))))
        doc-node))

(results v5
         '( (owner "12398")
            (balance {currency "USD"} "3212.12")
            (owner "12398") ))

;; Negation of alternatives - |not|
;; The |not| operator is the complement of the |or| operator: it selects
;; a node whenever the |or| operator would not have.
(def v6
     ((cxpath '(customer account / (|not| <text> owner (<a> currency (= "USD")))))
        doc-node))

(results v6
         '( {title "Savings 1", created "3/5/2008"}
            (descr-html "Main " (b "short term savings") " account.")
            (report-separator "  ")
            (*PI* "myapp" "another processing instruction")
            {currency "USD"}
            (b "short term savings")
            {title "Checking", created "4/27/2008"}
            (balance {currency "PND"} "123.00")
            (descr-html "Primary" (b "checking") " account.")
            {currency "PND"}
            (b "checking") ))


;; Parent selection - ..
;; The .. operator selects the parent nodes of its input nodes.
(def v8a
     ((cxpath '(/ b ..)) 
        doc-node))

(results v8a
         '( (descr-html "Main " (b "short term savings") " account.")
            (descr-html "Primary" (b "checking") " account.") ))




;; As in w3c XPath, the parent of an attribute is its containing element, not the
;; attribute collection. There's an asymmetry here, just as in w3c XPath, because
;; the attributes are not reachable on the child axis from the parent.
(def v8b
     ((cxpath '(customer account <a> title ..))
        doc-node))

(results v8b
         '((account {title "Savings 1", created "3/5/2008"} (owner "12398") (balance {currency "USD"} "3212.12") (descr-html "Main " (b "short term savings") " account.") (report-separator "  ") (*PI* "myapp" "another processing instruction"))
           (account {title "Checking", created "4/27/2008"} (owner "12398") (balance {currency "PND"} "123.00") (descr-html "Primary" (b "checking") " account."))))


;; Ancestor selection - ..*
;; The ..* operator selects the input nodes themselves plus any nodes
;; reachable through some number of applications of the parent operator.
;; The parent of the document element is the root element, *TOP*.
(def v9
     ((cxpath '(customer account <a> title ..*))
        doc-node))

(results v9
         (list
           '[title "Savings 1"] ; self 
           '(account {title "Savings 1", created "3/5/2008"} (owner "12398") (balance {currency "USD"} "3212.12") (descr-html "Main " (b "short term savings") " account.") (report-separator "  ") (*PI* "myapp" "another processing instruction")) ; parent
           '(customer (account {title "Savings 1", created "3/5/2008"} (owner "12398") (balance {currency "USD"} "3212.12") (descr-html "Main " (b "short term savings") " account.") (report-separator "  ") (*PI* "myapp" "another processing instruction")) (account {title "Checking", created "4/27/2008"} (owner "12398") (balance {currency "PND"} "123.00") (descr-html "Primary" (b "checking") " account."))) ; parent of parent
           doc-node ; entire document
           '[title "Checking"]
           '(account {title "Checking", created "4/27/2008"} (owner "12398") (balance {currency "PND"} "123.00") (descr-html "Primary" (b "checking") " account."))
           '(customer (account {title "Savings 1", created "3/5/2008"} (owner "12398") (balance {currency "USD"} "3212.12") (descr-html "Main " (b "short term savings") " account.") (report-separator "  ") (*PI* "myapp" "another processing instruction")) (account {title "Checking", created "4/27/2008"} (owner "12398") (balance {currency "PND"} "123.00") (descr-html "Primary" (b "checking") " account.")))
           doc-node))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subpath expressions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subpath expressions are a powerful query tool, allowing arbitrary cxpath expressions
;; to be used to filter nodes based on their content, without actually descending into
;; (projecting) the tested content itself.  Subpath expressions have the special form:
;; '({symbol|path} {number|path}*), where path means any valid cxpath.
;;
;; The initial symbol or path step does the primary selection and projection, determining a
;; superset of the nodes to be returned by the subpath. The subsequent number or path entries
;; serve as filters on the initial step's selections, applied independently of each other
;; (without projection). A numeric filter entry selects a kid node by position starting with 1,
;; with negative numbers counted from the last.  A list filter entry can be any full cxpath
;; expression and serves as an existence test, where presence of results when the path is
;; applied indicates success.


;; In the example below, we choose the text of any subelement of account having a currency
;; attribute of "USD".  Here the subpath (* (<a> currency (= "USD"))) represents elements
;; having an attribute currency of "USD".  The initial * determines the subpath's initial
;; results, to be filtered by the embedded subpath (<a> currencty (= "USD")) to form the
;; final results for the subpath.
(def v10a
     ((cxpath '(customer account (* (<a> currency (= "USD"))) <text>) )
      doc-node))

(results v10a
         '( "3212.12" ))


;; In this example we choose any elements having a balance subelement with a currency
;; attribute value of "USD", and from these elements choose their owner text.  Here
;; (* (balance <a> currency (= "USD"))) is a subpath expression, with * as the initial
;; selector (determining the type of results for the subpath), and
;; (balance <a> currency (= "USD"))) as a filter.  Finally, the subsequent "owner <text>"
;; after the subpath acts on the result elements of the subpath.
(def v10b
     ((cxpath '(customer (* (balance <a> currency (= "USD"))) owner <text> )) 
        doc-node))

(results v10b
         '( "12398" ))
           

;; This example is similar but illustrates using two filters in a subpath expression.
;; We retrieve the descr-html/b elements from under any elements having balance text of
;; "3212.12" and owner text of "12398".
(def v10c
     ((cxpath '(customer (* (balance (= "3212.12")) (owner (= "12398"))) descr-html b ))
        doc-node))

(results v10c
         '( (b  "short term savings") ))

;; Using subpaths to select nodes by position
;; This example selects the last child element of account.  Here the subpath is (* -1),
;; so we are selecting element children of account (the *), and -1 is the only filter,
;; meaning the selected elements must be the last child (first from the end).
(def v10d
     ((cxpath '(customer account (* -1)))
        doc-node))

(results v10d
         '( (report-separator "  ")
            (descr-html "Primary" (b "checking") " account.") ))


;; The initial item in a subpath may itself be a cxpath expression.
;; In this example we get the currency attribute text of any element having a certain balance
;; subelement. Here ((/ balance) ((= "3212.12"))) is a supbath expression.  Its results will
;; be a list of balance element nodes, since the head of the subpath is the cxpath (/ balance).
;; These balance elements are then filtered by the ((= "3212.12")) cxpath, to yield the
;; result values of the subpath expression.  The remaining path steps after the subpath,
;; <a> currency <text>, then select the currency from the subpath's balance results.
(def v10e
     ((cxpath '(customer ((/ balance) ((= "3212.12"))) <a> currency <text> ))
        doc-node))

(results v10e
         '( "USD" ))


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
          (account {title "Savings 1" created "3/5/2008"}
            (owner "12398")
            (balance {sb/currency "USD"} "3212.12") ; namespace on attribute
            (descr-html "Main " (html/b "short term savings") " account.") ; different namespace on subelement here
            (report-separator "  ")
            (*PI* "myapp" "another processing instruction")))))

;; The true expanded form of the above.
(results doc-node-ns
   '(*TOP* 
     (*PI* "myapp" "a processing instruction")
     (["http://some.bank.com/ns" account] {created "3/5/2008", title "Savings 1"}
        (["http://some.bank.com/ns" owner] "12398")
        (["http://some.bank.com/ns" balance] {["http://standards.org/banking" currency] "USD"} "3212.12")
        (["http://some.bank.com/ns" descr-html] "Main " (["http://www.w3.org/HTML/1998/html4" b] "short term savings") " account.")
        (["http://some.bank.com/ns" report-separator] "  ")
        (*PI* "myapp" "another processing instruction"))))

;; Long form
;; Long-form qualified tags can be used anywhere unqualified tags could be used.
(def v11a
     ((cxpath '(["http://some.bank.com/ns" account] ["http://some.bank.com/ns" owner]))
        doc-node-ns))

(results v11a
         '( (["http://some.bank.com/ns" owner] "12398") ))

;; Short form - using namespace prefixes
;; By passing in ns-uri's to cxpath as a second argument, namespace prefixes may be used on path
;; elements instead of using long-form tags. Only the default namespace is used in this case.
;; The prefixes are expanded prior to any processing, as if long-form tags ([uri sym]) tags had
;; been entered.
(def v11b
     ((cxpath '(account owner) 
              ns-uris) 
        doc-node-ns))

(results v11b
         '( (["http://some.bank.com/ns" owner] "12398") ))

;; An explict namespace prefix on one element, with the namespace of the account element defaulting.
(def v11c
     ((cxpath '(account / html/b) 
              ns-uris) 
        doc-node-ns))

(results v11c
         '( (["http://www.w3.org/HTML/1998/html4" b] "short term savings") ))

;; Namespace qualified attribute
(def v11d
     ((cxpath '(account balance <a> sb/currency <text>)
              ns-uris)
        doc-node-ns))

(results v11d
         '( "USD" ))


;; NOTE: The namespace prefix expansion done within cxpath expressions will currently apply a default
;; namespace if any to unprefixed _attribute_ tags as well as elements within the cxpath.  This
;; differs from xml which will not apply a default namespace to attributes (but where it is possible
;; to tell what's an attribute tag - in a cxpath expression it is generally not possible nor necessarily
;; desirable).  To prevent applying a default namespace on attributes when it's not intended, just use a
;; prefix on attributes that is mapped to nil in the prefix->uri map passed to the cxpath function.
;; In this example, account gets the default uri of "http://some.bank.com/ns", while the no-ns prefix on
;; the title attribute, which is mapped to nil, assures no namespace for the attribute.
(def v11e
     ((cxpath '(account <a> no-ns/title)
              (assoc ns-uris "no-ns" nil))
        doc-node-ns))

(results v11e
         '( [title "Savings 1"] ))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using clojure functions within paths
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A big advantage of SXPath and cxpath vs. the string-based w3c XPath is how natural it is to use
;; ordinary functions as custom path steps.


;; This example illustrates using a regular clojure function (f) as a filter in the middle of a path.
;; It selects all the attribute values from elements which have at least 2 attributes.  Here f assumes
;; that its input nodes will be collections (attribute maps), which is true because of the preceeding
;; part of the path.  Here cxp is a macro for applying cxpath which automatically quotes symbols in the
;; passed path and allows unquoting as in syntax quote.
(def v12a
     (let [f (fn [nodelist] 
                 (filter (fn [attrs-coll] (>= (count attrs-coll) 2)) nodelist))]
       ((cxp (/ <a> ~f * <text>)) ;; equiv. to: ((cxpath (list '/ '<a> f '* '<text>))
          doc-node)))

(results v12a
         '( "Savings 1" "3/5/2008" "Checking" "4/27/2008" ))

;; The function f in the above is a "converter", or a function that transforms a
;; nodelist to a nodelist.  The built-in path elements are themselves just shorthand
;; forms for converters.  Most builtin converters implictly select child nodes of the
;; passed nodes for filtering, transforming, etc: this is not the case for custom
;; converters, which are given as input the exact nodes produced from the previous parts
;; of the path, and from these may produce output nodes in any way whatsoever. In this
;; case we are filtering the (attribute collection) nodes without descending into them
;; in the custom converter, then descending again in the rest of the path.


;; This example uses custom converters to filter nodes in a subpath expression.
;; We select any html bold elements under child elements of account whose markup-stripped text
;; matches the regular expression pattern "[Mm]ain .* account". The descendants-text function
;; is a converter that strips markup, and it is followed by an application of the filter-nodes
;; function which takes a predicate and uses it to filter nodes.
(def v12b
     ((cxp (customer account (* (~concatenated-descendants-text ~(filter-nodes #(re-seq #"[Mm]ain .* account" %)))) b))
        doc-node))

(results v12b
         '( (b "short term savings") ))

;; (load-file "tutorial.clj") (in-ns 'cxpath)
