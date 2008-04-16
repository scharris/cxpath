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
(def xml-str 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<account title=\"Savings 1\" created='5/5/2008'>
  <owner>12398</owner>
  <balance currency=\"USD\">3212.12</balance>
  <descr-html>Main <b>short term savings</b> account.</descr-html>
  <report-separator>  </report-separator>
  <?myapp another processing instruction?>
</account>")

(assert (= doc-node
		   (cxml/parse-to-list (new java.io.StringReader xml-str))))


;; Simple element selection
(def v1 
	 ((cxpath '(account balance)) doc-node))
;; ((balance {currency "USD"} "3212.12"))

;; Element wildcard (*)
(def v2
	 ((cxpath '(account *)) doc-node))
;;( (owner "12398")
;;  (balance {currency "USD"} "3212.12")
;;  (descr-html "Main " (b "short term savings") " account.")
;;  (report-separator "  ") )


;; *or* operator
(def v3
	 ((cxpath '(account (*or* owner balance))) doc-node))
;; ( (owner "12398") (balance {currency "USD"} "3212.12") )
	
;; *not* operator
(def v4
	 ((cxpath '(account (*not* owner balance))) doc-node))
;; ( {title "Savings 1" created "5/5/2008"}  ; [attribute collections are nodes in cxpath, unlike w3c xpath]
;;	 (descr-html "Main " (b "short term savings") " account.")
;;	 (report-separator "  ")
;;	 (*PI* "myapp" "another processing instruction") )


;; Text selection
(def v5 
	 ((cxpath '(account owner *text*)) doc-node))
;; ("12398")


;; Attribute collections
(def v6 
	 ((cxpath '(account *at*)) doc-node))
;; ( {title "Savings 1" created "5/5/2008"} )

;; Attribute "elements" (once descended into the attribute axis, attributes are treated as regular cxml elements)
(def v7 
	 ((cxpath '(account *at* *)) doc-node))
;; ( [title "Savings 1"] [created "5/5/2008"] )
(def v8 
	 ((cxpath '(account *at* title)) doc-node))
;; ( [title "Savings 1"] )

;; Attribute values
(def v9 
	 ((cxpath '(account *at* title *text*)) doc-node))
;; ("Savings 1")


;; Descendants along child axis (**)
;;    - all element descendants of account
(def v10
	 ((cxpath '(account ** *)) doc-node))
;; ( (owner "12398")
;;   (balance {currency "USD"} "3212.12")
;;   (descr-html "Main " (b "short term savings") " account.")
;;   (b "short term savings")
;;   (report-separator "  ") )

;;    - all attribute nodes in the document
(def v11
	 ((cxpath '(** *at* *)) doc-node))
;; ( [title "Savings 1"] [created "5/5/2008"] [currency "USD"] )



;; Parent selection (..)
(def v12
	 ((cxpath '(** b ..)) doc-node))
;; ( (descr-html "Main " (b "short term savings") " account.") )

;; As in w3c XPath, the parent of an attribute "element" is its containing element, not the attribute collection.
(def v13
	 ((cxpath '(account *at* title ..)) doc-node))
;; ( (account ...) )


;; Ancestor selection (..*)
(def v14
	 ((cxpath '(account *at* title ..*)) doc-node))
;; ( [title "Savings 1"] ; self (think of ..* as meaning "0 or or more applications of ..")
;;   (account ...)       ; element containing the attribute
;;   (*TOP* ...) )       ; entire document


;; Mixing some of the above: find any attribute (*) of any ancestor of a currency attribute somewhere under /account.
(def v15
	 ((cxpath '(account ** *at* currency ..* *at* *)) doc-node))
;; ( [currency "USD"] [title "Savings 1"] [created "5/5/2008"] )


;; Sub-node equality (=)
;; The (= <node>) path step selects children and attribute collections contained (directly) within
;; the nodes fed into it which are equal to the given node.  Node equality tests are mainly useful
;; as filters in subpath expressions (see below).
;; Fetch the balance elements with a value of"3212.12".  This is done by selecting the account/balance/"3212"
;; elements themselves, then taking the parent.
(def v16
	 ((cxpath '(account balance (= "3212.12") ..)) doc-node))
;; ( (balance {currency "USD"} "3212.12") )



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subpath expressions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Subpath expressions are a powerful query tool, allowing arbitrary cxpath path expressions to be used to filter
;; nodes based on their content, without actually descending into (projecting) the tested content itself.
;; Subpath expressions have the special form: '(symbol|path path*), where path means any valid cxpath.
;; The initial symbol or path does the initial selection and projection, determining the type of nodes returned.
;; The subsequent path entries are only applied as filters independently to the nodes returned by the _initial_
;; symbol or path in the subpath expression.

;; Choose any elements having a balance subelement with a currency attribute value of "USD", and from these elements
;; choose their owner text.  In this example, the subpath is (* (balance *at* currency (= "USD"))), with * as the
;; initial selector and (balance ...) as the filter on those selected items (without projection).
(def v17
	 ((cxpath '((* (balance *at* currency (= "USD"))) owner *text*)) doc-node))
;; ("12398")
           
;; This example is similar but illustrates using two filters in a subpath expression.
;; Retrieve the descr-html/b elements from any elements having balance and owner subelement values of "3212.12" and "12398".
(def v18
	 ((cxpath '((* (balance (= "3212.12")) (owner (= "12398")) ) descr-html b)) doc-node))
;; ( (b  "short term savings") )

;; Get the currency of the account with a certain balance.
;; Here ((account balance) ((= "3212.12"))) is a supbath expression.  It will project out account balances (since the head of
;; the subpath is the cxpath (account balance).  These account balance elements are then filtered by the ((= "3212.12")) cxpath,
;; to yield the values of the subpath.  The remaining path steps <*at* currency *text*> then select the currency from the subpath's
;; balance results.
(def v19
	 ((cxpath '(((account balance) ((= "3212.12"))) *at* currency *text*)) doc-node))
;; ( "USD" )

;; TODO: custom clojure converters

;; TODO: namespace support


; (load-file "tutorial.clj") (in-ns 'cxpath)
