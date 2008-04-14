(in-ns 'cxpath)
(clojure/refer 'clojure)

(load-file "cxml.clj")
(load-file "cxpathlib.clj")
(load-file "cxpath.clj")



(import '(java.io StringReader))



(def doc-node '(*TOP*
				  (*PI* "myapp" "a processing instruction")
				  (account {title "Savings 1" created "5/5/2008"}
				    (ownerid "12398")
					(balance {currency "USD"} "3212.12")
					(descr-html "Main " (b "short term savings") " account.")
					(report-separator "  ")
					(*PI* "myapp" "another processing instruction"))))

(def ns-uris {nil    "http://some.bank.com/ns",
              "html" "http://www.w3.org/HTML/1998/html4",
              "sb"   "http://standards.org/banking"})

; The same document node with namespaces. Namespaces are stored in the metadatas of the tags.
(def doc-node-ns (with-xmlns ns-uris
                    '(*TOP*
                      (*PI* "myapp" "a processing instruction")
                      (account {title "Savings 1" created "5/5/2008"}
                        (ownerid "12398")
						(balance {sb/currency "USD"} "3212.12") ; namespace on attribute
						(descr-html "Main " (html/b "short term savings") " account.") ; different namespace on subelement here
						(report-separator "  ")
						(*PI* "myapp" "another processing instruction")))))

; Test that the with-xmlns function used above properly expands the namespace prefixes
(assert (= doc-node-ns
		   '(*TOP* 
			  (*PI* "myapp" "a processing instruction")
			  (["http://some.bank.com/ns" account] {created "5/5/2008", title "Savings 1"}
				 (["http://some.bank.com/ns" ownerid] "12398")
				 (["http://some.bank.com/ns" balance] {["http://standards.org/banking" currency] "USD"} "3212.12")
				 (["http://some.bank.com/ns" descr-html] "Main " (["http://www.w3.org/HTML/1998/html4" b] "short term savings") " account.")
				 (["http://some.bank.com/ns" report-separator] "  ")
				 (*PI* "myapp" "another processing instruction")))))

(def doc-node-ns-v (with-xmlns ns-uris
                    '[*TOP*
                      [*PI* "myapp" "a processing instruction"]
                      [account {title "Savings 1" created "5/5/2008"}
                        [ownerid "12398"]
						[balance {sb/currency "USD"} "3212.12"] ; namespace on attribute
						[descr-html "Main " [html/b "short term savings"] " account."] ; different namespace on subelement here
						[report-separator "  "]
						[*PI* "myapp" "another processing instruction"]]]))

(assert (= doc-node-ns-v
		   '[*TOP* 
			  [*PI* "myapp" "a processing instruction"]
			  [["http://some.bank.com/ns" account] {created "5/5/2008", title "Savings 1"}
			    [["http://some.bank.com/ns" ownerid] "12398"]
				[["http://some.bank.com/ns" balance] {["http://standards.org/banking" currency] "USD"} "3212.12"]
				[["http://some.bank.com/ns" descr-html] "Main " [["http://www.w3.org/HTML/1998/html4" b] "short term savings"] " account."]
				[["http://some.bank.com/ns" report-separator] "  "]
				[*PI* "myapp" "another processing instruction"]]]))


(def nodelist '((*PI* "myapp" "some p.i.")
				(el1 {a "a"} "text1" 
					 (nested1 "nestedtext1" (nested1a "nestedtext1a")))
				(el2 "text2" 
					 (nested2 "nestedtext2"))))

(def account-el '(account {title "Savings 1" created "5/5/2008"} 
				   (ownerid "12398")
				   (balance {currency "USD"} "3212.12")
				   (descr-html "Main " (b "short term savings") " account.")
				   (report-separator "  ") (*PI* "myapp" "another processing instruction")))

(def account-el-ns
	 (with-xmlns ns-uris
				 '(account {title "Savings 1" created "5/5/2008"}
				    (ownerid "12398")
					(balance {sb/currency "USD"} "3212.12") ; namespace on attribute
					(descr-html "Main " (html/b "short term savings") " account.") ; different namespace on subelement here
					(report-separator "  ")
					(*PI* "myapp" "another processing instruction"))))

; Parsing

(def xml-str 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<account title=\"Savings 1\" created='5/5/2008'>
  <ownerid>12398</ownerid>
  <balance currency=\"USD\">3212.12</balance>
  <descr-html>Main <b>short term savings</b> account.</descr-html>
  <report-separator>  </report-separator>
  <?myapp another processing instruction?>
</account>")

; Same but with namespaces.
(def xml-str-ns 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<account title=\"Savings 1\" created='5/5/2008' xmlns=\"http://some.bank.com/ns\">
  <ownerid>12398</ownerid>
  <balance xmlns:bs='http://standards.org/banking' bs:currency=\"USD\">3212.12</balance>
  <descr-html xmlns:h=\"http://www.w3.org/HTML/1998/html4\">Main <h:b>short term savings</h:b> account.</descr-html>
  <report-separator>  </report-separator>
  <?myapp another processing instruction?>
</account>")


(assert (= doc-node
		   (cxml/parse-to-list (new java.io.StringReader xml-str))))

  
(assert (= '[*TOP*
			 [*PI* "myapp" "a processing instruction"]
			 [account {title "Savings 1" created "5/5/2008"}
			   [ownerid "12398"]
			   [balance {currency "USD"} "3212.12"]
			   [descr-html "Main " [b "short term savings"] " account."]
			   [report-separator "  "]
			   [*PI* "myapp" "another processing instruction"]]]
		   (cxml/parse-to-vector (new java.io.StringReader xml-str))))


(assert (= doc-node-ns
           (cxml/parse-to-list (new java.io.StringReader xml-str-ns))))


; Keyword tags and attributes not supported for now, as keywords don't allow attaching the namespace uri as metadata.
;(assert (= '[*TOP*
;			 [*PI* "myapp" "a processing instruction"]
;			 [:account {:title "Savings 1" :created "5/5/2008"}
;			   [:ownerid "12398"]
;			   [:balance {:currency "USD"} "3212.12"]
;			   [:descr-html "Main " [:b "short term savings"] " account."]
;			   [:report-separator "  "]
;			   [*PI* "myapp" "another processing instruction"]]]
;		   (cxml/parse-to-vector (new java.io.StringReader xml-str) {:keyword-attributes? true :keyword-tags? true})))

; tag
(assert (= 'myel
		   (tag '(myel "text"))))
(assert (= '["http://a/b/c" el]
           (tag '(["http://a/b/c" el] "text"))))
(assert (= '["http://a/b/c" el]
           (tag '[["http://a/b/c" el] "text"])))
(assert (= nil
           (tag '(["http://a/b/c" el strange] "text"))))
(assert (= '*PI*
		   (tag '(*PI* "myapp" "text"))))
(assert (= 'myattr
		   (tag ['myattr "text"])))
(assert (= nil
		   (tag "text")))
(assert (= nil
		   (tag 2)))
(assert (= nil
		   (tag false)))

; element?-tag
(assert (= 'myel
		   (element?-tag '(myel "text"))))
(assert (= '["http://a/b/c" el]
           (element?-tag '(["http://a/b/c" el] "text"))))
(assert (= '["http://a/b/c" el]
           (element?-tag '[["http://a/b/c" el] "text"])))
(assert (= '*TOP*
           (element?-tag '(*TOP* (el "text")))))
(assert (= nil
		   (element?-tag '(*PI* "myapp" "processing instruction"))))
(assert (= nil
		   (tag "text")))
(assert (= nil
		   (tag 2)))
(assert (= nil
		   (tag false)))

; nodelist?
(assert (nodelist? '((el "text"))))
(assert (nodelist? '((el "text") "text")))
(assert (nodelist? '("text" (el "text") "text")))
(assert (nodelist? '(2 3)))
(assert (nodelist? '()))
(assert (nodelist? nil))
(assert (not (nodelist? "text")))
(assert (not (nodelist? 2)))
(assert (not (nodelist? '{a "a"})))
(assert (not (nodelist? '(el "text"))))
(assert (not (nodelist? '[myattr "val"])))

; as-nodelist
(assert (= (as-nodelist '(el "text"))
		   '((el "text"))))

; empty-nodelist?
(assert (empty-nodelist? '()))
(assert (empty-nodelist? nil))
(assert (not (empty-nodelist? (list 2 3))))

; element?
(assert (element? '(el "text")))
(assert (element? '(*TOP* "text")))
(assert (element? '[myattr "text"]))
(assert (not (element? '(*PI* "app" "text"))))
(assert (not (element? '(*COMMENT* "app" "text"))))
(assert (not (element? '(*ENTITY* "app" "text"))))
(assert (not (element? "text")))
(assert (not (element? '{a "a"})))
(assert (not (element? {})))
(assert (not (element? '())))
(assert (not (element? nil)))

; ntype-names??
(assert ((ntype-names?? '(myel other)) '(myel "text")))
(assert ((ntype-names?? '(attr1)) '[attr1 "val1"]))
(assert (not ((ntype-names?? '(myel other)) '(*TOP* "text"))))
(assert (not ((ntype-names?? '()) '(*TOP* "text"))))
(assert (not ((ntype-names?? '(myel)) "text")))
(assert (not ((ntype-names?? '(myel)) 2)))
(assert (not ((ntype-names?? '()) nil)))
(assert (not ((ntype-names?? '()) '())))
(assert (not ((ntype-names?? nil) nil)))

; ntype??
(assert ((ntype?? 'myel) '(myel "text")))
(assert ((ntype?? 'myattr) '[myattr "text"]))
(assert ((ntype?? '*) '(myel "text")))
(assert ((ntype?? '*) '[myattr "text"]))
(assert ((ntype?? '*at*) {'attr1 "val1" 'attr2 "val2"}))
(assert ((ntype?? '*TOP*) '(*TOP* (myel "text"))))
(assert ((ntype?? '*PI*) '(*PI* "app" "text")))
(assert ((ntype?? '*COMMENT*) '(*COMMENT* "text")))
(assert ((ntype?? '*ENTITY*) '(*ENTITY* "text")))
(assert ((ntype?? '*text*) "text"))
(assert ((ntype?? '*data*) "text"))
(assert ((ntype?? '*data*) 2)) ; non-string atomic data values aren't really produced by the parser but are supported anyway
(assert ((ntype?? '*data*) true))
(assert ((ntype?? '*any*) 2))
(assert (not ((ntype?? '*text*) '(myel "text"))))
(assert (not ((ntype?? '*data*) '(myel "text"))))
(assert (not ((ntype?? '*) "text")))
(assert (not ((ntype?? '*data*) ['myattr "text"])))
(assert (not ((ntype?? '*text*) ['myattr "text"])))
(assert (not ((ntype?? '*data*) ['myattr "text"])))

; ntype-namespace-id??
(assert ((ntype-namespace-id?? "http://a/b/c") '(["http://a/b/c" myel] "text")))
(assert ((ntype-namespace-id?? "http://a/b/c") '[["http://a/b/c" myattr] "text"]))
(assert ((ntype-namespace-id?? nil) '(myel "text")))
(assert (not ((ntype-namespace-id?? "http://a/b/c") '(myel "text"))))
(assert (not ((ntype-namespace-id?? "http://a/b/c") '(["http://a" myel] "text"))))
(assert (not ((ntype-namespace-id?? nil) '(["http://a" myel] "text"))))

; node-eq?
(assert (let [ x '(el "text"), y  x ] ((node-eq? x) y)))
(assert (not ((node-eq? '(el "text")) '(el "text"))))
(assert (not ((node-eq? '(el "text")) '(el "other text"))))

; node-equal?
(assert (let [ x '(el "text"), y  x ] ((node-equal? x) y)))
(assert ((node-equal? '(el "text")) '(el "text")))
(assert (not ((node-equal? '(el "text")) '(el "othertext"))))
(assert ((node-equal? '(el "text" (el2 "subeltext"))) '(el "text" (el2 "subeltext"))))
(assert (not ((node-equal? '(el "text" (el2 "subeltext"))) '(el "text" (el2 "othersubeltext")))))
(assert ((node-equal? '(el {attr "val"} "text" (el2 "subeltext"))) '(el {attr "val"} "text" (el2 "subeltext"))))
(assert (not ((node-equal? '(el {attr "val"} "text" (el2 "subeltext"))) '(el {attr "otherval"} "text" (el2 "othersubeltext")))))
(assert ((node-equal? '(["http:/a/b/c" el] "text")) '(["http:/a/b/c" el] "text")))
(assert (not ((node-equal? '(["http:/a/" el] "text")) '(["http:/a/b/c" el] "text"))))
(assert (not ((node-equal? '(el "text")) '(["http:/a/b/c" el] "text"))))


; node-pos
(assert (= '((a "a")) 
           ((node-pos 1) '((a "a") (b "b") (c "c")))))
(assert (= '([c "c"])
           ((node-pos -1) '([a "a"] [b "b"] [c "c"]))))
(assert (= nil 
           ((node-pos 1) '(a "a"))))
(assert (= nil 
           ((node-pos 1) '())))
(assert (= nil 
           ((node-pos 1) nil)))


(let [ two-els-nodelist '((el1 "text1") (el2 "text2")) ]

  ; filter-nodes
  (assert (= '((el2 "text2")) 
             ((filter-nodes (fn [node] (= 'el2 (first node)))) two-els-nodelist)))
  (assert (= '((el2 "text2"))
             ((filter-nodes (fn [node] (= 'el2 (first node)))) '(el2 "text2"))))
  (assert (= nil
             ((filter-nodes (fn [node] (= 'el3 (first node)))) two-els-nodelist)))
  (assert (= nil
             ((filter-nodes (fn [node] (= 'el3 (first node)))) '())))
  (assert (= nil
             ((filter-nodes (fn [node] (= 'el3 (first node)))) nil)))
  
  ; take-nodes-until
  (assert (= '((el1 "text1"))
			 ((take-nodes-until (fn [node] (= 'el2 (first node)))) two-els-nodelist)))
  (assert (= two-els-nodelist
			 ((take-nodes-until (fn [node] (= 'el3 (first node)))) two-els-nodelist)))
  (assert (= nil
			 ((take-nodes-until (fn [node] (= 'el3 (first node)))) '())))
  (assert (= nil
			 ((take-nodes-until (fn [node] (= 'el3 (first node)))) nil)))
  (assert (= nil
			 ((take-nodes-until (fn [node] (= 'el1 (first node)))) '((el1 "text1")))))
  ; take-nodes-after
  (assert (= '((el2 "text2"))
			 ((take-nodes-after (fn [node] (= 'el1 (first node)))) two-els-nodelist)))
  (assert (= nil
			 ((take-nodes-after (fn [node] (= 'el2 (first node)))) two-els-nodelist)))
  (assert (= nil
			 ((take-nodes-after (fn [node] (= 'el3 (first node)))) two-els-nodelist)))
  (assert (= nil
			 ((take-nodes-after (fn [node] (= 'el3 (first node)))) '())))
  (assert (= nil
			 ((take-nodes-after (fn [node] (= 'el3 (first node)))) nil)))
  
  ; map-union
  (assert (= '((el1 "text1") (el2 "text2") (el2 "text2"))
			 (map-union (fn [n] (if (= 'el1 (first n)) (list n) (list n n)))
						two-els-nodelist)))
  (assert (= '((el2 "text2") (el2 "text2"))
			 (map-union (fn [n] (if (= 'el1 (first n)) nil (list n n)))
						two-els-nodelist)))
  (assert (= '((el2 "text2") (el2 "text2"))
			 (map-union (fn [n] (if (= 'el1 (first n)) '() (list n n)))
						two-els-nodelist)))
  (assert (= '((el2 "text2") (el2 "text2"))
			 (map-union (fn [n] (if (= 'el1 (first n)) nil (list n n)))
						two-els-nodelist)))
  (assert (= nil
			 (map-union (fn [n] (if (= 'el1 (first n)) (list n) (list n n)))
						'())))
  (assert (= nil     
			 (map-union (fn [n] (if (= 'el1 (first n)) (list n) (list n n)))
						nil)))
  ; node-reverse
  (assert (= '((el2 "text2") (el1 "text1"))
			 (node-reverse two-els-nodelist)))
  (assert (= '((el1 "text1"))
			 (node-reverse '((el1 "text1")))))
  (assert (= nil
			 (node-reverse '())))
  (assert (= nil
			 (node-reverse nil)))
)


; select-kids
(assert (= '("text1")
		   ((select-kids (ntype?? '*text*)) '(el1 "text1"))))

(assert (= '({attr1 "val1"})
		   ((select-kids (ntype?? '*at*)) '(el1 {attr1 "val1"} "text1"))))

(assert (= '((*PI* "myapp" "a processing instruction"))
           ((select-kids (ntype?? '*PI*)) doc-node)))

(assert (= nil
	 ((select-kids (ntype?? '*data*)) doc-node)))

(assert (= '("myapp" "some p.i." "text1" "text2")
	 ((select-kids (ntype?? '*data*)) nodelist)))

(assert (= '("myapp" "some p.i." {a "a"} "text1" (nested1 "nestedtext1"  (nested1a "nestedtext1a")) "text2" (nested2 "nestedtext2"))
	 ((select-kids (ntype?? '*any*)) nodelist)))

(assert (= '((nested1 "nestedtext1" (nested1a "nestedtext1a")) (nested2 "nestedtext2"))
	 ((select-kids (ntype?? '*)) nodelist)))

(assert (= '({a "a"})
	 ((select-kids (ntype?? '*at*)) nodelist)))

; node-join
(assert (= '("12398" "3212.12" "Main " " account." "  ")
	 ((node-join (select-kids (ntype-names?? '(account)))
				 (select-kids (ntype?? '*))
				 (select-kids (ntype?? '*text*))) doc-node)))

(assert (= '("nestedtext1" "nestedtext2")
		   ((node-join (select-kids (ntype?? '*)) 
					   (select-kids (ntype?? '*text*))) nodelist)))

(assert (= '((nested1a "nestedtext1a"))
		   ((node-join (select-kids (ntype?? '*)) 
					   (select-kids (ntype?? '*))) nodelist)))
  
(assert (= '("nestedtext1a")
		   ((node-join (select-kids (ntype?? '*))
					   (select-kids (ntype?? '*))
					   (select-kids (ntype?? '*text*))) nodelist)))
; node-reduce
(assert (= '("12398" "3212.12" "Main " " account." "  ")
		   ((node-reduce (select-kids (ntype-names?? '(account)))
						 (select-kids (ntype?? '*))
						 (select-kids (ntype?? '*text*))) doc-node)))


(assert (= '("nestedtext1" "nestedtext2")
		   ((node-reduce (select-kids (ntype?? '*)) 
						 (select-kids (ntype?? '*text*))) nodelist)))

(assert (= '((nested1a "nestedtext1a"))
		   ((node-reduce (select-kids (ntype?? '*)) 
						 (select-kids (ntype?? '*))) nodelist)))
  
(assert (= '("nestedtext1a")
		   ((node-reduce (select-kids (ntype?? '*))
						 (select-kids (ntype?? '*))
						 (select-kids (ntype?? '*text*))) nodelist)))

; node-or
(assert (= '((el2 "text2" (nested2 "nestedtext2"))
			 (*PI* "myapp" "some p.i."))
		   ((node-or (filter-nodes (ntype-names?? '(el2))) 
					 (filter-nodes (ntype-names?? '(*PI*)))
					 (filter-nodes (ntype-names?? '(*ENTITY*)))) nodelist)))

(assert (= (list '(*PI* "myapp" "a processing instruction")
				 account-el)
		   ((node-or (select-kids (ntype?? '*PI*))
					 (select-kids (ntype?? '*)))  doc-node)))

; child-elements
(assert (= '((nested1 "nestedtext1" (nested1a "nestedtext1a"))
			 (nested2 "nestedtext2"))
		   (child-elements nodelist)))
  
; node-closure
(assert (= '("12398" "3212.12" "Main " " account." "  " "short term savings")
		   ((node-closure (ntype?? '*text*)) doc-node)))

(assert (= '("pi-text" "el-text")
		   ((node-closure (ntype?? '*text*)) '((*PI* "pi-text") (el "el-text")))))

(assert (= '("myapp" "some p.i." "text1" "text2" "nestedtext1" "nestedtext2" "nestedtext1a")
		   ((node-closure (ntype?? '*text*)) nodelist)))

(assert (= '({a "a"})
		   ((node-closure (ntype?? '*at*)) nodelist)))


; sxml-node?
(assert (xpath-node? '(el "text")))
(assert (xpath-node? "text"))
(assert (not (xpath-node? '{a "a"})))

; attr-list
(assert (=  (set '([b "b"] [a "a"]))
			(set (attr-list '(el {a "a" b "b"} "text")))))

; attr-map
(assert (=  '{a "a" b "b"}
			(attr-map '(el {a "a" b "b"} "text"))))

; attribute
(assert (= '([attr1 "val1"])
		   ((attribute #(= 'attr1 (first %))) '(el {attr1 "val1" attr2 "val2"} "text"))))


; child
(assert (= '("text1")
		   ((child (ntype?? '*text*)) '(el1 "text1"))))

(assert (= nil
		   ((child (ntype?? '*PI*)) doc-node))) ; special elements ignored

(assert (= nil
		   ((child (ntype?? '*data*)) doc-node)))

(assert (= '("text1" "text2")
		   ((child (ntype?? '*data*)) nodelist)))

(assert (= '("text1" (nested1 "nestedtext1"  (nested1a "nestedtext1a")) "text2" (nested2 "nestedtext2")) ; attr collection ignored
		   ((child (ntype?? '*any*)) nodelist)))

(assert (= '((nested1 "nestedtext1" (nested1a "nestedtext1a")) (nested2 "nestedtext2"))
		   ((child (ntype?? '*)) nodelist)))


; parent

(assert (= '((ownerid "12398"))
		   (((parent (ntype?? '*)) doc-node) "12398")))

(assert (= '((ownerid "12398")
			 (balance {currency "USD"} "3212.12"))
		   (((parent (ntype?? '*)) doc-node) '("12398" "3212.12"))))

(assert (= (list account-el)
		   (((parent (ntype?? '*)) doc-node) '(report-separator "  "))))


(assert (= (list '(balance {currency "USD"} "3212.12") 
				 account-el)
		   (((parent (ntype?? '*)) doc-node) '([currency "USD"] [title "Savings 1"]))))


; ancestor

(assert (= (list '(ownerid "12398") 
				 account-el
				 doc-node)
		   (((ancestor (ntype?? '*)) doc-node) "12398")))


(assert (= (list account-el
				 doc-node)
		   (((ancestor (ntype?? '*any*)) doc-node) '(ownerid "12398"))))

(assert (= (list account-el
				 doc-node)
		   (((ancestor (ntype?? '*any*)) doc-node) '[title "Savings 1"])))

(assert (= nil
		   (((ancestor (ntype?? '*any*)) doc-node) '{title "Savings 1" created "5/5/2008"}))) ; no ancestors for attribute collections



; ancestor-or-self

(assert (= (list '(ownerid "12398") 
				 account-el
				 doc-node)
		   (((ancestor-or-self (ntype?? '*)) doc-node) "12398")))

(assert (= (list "12398"
			     '(ownerid "12398") 
				 account-el
				 doc-node)
		   (((ancestor-or-self (ntype?? '*any*)) doc-node) "12398")))

(assert (= (list '(ownerid "12398") 
				 account-el
				 doc-node
				 '(balance {currency "USD"} "3212.12")
				 account-el
				 doc-node)
		   (((ancestor-or-self (ntype?? '*any*)) doc-node) '((ownerid "12398") (balance {currency "USD"} "3212.12")))))


; descendant

(assert (= '("text1")
		   ((descendant (ntype?? '*text*)) '(el1 "text1"))))

(assert (= nil
		   ((descendant (ntype?? '*PI*)) doc-node))) ; special elements ignored

(assert (= '("12398" "3212.12" "Main " "short term savings" " account." "  ")
		   ((descendant (ntype?? '*data*)) doc-node)))

(assert (= '("text1" "nestedtext1" "nestedtext1a" "text2" "nestedtext2")
		   ((descendant (ntype?? '*data*)) nodelist)))

(assert (= '("text1" 
			 (nested1 "nestedtext1" (nested1a "nestedtext1a"))
			 "nestedtext1"
			 (nested1a "nestedtext1a")
			 "nestedtext1a"
			 "text2"
			 (nested2 "nestedtext2")
			 "nestedtext2")
		   ((descendant (ntype?? '*any*)) nodelist)))

(assert (= '((nested1 "nestedtext1" (nested1a "nestedtext1a")) (nested1a "nestedtext1a") (nested2 "nestedtext2"))
		   ((descendant (ntype?? '*)) nodelist)))

; descendant-or-self

(assert (= '("text1")
		   ((descendant-or-self (ntype?? '*text*)) '(el1 "text1"))))

(assert (= '((el1 "text1") "text1")
		   ((descendant-or-self (ntype?? '*any*)) '(el1 "text1"))))

(assert (= nil
		   ((descendant-or-self (ntype?? '*PI*)) doc-node))) ; special elements ignored

(assert (= '("12398" "3212.12" "Main " "short term savings" " account." "  ")
		   ((descendant-or-self (ntype?? '*data*)) doc-node)))

(assert (= '("text1" "nestedtext1" "nestedtext1a" "text2" "nestedtext2")
		   ((descendant-or-self (ntype?? '*data*)) nodelist)))

				
(assert (= '((el1 {a "a"} "text1" (nested1 "nestedtext1" (nested1a "nestedtext1a")))
			 "text1" 
			 (nested1 "nestedtext1" (nested1a "nestedtext1a"))
			 "nestedtext1"
			 (nested1a "nestedtext1a")
			 "nestedtext1a"
			 (el2 "text2" (nested2 "nestedtext2"))
			 "text2"
			 (nested2 "nestedtext2")
			 "nestedtext2")
		   ((descendant-or-self (ntype?? '*any*)) nodelist)))

(assert (= '((el1 {a "a"} "text1" (nested1 "nestedtext1" (nested1a "nestedtext1a")))
			 (nested1 "nestedtext1" (nested1a "nestedtext1a"))
			 (nested1a "nestedtext1a")
			 (el2 "text2" (nested2 "nestedtext2"))
			 (nested2 "nestedtext2"))
		   ((descendant-or-self (ntype?? '*)) nodelist)))

;; expand-ns-prefixes
(assert (= '(["http://etc" el] el (*not* ["http://etc" el] el (el ["http://etc" el])) el ["http://etc" b])
           (expand-ns-prefixes '(h/el el (*not* h/el el (el h/el)) el h/b) {"h" "http://etc"})))



;; cxpath tests

(assert (= (list account-el)
		   ((cxpath '(account)) doc-node)))

(assert (= (list account-el-ns)
		 ((cxpath '(["http://some.bank.com/ns" account])) doc-node-ns)))

; same thing with namespace map applying default namespace on the account element
(assert (= (list account-el-ns)
           ((cxpath '(account) ns-uris) doc-node-ns)))

(assert (= '((ownerid "12398"))
		   ((cxpath '(account ownerid)) doc-node)))

(assert (= '((["http://some.bank.com/ns" ownerid] "12398"))
		   ((cxpath '(account ownerid) ns-uris) doc-node-ns)))

(assert (= '((ownerid "12398") (balance {currency "USD"} "3212.12"))
		   ((cxpath '(account (*or* ownerid balance))) doc-node)))

(assert (= '((["http://some.bank.com/ns" ownerid] "12398") (["http://some.bank.com/ns" balance] {["http://standards.org/banking" currency] "USD"} "3212.12"))
		   ((cxpath '(account (*or* ownerid balance)) ns-uris) doc-node-ns)))

(assert (= (with-xmlns ns-uris '((ownerid "12398")  (balance {sb/currency "USD"} "3212.12")))
		   ((cxpath '(account (*or* ownerid balance)) ns-uris) doc-node-ns)))


(assert (= '({title "Savings 1" created "5/5/2008"}
			 (descr-html "Main " (b "short term savings") " account.")
			 (report-separator "  ")
			 (*PI* "myapp" "another processing instruction"))
   		   ((cxpath '(account (*not* ownerid balance))) doc-node)))

; TODO: better syntax for this?  Syntax quote seems to garble things too much by inserting namespaces on symbols: how to handle this?
(assert (= '((descr-html "Main " (b "short term savings") " account.") (report-separator "  "))
           ((cxpath (concat '(account (*not* ownerid balance)) (list (filter-nodes element?)))) doc-node)))

(assert (= '("12398")
		   ((cxpath '(account ownerid *text*)) doc-node)))

(assert (= '("Savings 1")
           ((cxpath '(account *at* title *text*)) doc-node)))

(assert (= (list account-el)
           ((cxpath '(account *at* title *par*)) doc-node)))

(assert (= (list '[title "Savings 1"] 
                 account-el
                 doc-node)
           ((cxpath '(account *at* title *anc*)) doc-node)))

(assert (= '([currency "USD"] [title "Savings 1"] [created "5/5/2008"])
           ((cxpath '(account balance *at* currency *anc* *at* *)) doc-node)))


; Syntax within a subpath is different from that in the top level. Subpaths have the special form: '(symbol-or-path path*). 
; The initial symbol-or-path determines the type of nodes returned, whereas the subsequent path* entries are full cxpath paths and are only
; applied as filters (without projection) to the nodes returned by the first symbol-or-path entry.
(assert (= '("12398")
           ((cxpath '((* (balance *at* currency (= "USD"))) ownerid *text*)) doc-node)))

(assert (= '((b  "short term savings"))
		   ((cxpath '((* (balance (= "3212.12")) (ownerid (= "12398")) ) descr-html b)) doc-node)))


; TODO: again, how to use syntax quote here?
(assert (= (list account-el)
           ((cxpath (list '* '(*at* (title (= "Savings 1"))) 'title ((parent identity) doc-node))) doc-node)))
(assert (= '((ownerid "12398"))
           ((cxpath '((* (*at* title (= "Savings 1")) (balance)) ownerid)) doc-node)))

; Select the ownerid text of some element having a balance sub-element with particular text (lookup owner by balance).
(assert (= '((ownerid "12398"))
           ((cxpath '((* (balance (= "3212.12"))) ownerid)) doc-node)))

; Retrieve all attribute collections in the document
(assert (= '({title "Savings 1" created "5/5/2008"} {currency "USD"})
           ((cxpath '(.. *at*)) doc-node)))

; Attribute/value pair vectors are treated as subelements of an attributes collection once that axis is entered.
; Retrieve all attribute values in the document
(assert (= '("Savings 1" "5/5/2008" "USD")
           ((cxpath '(.. *at* * *text*)) doc-node))) ; notice the * standing for the [attribute value] map entries

; The next example illustrates using a custom converter as a filter and the fact that attribute collections are themselves nodes (selected via *at*) in cxml.
; Select all the attribute values from elements having at least 2 attributes
(assert (= '("Savings 1" "5/5/2008")
        ((cxpath (list '.. '*at* (filter-nodes #(>= (count %) 2)) '* '*text*)) doc-node)))


; (load-file "unit-tests.clj") (in-ns 'cxpath)

