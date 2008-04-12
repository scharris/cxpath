;
;   Derived from Rich Hickey's xml.clj by Steve Harris.
;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; -------------------------------------------------------------------------------------------
;   An xml parser yielding sxml style output (http://okmij.org/ftp/Scheme/SXML.html).
;   There are a few minor differences with SXML:
;      - Clojure maps are used in place of attribute lists.
;      - Qualified element and attribute tags are represented as pairs [uri sym-or-kwd].
;      - Can output compound nodes as lists or vectors,  use the parse-to-vector function for vector output.
; 
; (load-file "cxml-parser.clj")
; (def x (cxml/parse "test.xml"))
; (def xv (cxml/parse-to-vector "test.xml"))
; (def xws (cxml/parse "test.xml" {:ignore-whitespace-between-elements? false})) ; produces nodes for all whitespace


(in-ns 'cxml)
(clojure/refer 'clojure)

(import '(org.xml.sax ContentHandler Attributes SAXException)
        '(javax.xml.parsers SAXParser SAXParserFactory)
        '(org.xml.sax InputSource))

(def *stack*)
(def *current*)
(def *pending-chars*)
(def *state*)

(def DOCROOT-TAG '*TOP*)
(def PI-TAG '*PI*)
(def COMMENT-TAG '*COMMENT*)
(def ENTITY-TAG '*ENTITY*)


(defn content-handler [opts create-el finalize-el add-to-el]
  (let [add-pending-char-data (fn [add-to-el]
	      (set! *current* (add-to-el *current* (str *pending-chars*)))
		  (set! *pending-chars* nil))

		ns-prefix (fn [#^String qname]
		  (let [ colon-ix (. qname (indexOf (int \:))) ]
			(if (= -1 colon-ix)
			  nil
			  (. qname (substring (int 0) colon-ix)))))

		
		all-whitespace? (fn [chars-array start len] 
		  (loop [i (+ start (dec len))]
			(if (< i start)
			  true
			  (if (not (. Character (isWhitespace (aget chars-array i))))
				false
				(recur (dec i))))))

		string-nonempty? (fn [#^String s] (if s (pos? (. s (length))) false))

		make-attr-tagsym (if (:keyword-attributes? opts) keyword symbol)
		make-el-tagsym (if (:keyword-tags? opts) keyword symbol)]

	 (new clojure.lang.XMLHandler
       (proxy [ContentHandler] []

         (startElement [uri local-name q-name #^Attributes atts]
            (let [add-attr (fn [ats-map i]
							   (let [at-sym (make-attr-tagsym (. atts (getLocalName i)))
									 at-uri (let [u (. atts (getURI i))] (if (string-nonempty? u) u))
									 at-prefix (if at-uri (ns-prefix (. atts (getQName i))))
									 at (if at-uri 
										  (with-meta [at-uri at-sym] {:xmlns-prefix at-prefix})   ; ns-qualified attribute
										  at-sym)                                                 ; unqualified attribute
									 at-val (. atts (getValue i))]
								 (assoc ats-map at at-val)))

				  attrs (reduce add-attr nil (range 0 (. atts (getLength))))

				  tag (let [sym (make-el-tagsym local-name)]
						(if (string-nonempty? uri)
						  (with-meta [uri sym] {:xmlns-prefix (ns-prefix q-name)})
						  sym))
                 
				  new-el (let [e (create-el tag)] 
						   (if attrs 
							 (add-to-el e attrs)
							 e))]
                 
            (when *pending-chars*
               (let [ignore (and (:ignore-whitespace-between-elements? opts)
                                 (or (= *state* :ws-read-after-element-start)
                                     (= *state* :ws-read-after-element-end))) ]
                 (if ignore
                   (set! *pending-chars* nil)
                   (add-pending-char-data add-to-el))))
             (set! *stack* (conj *stack* *current*))
             (set! *current* new-el)
             (set! *state* :element-started))    
           nil)


         (endElement [uri local-name q-name]
           (when *pending-chars*
             (let [ignore (and (:ignore-whitespace-between-elements? opts)
                               (= *state* :ws-read-after-element-end)) ]
               (if ignore
                 (set! *pending-chars* nil)
                 (add-pending-char-data add-to-el))))
           (set! *current* (conj (peek *stack*) (finalize-el *current*)))
           (set! *stack* (pop *stack*))
           (set! *state* :element-ended)
           nil)
         

         (characters [cdata start len]
           (when-not *pending-chars*
             (set! *pending-chars* (new StringBuilder)))
           (let [#^StringBuilder sb *pending-chars*]
             (. sb (append cdata start len))
             (set! *state* 
                   (if (and (:ignore-whitespace-between-elements? opts)
                            (all-whitespace? cdata start len))
                     (cond
                       (or (= *state* :element-started) (= *state* :ws-read-after-element-start)) :ws-read-after-element-start
                       (or (= *state* :element-ended)   (= *state* :ws-read-after-element-end))   :ws-read-after-element-end
                       true :chars-read)
                     :chars-read)))
           nil)

         ; Add the processing instruction to the current element but otherwise don't change state.
         ; Pending text is added or ignored first just as if this were the beginning of a new element.
         (processingInstruction [target data]
           (when *pending-chars*
             (let [ignore (and (:ignore-whitespace-between-elements? opts)
                               (or (= *state* :ws-read-after-element-start)
                                   (= *state* :ws-read-after-element-end))) ]
               (if ignore
                 (set! *pending-chars* nil)
                 (add-pending-char-data add-to-el))))

           (let [ pi (finalize-el (add-to-el (add-to-el (create-el PI-TAG) target) data)) ]
             (set! *current* (add-to-el *current* pi)))
           nil)
         

		 (startPrefixMapping [prefix uri])
		 (endPrefixMapping [prefix])
         (setDocumentLocator [locator])
         (startDocument [])
         (endDocument [])))))
        


(defn parse-impl
  ([s opts create-el finalize-el add-to-el]
   (let [ parser-factory (doto (. SAXParserFactory (newInstance)) (setNamespaceAware true))
		  parser (. parser-factory (newSAXParser)) ]
     (binding [*stack* nil
               *current* (create-el DOCROOT-TAG)
               *state* nil
               *pending-chars* nil]
       (. parser (parse (new InputSource s) (content-handler opts create-el finalize-el add-to-el)))
       (finalize-el *current*)))))


(def default-opts { :ignore-whitespace-between-elements? true
                    :keyword-attributes? false
				    :keyword-tags false } )

(defn parse-to-list
  ([s] (parse-to-list s default-opts))
  ([s opts] 
   (parse-impl s opts list reverse conj)))

(defn parse-to-vector
  ([s] (parse-to-vector s default-opts))
  ([s opts] 
   (parse-impl s opts vector identity conj)))


(def parse parse-to-list)

(comment Parser tests

(def xml-str (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				  "<?myapp a processing instruction?>"
				  "<account title='Savings 1' created='5/5/2008'>" 
				  "<ownerid>12398</ownerid>"
				  "<balance currency=\"USD\">3212.12</balance>"
				  "<descr-html>Main <b>short term savings</b> account.</descr-html>"
				  "<report-separator>  </report-separator>"
				  "<?myapp another processing instruction?>"
				  "</account>"))

(def xml-str-ns "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?myapp a processing instruction?>
<account title=\"Savings 1\" created='5/5/2008' xmlns=\"http://some.bank.com/ns\">
  <ownerid>12398</ownerid>
  <balance xmlns:sb='http://standards.org/banking' sb:currency=\"USD\">3212.12</balance>
  <descr-html xmlns:h=\"http://www.w3.org/HTML/1998/html4\">Main <h:b>short term savings</h:b> account.</descr-html>
  <report-separator>  </report-separator>
  <?myapp another processing instruction?>
</account>")

(def doc1 (cxml/parse-to-list (new java.io.StringReader xml-str)))
(def doc2 (cxml/parse-to-vector (new java.io.StringReader xml-str)))
(def doc3 (cxml/parse-to-list (new java.io.StringReader xml-str-ns)))
(def doc4 (cxml/parse-to-vector (new java.io.StringReader xml-str-ns)))
(def doc5 (cxml/parse-to-vector (new java.io.StringReader xml-str) {:keyword-attributes? true :keyword-tags? true :ignore-whitespace-between-elements? true}))
(def doc6 (cxml/parse-to-vector (new java.io.StringReader xml-str-ns) {:keyword-attributes? true :keyword-tags? true :ignore-whitespace-between-elements? true}))

)

; (load-file "cxml.clj")