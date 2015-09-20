(ns endophile.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [endophile.utils :refer :all])
  (:import [org.pegdown.ast
            RootNode BulletListNode ListItemNode SuperNode TextNode RefLinkNode
            AutoLinkNode BlockQuoteNode CodeNode TextNode ExpImageNode
            ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode MailLinkNode
            OrderedListNode ParaNode QuotedNode QuotedNode$Type SimpleNode
            SimpleNode$Type SpecialTextNode StrongEmphSuperNode VerbatimNode
            ReferenceNode StrikeNode]
           [org.pegdown PegDownProcessor Extensions]))

;; See https://github.com/sirthias/pegdown/blob/master/src/main/java/org/pegdown/Extensions.java
;; for descriptions
(def extensions
  {:smarts               Extensions/SMARTS
   :quotes               Extensions/QUOTES
   :smartypants          Extensions/SMARTYPANTS
   :abbreviations        Extensions/ABBREVIATIONS
   :hardwraps            Extensions/HARDWRAPS
   :autolinks            Extensions/AUTOLINKS
   :tables               Extensions/TABLES
   :definitions          Extensions/DEFINITIONS
   :fenced-code-blocks   Extensions/FENCED_CODE_BLOCKS
   :wikilinks            Extensions/WIKILINKS
   :strikethrough        Extensions/STRIKETHROUGH
   :anchorlinks          Extensions/ANCHORLINKS
   :all                  Extensions/ALL
   :suppress-html-blocks Extensions/SUPPRESS_HTML_BLOCKS
   :supress-all-html     Extensions/SUPPRESS_ALL_HTML})

(defn- bit-or'
  "Bit-or which works if only one argument is given."
  [& xs]
  (if (seq (rest xs))
    (apply bit-or xs)
    (first xs)))

(defn extensions-map->int [opts]
  (->> opts
       (merge {:autolinks true
               :strikethrough true
               :fenced-code-blocks true})
       (filter val)
       keys
       (map extensions)
       (apply bit-or')
       int))

(defn mp
  "Parses given markdown.

   Second (optional) parameter is options map.

   Available options:
   - :extensions - Map of extensions to enable or disable. Check
     endophile.core/extensions for available extensions."
  ([md] (mp md {}))
  ([md opts]
   (.parseMarkdown
     (PegDownProcessor. (extensions-map->int (:extensions opts)))
     (char-array md))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods return clojure representation of HTML nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare ^:dynamic *references*)

(defprotocol AstToClj
  (to-clj [node]))

(defn- inline-html? [x]
  (instance? InlineHtmlNode x))

(defn end-tag [k]
  (str "</" (name k) ">"))

(defrecord InlineHtmlWrap [start-tag start children])

(defn add [this el]
  (InlineHtmlWrap. (.start-tag this) (.start this) (cons el (.children this))))

(defn output-tag [this]
  (InlineHtmlWrap. (.start-tag this) (.start this) (reverse (.children this))))

(defn wrap-inline-html [children]
  (loop [children children
         out (list)
         tag nil]
    (if-let [f (first children)]
      (if (inline-html? f)
        (let [start (first (html/html-snippet (.getText f)))]
          (if tag
            (if (= (end-tag (.start-tag tag)) (.getText f))
              ; Close current tag
              (recur (rest children) (cons (output-tag tag) out) nil)
              ; Append to current tag
              (recur (rest children) out (add tag f)))
            (if start
              ; Start new tag
              (recur (rest children) out (InlineHtmlWrap. (:tag start) f (list)))
              ; Append to output as inline html couldn't be parsed
              (recur (rest children) (cons f out) tag))))
        (if tag
          (recur (rest children) out (add tag f))
          (recur (rest children) (cons f out) tag)))
      (if tag
        (reverse (cons (output-tag tag) out))
        (reverse out)))))

(defn clj-contents [node]
  (->> node
       (.getChildren)
       seq
       wrap-inline-html
       (map to-clj)
       flatten
       doall))

(extend-type SuperNode AstToClj
  (to-clj [node] (clj-contents node)))

(extend-type RootNode AstToClj
  (to-clj [node]
    (if (bound? #'*references*)
      (clj-contents node)
      (binding [*references*
                (into {}
                      (for [ref (.getReferences node)]
                        [(first (clj-contents ref)) ref]))]
        (clj-contents node)))))

(extend-type BulletListNode AstToClj
  (to-clj [node] {:tag :ul
                  :content (clj-contents node)}))

(extend-type ListItemNode AstToClj
  (to-clj [node] {:tag :li :content (clj-contents node)}))

(extend-type TextNode AstToClj
  ;; html-snippet converts entities back into text
  (to-clj [node] (first (html/html-snippet (.getText node)))))

(extend-type AutoLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs (a-attrs {:href (.getText node)})
                  :content (list (.getText node))}))

(extend-type BlockQuoteNode AstToClj
  (to-clj [node] {:tag :blockquote
                  :content (clj-contents node)}))

(extend-type CodeNode AstToClj
  (to-clj [node] {:tag :code
                  :content (list (.getText node))}))

(extend-type ExpImageNode AstToClj
  (to-clj [node] {:tag :img
                  :attrs (a-attrs
                          {:src (.url node)
                           :title (.title node)
                           :alt (str/join (clj-contents node))})}))

(extend-type ExpLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs (a-attrs {:href (.url node) :title (.title node)})
                  :content (clj-contents node)}))

(extend-type HeaderNode AstToClj
  (to-clj [node] {:tag (keyword (str "h" (.getLevel node)))
                  :content (clj-contents node)}))


(extend-type HtmlBlockNode AstToClj
  (to-clj [node]
    (html/html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToClj
  (to-clj [node] (html/html-snippet (.getText node))))

(extend-type InlineHtmlWrap AstToClj
  (to-clj [node] (let [tag (first (html/html-snippet (.getText (.start node))))]
                   (assoc tag :content (clj-contents (SuperNode. (.children node)))))))


(extend-type MailLinkNode AstToClj
  (to-clj [node] {:tag :a
                  :attrs (a-attrs {:href (str "mailto:" (.getText node))})
                  :content (clj-contents node)}))

(extend-type OrderedListNode AstToClj
  (to-clj [node] {:tag :ol
                  :content (clj-contents node)}))

(extend-type ParaNode AstToClj
  (to-clj [node] {:tag :p
                  :content (clj-contents node)}))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToClj
  (to-clj [node] {:tag :p
                  :content (flatten
                            (let [q (qts (.getType node))]
                              (list (q 0) (clj-contents node) (q 1))))}))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule {:tag :hr}
   SimpleNode$Type/Linebreak {:tag :br}
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToClj
  (to-clj [node] (simple-nodes (.getType node))))


(extend-type SpecialTextNode AstToClj
  (to-clj [node] (.getText node)))

(extend-type StrongEmphSuperNode AstToClj
  (to-clj [node] {:tag (if (.isStrong node) :strong :em)
                  :content (clj-contents node)}))

(extend-type StrikeNode AstToClj
  (to-clj [node]
    {:tag :del
     :content (clj-contents node)}))

(extend-type VerbatimNode AstToClj
  (to-clj [node]
    {:tag :pre
     :content (list (merge {:tag :code
                            :content (list (.getText node))}
                           (when-let [c (.getType node)]
                             (if-not (or (str/blank? c)
                                         (nil? c))
                               {:attrs {:class c}}))))}))

(extend-type RefLinkNode AstToClj
  (to-clj [node]
    (let [contents (clj-contents node)
          key (if-let [nd (.referenceKey node)]
                (first (to-clj nd)) (apply str contents))]
     (if-let [ref (*references* key)]
       {:tag :a :attrs (a-attrs {:href (.getUrl ref) :title (.getTitle ref)})
        :content contents}
       (cons "[" (concat contents
                         (if (.separatorSpace node)
                             [(str "]"
                                   (.separatorSpace node)
                                   "[" (.referenceKey node) "]")]
                             ["]"])))))))

(extend-type ReferenceNode AstToClj
  (to-clj [node]
    ""))

(defn html-string [clj-md]
  (str/join (html/emit* clj-md)))

(defn to-html [parsed]
  (html-string
   {:tag :html
      :content
      (list
       {:tag :head :content
        (list {:tag :meta :attrs {:charset "utf-8"}})}
       {:tag :body
        :content (to-clj parsed)})}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [file]
  (println (to-html (mp (slurp file)))))
