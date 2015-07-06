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
            ReferenceNode StrikeNode AnchorLinkNode]
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
  (to-clj' [node opts]))

(defn to-clj
  ([node] (to-clj' node {}))
  ([node opts] (to-clj' node opts)))

(defn clj-contents [node opts]
  (doall (flatten (map #(to-clj' % opts) (seq (.getChildren node))))))

(extend-type SuperNode AstToClj
  (to-clj' [node opts] (clj-contents node opts)))

(extend-type RootNode AstToClj
  (to-clj' [node opts]
    (if (bound? #'*references*)
      (clj-contents node opts)
      (binding [*references*
                (into {}
                      (for [ref (.getReferences node)]
                        [(first (clj-contents ref opts)) ref]))]
        (clj-contents node opts)))))

(extend-type BulletListNode AstToClj
  (to-clj' [node opts] {:tag :ul
                  :content (clj-contents node opts)}))

(extend-type ListItemNode AstToClj
  (to-clj' [node opts] {:tag :li :content (clj-contents node opts)}))

(extend-type TextNode AstToClj
  ;; html-snippet converts entities back into text
  (to-clj' [node opts] (first (html/html-snippet (.getText node)))))

(extend-type AutoLinkNode AstToClj
  (to-clj' [node opts]
    {:tag :a
     :attrs (a-attrs {:href (.getText node)})
     :content (list (.getText node))}))

(extend-type BlockQuoteNode AstToClj
  (to-clj' [node opts]
    {:tag :blockquote
     :content (clj-contents node opts)}))

(extend-type CodeNode AstToClj
  (to-clj' [node opts]
    {:tag :code
     :content (list (.getText node))}))

(extend-type ExpImageNode AstToClj
  (to-clj' [node opts]
    {:tag :img
     :attrs (a-attrs
              {:src (.url node)
               :title (.title node)
               :alt (str/join (clj-contents node opts))})}))

(extend-type ExpLinkNode AstToClj
  (to-clj' [node opts]
    {:tag :a
     :attrs (a-attrs {:href (.url node) :title (.title node)})
     :content (clj-contents node opts)}))

(extend-type HeaderNode AstToClj
  (to-clj' [node opts]
    {:tag (keyword (str "h" (.getLevel node)))
     :content (clj-contents node opts)}))

(extend-type HtmlBlockNode AstToClj
  (to-clj' [node opts]
    (html/html-snippet (.getText node))))

(extend-type InlineHtmlNode AstToClj
  (to-clj' [node opts]
    (html/html-snippet (.getText node))))


(extend-type MailLinkNode AstToClj
  (to-clj' [node opts] {:tag :a
                  :attrs (a-attrs {:href (str "mailto:" (.getText node))})
                  :content (clj-contents node opts)}))

(extend-type OrderedListNode AstToClj
  (to-clj' [node opts]
    {:tag :ol
     :content (clj-contents node opts)}))

(extend-type ParaNode AstToClj
  (to-clj' [node opts]
    {:tag :p
     :content (clj-contents node opts)}))

(def qts
  {QuotedNode$Type/DoubleAngle [\u00AB \u00BB]
   QuotedNode$Type/Double [\u201C \u201D]
   QuotedNode$Type/Single [\u2018 \u2019]})

(extend-type QuotedNode AstToClj
  (to-clj' [node opts]
    {:tag :p
     :content (flatten
                (let [q (qts (.getType node))]
                  (list (q 0) (clj-contents node opts) (q 1))))}))

(def simple-nodes
  {SimpleNode$Type/Apostrophe \'
   SimpleNode$Type/Ellipsis \u2026
   SimpleNode$Type/Emdash \u2014
   SimpleNode$Type/Endash \u2013
   SimpleNode$Type/HRule {:tag :hr}
   SimpleNode$Type/Linebreak {:tag :br}
   SimpleNode$Type/Nbsp \u00A0})

(extend-type SimpleNode AstToClj
  (to-clj' [node opts] (simple-nodes (.getType node))))

(extend-type SpecialTextNode AstToClj
  (to-clj' [node opts] (.getText node)))

(extend-type StrongEmphSuperNode AstToClj
  (to-clj' [node opts]
    {:tag (if (.isStrong node) :strong :em)
     :content (clj-contents node opts)}))

(extend-type StrikeNode AstToClj
  (to-clj' [node opts]
    {:tag :del
     :content (clj-contents node opts)}))

(extend-type AnchorLinkNode AstToClj
  (to-clj' [node opts]
    {:tag :a
     :attrs {:name (.getName node)
             :href (str "#" (.getName node))}
     :content (list (.getText node))}))

(extend-type VerbatimNode AstToClj
  (to-clj' [node opts]
    {:tag :pre
     :content (list (merge {:tag :code
                            :content (list (.getText node))}
                           (when-let [c (.getType node)]
                             (if-not (or (str/blank? c)
                                         (nil? c))
                               {:attrs {:class c}}))))}))

(extend-type RefLinkNode AstToClj
  (to-clj' [node opts]
    (let [contents (clj-contents node opts)
          key (if-let [nd (.referenceKey node)]
                (first (to-clj' nd opts)) (apply str contents))]
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
  (to-clj' [node opts]
    ""))

(defn html-string [clj-md]
  (str/join (html/emit* clj-md)))

(defn to-html
  ([parsed] (to-html parsed {}))
  ([parsed opts]
   (html-string
     {:tag :html
      :content
      (list
        {:tag :head :content
         (list {:tag :meta :attrs {:charset "utf-8"}})}
        {:tag :body
         :content (to-clj' parsed opts)})})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [file]
  (println (to-html (mp (slurp file)))))
