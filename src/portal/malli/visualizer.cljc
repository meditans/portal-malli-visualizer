(ns portal.malli.visualizer
  (:require [malli.core :as m]
            [malli.error :as me]))

;; * Utilities for setting portal metadata

(defn portal-hiccup [x]
  (with-meta x {:portal.viewer/default :portal.viewer/hiccup}))

(defn portal-pprint [x]
  (with-meta x {:portal.viewer/default :portal.viewer/pprint}))

;; * A multimethod to tap hiccup when instrumenting

(defn format-error [{:keys [value message]}]
  (let [quoted-value (if (string? value) (str "\"" value "\"") value)]
    [:li
     {:style {:list-style-type :none}}
     (str quoted-value " " message)]))

(defmulti display-malli-error (fn [type _] type))

(defmethod display-malli-error ::m/invalid-input
  [_ {original-args :args
      input-schema :input
      original-fn-meta :original-fn-meta}]
  (let [reason (m/explain input-schema original-args)
        errors (:errors (me/with-error-messages reason))
        fn-name (:name original-fn-meta)]
    (tap>
     (portal-hiccup
      [:div {:style {:padding-left "20px" :border-left "4px dotted"}}
       [:h2 (str "Input error: " fn-name)]
       [:p "When calling" [:br]
        [:code {:style {:color :#ebcb8b}} (str (cons fn-name original-args))]]
       [:p "with input spec" [:br]
        [:code {:style {:color :#ebcb8b}} (str (m/form input-schema))]]
       [:p "we have that" [:br]
        [:code {:style {:color :#bf616a}} (map format-error errors)]]]))))

(defmethod display-malli-error ::m/invalid-output
  [_ {original-args :args
      output-schema :output
      result :value
      original-fn-meta :original-fn-meta}]
  (let [reason (m/explain output-schema result)
        errors (:errors (me/with-error-messages reason))
        fn-name (:name original-fn-meta)]
    (tap>
     (portal-hiccup
      [:div {:style {:padding-left "20px" :border-left "4px dotted"}}
       [:h2 (str "Output error: " fn-name)]
       [:p "When evaluating" [:br]
        [:code {:style {:color :#ebcb8b}}
         (str (cons fn-name original-args) " => " result)]]
       [:p "with output spec" [:br]
        [:code {:style {:color :#ebcb8b}} (str (m/form output-schema))]]
       [:p "we have that" [:br]
        [:code {:style {:color :#bf616a}} (map format-error errors)]]]))))

(defmethod display-malli-error :default
  [type data]
  (tap> "DEFAULT CASE IN display-malli-error")
  (tap> type)
  (tap> data))

;; * Setting the instrumentation to use our functions

;; (comment
;;   (dev/start!
;;    {:report display-malli-error})
;;   dev/stop!)

;; Testing
;; (m/=> -add [:=> [:cat :int :int] [:map-of :symbol :int]])
;; (defn -add [x y] {:a :z})

;; (-> (m/explain
;;      [:and
;;       [:tuple :int :int]
;;       [:fn (fn [[a b]] (< 0 a b))]]
;;      [5 3])
;;     me/with-error-messages
;;     portal-pprint
;;     tap>)

;; (-add 2 "3")

;; * Checking
;; ** Hiccup visualizers
(defn create-pretty-check-message [check-error]
  (let [function-name (first check-error)
        first-error (-> check-error second :errors first)
        problematic-input (-> first-error :check :smallest first)
        error (-> first-error :check :malli.generator/explain-output)
        result (-> error :value)]
    (portal-hiccup
     [:div {:style {:padding-left "20px"
                    :border-left "4px dotted"}}
      [:p "When calling" [:br]
       [:code {:style {:color :#ebcb8b}} (str (cons function-name problematic-input))] [:br]
       "we get" [:br]
       [:code {:style {:color :#ebcb8b}} (str result)]]
      [:p
       "The problem is that" [:br]
       [:code {:style {:color :#bf616a}} (str (me/humanize error))]]])))

(defn create-pretty-check-messages [check-errors]
  (portal-hiccup
   [:div
    [:h3 "Generative errors"]
    (map create-pretty-check-message check-errors)]))

;; ** Emacs integration function
(defn check->portal
  "This is the function that has to be called from my emacs code"
  [check-result]
  (->> check-result
       create-pretty-check-messages
       tap>))

;; ** Testing
;; (comment
;;   (mi/check))
