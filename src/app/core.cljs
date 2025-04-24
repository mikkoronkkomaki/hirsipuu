(ns app.core
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [clojure.string :as str]))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(def words ["veini" "alvar" "verneri"])

(defn all-letters []
  (map str "abcdefghijklmnopqrstuvwxyzåäö"))

(defn normalize [s]
  (str/lower-case s))

(defn mistakes [words guesses]
  (let [all-letters (->> words (mapcat seq) (map str) set)]
    (count (remove all-letters guesses))))

(defn game-over? [words guesses max-mistakes]
  (or (>= (mistakes words guesses) max-mistakes)
      (every? #(guesses (str %)) (remove #{\space} (apply str words)))))

(defn win? [words guesses]
  (every? #(guesses (str %)) (remove #{\space} (apply str words))))

;; Luo ruudukon, johon sanat sijoitettu ristiin
(defn crossword-grid [guesses]
  (let [grid       (vec (repeat 8 (vec (repeat 10 " "))))
        ;; Sanat normalisoituna
        firstname  (normalize "veini")
        secondname (normalize "alvar")
        thirdname  (normalize "verneri")
        ;; Aseta sanat ruudukkoon
        grid       (reduce (fn [g [i c]]
                             (assoc-in g [2 (+ 3 i)] (if (guesses (str c)) (str c) "-")))
                           grid
                           (map-indexed vector firstname))
        grid       (reduce (fn [g [i c]]
                             (assoc-in g [6 i] (if (guesses (str c)) (str c) "-")))
                           grid
                           (map-indexed vector secondname))
        grid       (reduce (fn [g [i c]]
                             (assoc-in g [(+ 1 i) 4] (if (guesses (str c)) (str c) "-")))
                           grid
                           (map-indexed vector thirdname))]
    grid))

(defui app []
       (let [[guesses set-guesses!] (uix.core/use-state #{})
             max-mistakes     6
             normalized-words (map normalize words)
             incorrect        (mistakes normalized-words guesses)
             over?            (game-over? normalized-words guesses max-mistakes)
             win              (win? normalized-words guesses)
             grid             (crossword-grid guesses)]

         ($ :div {:style {:font-family      "monospace"
                          :padding          "2em"
                          :background-color "#e0f7fa"
                          :min-height       "100vh"}}

            ($ :h1 {:style {:color "#0277bd"}} "Hirsipuupeli – Nimi ristikkona")

            ;; Ruuturuudukko
            ($ :div
               (for [[y row] (map-indexed vector grid)]
                 ($ :div {:key y :style {:height "2em"}}
                    (for [[x cell] (map-indexed vector row)]
                      ($ :div {:key   (str y "-" x)
                               :style {:display          "inline-block"
                                       :width            "2em"
                                       :height           "2em"
                                       :text-align       "center"
                                       :vertical-align   "middle"
                                       :line-height      "2em"
                                       :font-weight      "bold"
                                       :border-right     (when (not= cell " ") "1px solid #0288d1")
                                       :border-bottom    (when (not= cell " ") "1px solid #0288d1")
                                       :border-left      (when (and (not= cell " ") (= x 0)) "1px solid #0288d1")
                                       :border-top       (when (and (not= cell " ") (= y 0)) "1px solid #0288d1")
                                       :background-color (when (not= cell " ") "#b3e5fc")
                                       :border-radius    "4px"}} ;; pehmeämpi ulkoasu
                         cell)))))

            ($ :p {:style {:color      "#01579b"
                           :margin-top "1em"
                           :font-size  "1.1em"}}
               (str "Virheiden määrä: " incorrect " / " max-mistakes))

            (if-not over?
              ;; Kirjainnäppäimet
              ($ :div {:style {:margin "1em 0"}}
                 (for [c (all-letters)]
                   ($ :button {:key      c
                               :style    {:margin           "0.2em"
                                          :padding          "0.5em"
                                          :background-color (if (guesses c) "#b0bec5" "#4fc3f7")
                                          :color            "white"
                                          :border           "none"
                                          :border-radius    "4px"
                                          :cursor           (if (guesses c) "not-allowed" "pointer")}
                               :disabled (guesses c)
                               :on-click #(set-guesses! conj c)}
                      c)))
              ;; Pelin lopetus
              ($ :div {:style {:margin-top "1em"}}
                 ($ :h2 {:style {:color (if win "#2e7d32" "#c62828")}}
                    (if win "Voitit!" "Hävisit!"))
                 ($ :p (str "Sanat olivat: " (str/join ", " words)))
                 ($ :button {:on-click #(set-guesses! #{})
                             :style    {:margin-top       "0.5em"
                                        :padding          "0.5em 1em"
                                        :background-color "#0288d1"
                                        :color            "white"
                                        :border           "none"
                                        :border-radius    "4px"
                                        :cursor           "pointer"}}
                    "Uusi peli"))))))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))