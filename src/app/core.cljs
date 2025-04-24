(ns app.core
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [clojure.string :as str]))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(def words ["VEINI" "ALVAR" "VERNERI"])

(defn all-letters []
  (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ"))

(defn normalize [s]
  (str/upper-case s))

(defn mistakes [words guesses]
  (let [all-letters (->> words (mapcat seq) (map str) set)]
    (count (remove all-letters guesses))))

(defn game-over? [words guesses max-mistakes]
  (or (>= (mistakes words guesses) max-mistakes)
      (every? #(guesses (str %)) (remove #{\space} (apply str words)))))

(defn win? [words guesses]
  (every? #(guesses (str %)) (remove #{\space} (apply str words))))

;; Luo ruudukon, johon nimet sijoitettu ristiin
(defn crossword-grid [guesses]
  (let [grid       (vec (repeat 8 (vec (repeat 10 " "))))
        ;; Nimet normalisoituna
        firstname  (normalize "VEINI")
        secondname (normalize "ALVAR")
        thirdname  (normalize "VERNERI")
        ;; Aseta nimet ruudukkoon
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

         ($ :div {:style {:background-image    "url('/background.jpg')"
                          :background-size     "cover"
                          :background-position "bottom right"
                          :background-repeat   "no-repeat"
                          :min-height          "100vh"
                          :padding             "2em"
                          :font-family         "monospace"
                          }}


            ($ :div {:style {:position         "absolute"
                             :top              0 :left 0 :right 0 :bottom 0
                             :background-color "rgba(255,255,255,0.7)"
                             :z-index          1
                             :display             "flex"
                             :flex-direction      "column"
                             :align-items         "center"}}

               ($ :h1 {:style {:color "#0277bd"} :text-align "center" :class "cherry-bomb-one-regular"} "Arvaa kokonimi!")

               ;; Ruuturuudukko
               ($ :div
                  (for [[y row] (map-indexed vector grid)]
                    ($ :div {:key y :style {:height "2em" :margin "0.5px"}}
                       (for [[x cell] (map-indexed vector row)]
                         ($ :div {:key   (str y "-" x)
                                  :style {:display          "inline-block"
                                          :width            "2em"
                                          :height           "2em"
                                          :text-align       "center"
                                          :vertical-align   "middle"
                                          :line-height      "2em"
                                          :font-weight      "bold"
                                          :border-right     (when (not= cell " ") "0.5px solid #0288d1")
                                          :border-bottom    (when (not= cell " ") "0.5px solid #0288d1")
                                          :border-left      (when (and (not= cell " ") (= x 0)) "0.5px solid #0288d1")
                                          :border-top       (when (and (not= cell " ") (= y 0)) "0.5px solid #0288d1")
                                          :background-color (when (not= cell " ") "#ffffff")
                                          :border-radius    "2px"
                                          :margin           "0.5px"}}
                            cell)))))

               ($ :p {:style {:color      "#01579b"
                              :margin-top "1em"
                              :font-size  "1.1em"}
                      :class "cherry-bomb-one-regular"}
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
                    ($ :h2 {:style {:color (if win "#2e7d32" "#c62828")} :class "cherry-bomb-one-regular"}
                       (if win "Voitit!" "Hävisit!"))
                    ($ :p {:class "cherry-bomb-one-regular"} (str "Kokonimi on: " (str/join ", " words)))
                    ($ :button {:on-click #(set-guesses! #{})
                                :style    {:margin-top       "0.5em"
                                           :padding          "0.5em 1em"
                                           :background-color "#0288d1"
                                           :color            "white"
                                           :border           "none"
                                           :border-radius    "4px"
                                           :cursor           "pointer"}}
                       "Uusi peli")))))))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))