(ns app.core
  (:require [uix.core :refer [defui $]]
            [uix.dom]
            [clojure.string :as str]))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(def words ["Veini" "Alvar" "Verneri"])

(defn normalize-word [w]
  (str/lower-case w))

(defn display-word [word guesses]
  (->> word
       (map (fn [c]
              (cond
                (= c \space) " "
                (guesses (str c)) (str c)
                :else "_")))
       (str/join " ")))

(defn all-letters []
  (map str "abcdefghijklmnopqrstuvwxyzåäö"))

(defn mistakes [words guesses]
  (let [all-letters (->> words (mapcat seq) (map str) set)]
    (count (remove all-letters guesses))))

(defn game-over? [words guesses max-mistakes]
  (or (>= (mistakes words guesses) max-mistakes)
      (every? #(guesses (str %)) (remove #{\space} (apply str words)))))

(defn win? [words guesses]
  (every? #(guesses (str %)) (remove #{\space} (apply str words))))

(defui app []
       (let [[guesses set-guesses!] (uix.core/use-state #{})
             max-mistakes     6

             normalized-words (map normalize-word words)
             incorrect        (mistakes normalized-words guesses)
             over?            (game-over? normalized-words guesses max-mistakes)
             win              (win? normalized-words guesses)]

         ($ :div {:style {:font-family "monospace"}}
            ($ :h1 "Nimiarvaus")

            ;; Näytetään kaikki sanat omilla riveillään
            (for [[i word] (map-indexed vector normalized-words)]
              ($ :p {:key i} (display-word word guesses)))

            ($ :p (str "Virheiden määrä: " incorrect " / " max-mistakes))

            (if-not over?
              ;; Kirjainnäppäimet
              ($ :div {:style {:margin "1em 0"}}
                 (for [c (all-letters)]
                   ($ :button {:key      c
                               :style    {:margin "0.2em"}
                               :disabled (guesses c)
                               :on-click #(set-guesses! conj c)}
                      c)))
              ;; Pelin lopetus
              ($ :div
                 ($ :h2 (if win "Voitit!" "Hävisit!"))
                 ($ :p (str "Sanat olivat: " (str/join ", " words)))
                 ($ :button {:on-click #(set-guesses! #{})}
                    "Uusi peli"))))))

(defn render []
  (uix.dom/render-root ($ app) root))

(defn ^:export init []
  (render))