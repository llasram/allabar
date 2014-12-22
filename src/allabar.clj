(ns allabar
  (:refer-clojure :exclude [== !=])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.logic :refer :all]
            [clojure.tools.macro :refer [symbol-macrolet]]))

(defne heado
  [x l]
  ([_ _] (emptyo x))
  ([[h . xr] [h . lr]] (heado xr lr)))

(defne ordero
  [x l]
  ([_ _] (heado x l))
  ([_ [_ . r]] (ordero x r)))

(defn ritualo
  [ds]
  (symbol-macrolet [_ (lvar)]
    (all
     ;; Firste, ye penitent mufte draw upon thy floore a many-pointed ftarr.
     ;; Thy ftar muste then be inscybed withe a number of shappes, each drawne
     ;; wythe a correcte substancs, and in thy correcte ordar, and wyth thy
     ;; correcte worde of power uttered upon completionne, and wyth thy correcte
     ;; inscryptionne.

     ;; The shape inscryped in blood is two shappes within a tryangle.
     (ordero [[_ 'blood _] _ ['triangle _ _]] ds)

     ;; The outermoste shape isn't a pentagram.
     (membero ['pentagram _ _] ds)
     (fresh [s]
       (!= s 'pentagram)
       (== [_ _ _ _ [s _ _]] ds))

     ;; Of the shape inscrybed in blood and the shappe whose worde is
     ;; Ch'rulthin, one is a circle and the other is within two shappes and
     ;; without two [corrected to "without three"] shapes.  [Agreed to mean the
     ;; third shape.]
     (fresh [s]
       (!= s 'circle)
       (conde
        [(membero ['circle 'blood _] ds)
         (== [_ _ [s _ 'chrulthin] _ _] ds)]
        [(membero ['circle _ 'chrulthin] ds)
         (== [_ _ [s 'blood _] _ _] ds)]))

     ;; Neither the shape inscrybbed in chalk nor the thirde shappe from the
     ;; many-pointed star should have the word Wyrin spoken over them.
     (membero [_ _ 'wyrin] ds)
     (fresh [w]
       (!= w 'wyrin)
       (membero [_ 'chalk w] ds))
     (fresh [w]
       (!= w 'wyrin)
       (== [_ _ [_ _ w] _ _] ds))
     ;; [exclusive]
     (fresh [m]
       (!= m 'chalk)
       (== [_ _ [m _ _] _ _] ds))

     ;; Thou shouldst notte speaketh the words Shub'rignuth over the shape
     ;; inscrybbed in hair.
     (membero [_ 'hair _] ds)
     (fresh [m]
       (!= m 'hair)
       (membero [_ m 'shubrignuth] ds))

     ;; The shape inscrybbed in chalk should have either the word "Ch'rulthin"
     ;; fspokenne or is a squarre.
     (conde
      [(fresh [s]
         (!= s 'square)
         (membero [s 'chalk 'chrulthin] ds))]
      [(fresh [w]
         (!= w 'chrulthin)
         (membero ['square 'chalk w] ds))])

     ;; One of the shapes is ye pentacle.
     (membero ['pentacle _ _] ds)

     ;; Thy shape whose worde ys Rug'nuroth is the innermost, aside from the
     ;; starr.
     (firsto ds [_ _ 'rugnaroth])

     ;; The shape inscrybbe in hair isn't a tri-angle.
     (membero ['triangle _ _] ds)
     (fresh [s]
       (!= s 'triangle)
       (membero [s 'hair _] ds))

     ;; The second shappe out frome the star is notte inscrybbed in ink.
     (membero [_ 'ink _] ds)
     (fresh [m]
       (!= m 'ink)
       (== [_ [_ m _] _ _ _] ds))

     ;; Thy shouldst speake the words Shub'rignuth shape [sic] two shappes
     ;; within the worde Blugh.
     (ordero [[_ _ 'shubrignuth] _ [_ _ 'blugh]] ds)

     ;; The shappe inscrybbed in chalke is two shappes out from the shappe whose
     ;; words are Shub'rignuth.
     (ordero [[_ _ 'shubrignuth] _ [_ 'chalk _]] ds)

     ;; Onne ofeth thy shappes is inscrybbed in ye olde sannd.
     (membero [_ 'sand _] ds)

     ;; I am actually perfectly capable of writing comprehensibly in standard,
     ;; modern Common.  [Thanks, John!]
     )))

(defn solve
  [n]
  (->> (run* [q] (ritualo q))
       (distinct)
       (take n)))

(defn -main
  [& args]
  (pprint (solve 1)))
