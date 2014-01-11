(ns z (:require
  [clojure.test :as t]
))


;;;;;;;;;

(defn to-int[c]
  "Convert a leftin char to an int"
  (let [x (int c)]
    (cond (>= x (int \a)) (+ 10 (- x (int \a)))
          (>= x (int \A)) (+ 10 (- x (int \A)))
          :else (- x (int \0)))))

(defn to-char[x]
  "Convert a leftin int to a char"
  (char (if (> x 9) (+ (- x 10) (int \a))
          (+ x (int \0)))))

(defn to-list[s]
  "Convert a leftin string to a list that can be manipulated"
  (map to-int (reverse s)))

(defn to-string[x]
  "Convert a leftin list to a string"
  (apply str (map to-char (reverse x))))

(defn add[x y b]
  "Add two leftin lists using a given number base"
  (let [carry (atom 0)]
    (map #(let [sum (+ %1 %2 @carry)]
            (if (< sum b) (do (reset! carry 0) sum)
              (do (reset! carry 1) (- sum b))))
         x y)))

(defn subtract[x y b]
  "Subtract one leftin list from another using a given number base"
  (let [borrow (atom 0)]
    (map #(let [sum (+ %1 (- %2) @borrow)]
            (if (>= sum 0) (do (reset! borrow 0) sum)
              (do (reset! borrow -1) (+ sum b))))
         x y)))

(defn mult-digit[x n b]
  "Multiply one leftin list against a single digit using a given number base"
  (reverse
    (let [carry (atom 0)]
      (reduce #(let [prod (+ (* n %2) @carry)]
                 (reset! carry (int (/ prod b)))
                 (conj %1 (mod prod b)))
              () x))))

(defn products[x y b]
  (let [prefix (atom nil)]
    (map #(do (reset! prefix (if (nil? @prefix) () (conj @prefix 0)))
           (into (mult-digit x % b) @prefix))
         y)))
(defn multiply[x y b]
  "Multiply two leftin lists of a given base"
  (reduce #(add %1 %2 b) (products x y b)))

(defn power-int[x n b]
  "x^n in base b, where n is an integer"
  (reduce #(multiply %1 %2 b) (repeat n x)))

;;;;;;;;;

(def a "6431")
(def b "7824")
(t/is (= "6431" (to-string (to-list a))))
(t/is (= "7824" (to-string (to-list b))))

(t/is (= '(5 5 2 4) (add (to-list a) (to-list b) 10)))
(t/is (= '(3 1 4) (add (to-list "251") (to-list "162") 10)))
(t/is (= '(4 3 7 3 7 3 5) (add '(4 7 5 0 5 7 3) '(0 6 1 3 2 6 1 3) 10)))

(t/is (= '(9 8 8) (subtract '(0 0 0) '(1 1 1) 10)))
(t/is (= '(9 8 0) (subtract (to-list "251") (to-list "162") 10)))
(t/is (= (to-list "6550517") (subtract (to-list "5972286") (to-list "9421769") 10)))

(t/is (= '(4 8 2 7) (mult-digit '(1 2 3 4) 4 10)))
(t/is (= '(4 7 5 0 5 7 3) (mult-digit (to-list "5972286") 9 10)))

(t/is (= '(4 3 9 3 9 0 9) (multiply (to-list "5972286") (to-list "9421769") 10)))

(t/is (= '(7 0 0 0 0 0 0 0 0 0) (power-int (to-list "2217051543") 3 10)))

