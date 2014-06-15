(ns z (:require
  [clojure.test :as t]
))


;;;;;;;;;

(defn c->int[c]
  "char to int, clj specific"
  (int c))

(defn s->int[s]
  "string to int, clj specific"
  (Integer/parseInt s))

(defn l-char->int[c]
  "Convert a leftin char to an int"
  (let [x (c->int c)]
    (cond (>= x (c->int \a)) (+ 10 (- x (c->int \a)))
          (>= x (c->int \A)) (+ 10 (- x (c->int \A)))
          :else (- x (c->int \0)))))

(defn l-int->char[x]
  "Convert a leftin int to a char"
  (char (if (> x 9) (+ (- x 10) (c->int \a))
          (+ x (c->int \0)))))

(defn l-string->list[s]
  "Convert a leftin string to a list that can be manipulated"
  (map l-char->int (reverse s)))

(defn l-list->string[x]
  "Convert a leftin list to a string"
  (apply str (map l-int->char (reverse x))))

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


;;;;;;;;;;;;;;;;;

(defn stringOp[op x y b]
  (let [xl (l-string->list x)
        yl (l-string->list y)]
    (l-list->string (op xl yl b))))

(defn ^:export addStrings[x y b] (stringOp add x y b))
(defn ^:export subtractStrings[x y b] (stringOp subtract x y b))
(defn ^:export multiplyStrings[x y b] (stringOp multiply x y b))

(defn ^:export powerStrings[x n b]
  (let [xl (l-string->list x)
        nl (s->int n)]
    (l-list->string (power-int xl nl b))))
;;;;;;;;;

(def a "6431")
(def b "7824")
(t/is (= "6431" (list->string (string->list a))))
(t/is (= "7824" (list->string (string->list b))))

(t/is (= '(5 5 2 4) (add (string->list a) (string->list b) 10)))
(t/is (= '(3 1 4) (add (string->list "251") (string->list "162") 10)))
(t/is (= '(4 3 7 3 7 3 5) (add '(4 7 5 0 5 7 3) '(0 6 1 3 2 6 1 3) 10)))

(t/is (= '(9 8 8) (subtract '(0 0 0) '(1 1 1) 10)))
(t/is (= '(9 8 0) (subtract (string->list "251") (string->list "162") 10)))
(t/is (= (string->list "6550517") (subtract (string->list "5972286") (string->list "9421769") 10)))

(t/is (= '(4 8 2 7) (mult-digit '(1 2 3 4) 4 10)))
(t/is (= '(4 7 5 0 5 7 3) (mult-digit (string->list "5972286") 9 10)))

(t/is (= '(4 3 9 3 9 0 9) (multiply (string->list "5972286") (string->list "9421769") 10)))

(t/is (= '(7 0 0 0 0 0 0 0 0 0) (power-int (string->list "2217051543") 3 10)))

(t/is (= (addStrings "12" "34" 10) "46"))
(t/is (= (subtractStrings "12" "34" 10) "78"))
(t/is (= (multiplyStrings "123" "456" 10) "088"))
(t/is (= (powerStrings "7051543" "3" 10) "0000007"))



;;;;;;;;;;;;;;;;;

(defn chop-l1[l1 l2]
  "Shorten l1 so it's the same length as l2"
  (list* (subvec (vec l1) 0 (count l2))))

(defn add-if-product[x y z b lst]
  "If x * y = z, add it to list of answers"
  (let [z' (multiply x y b)]
    (if (= z' (chop-l1 z z'))
      (conj lst x) lst)))

(defn try-next-digit[x y z b]
  "Try adding digits to x to find cases where x*y=z"
  (loop [i 0
         answer []]
    (if (= i 10) answer
      (recur (inc i) (add-if-product' (concat x `(~i)) y z b answer)))))

(t/is (= [1 2]
         (chop-v1 [1 2 3 4] [9 10])))
(t/is (= [[1 2] [3 4 5]]
         (add-if-product [3 4 5] [4 5 6 7] [2 2 1] 10 [[1 2]])))
(t/is (= [[1 2]]
         (add-if-product [3 4 5] [4 5 6 7] [4 4 3] 10 [[1 2]])))
(t/is (= [[3 4 0] [3 4 5]]
         (try-next-digit [3 4] [4 5 6] [2 2 1] 10)) )

