(ns leftins.core)

;(. js/console (log "Hello world!"))

(defn c->int[c]
  "char to int, cljs specific"
  (.charCodeAt c))

(defn s->int[s]
  "string to int, cljs specific"
  (js/parseInt s))

(defn to-int[c]
  "Convert a leftin char to an int"
  (let [x (c->int c)]
    (cond (>= x (c->int \a)) (+ 10 (- x (c->int \a)))
          (>= x (c->int \A)) (+ 10 (- x (c->int \A)))
          :else (- x (c->int \0)))))

(defn to-char[x]
  "Convert a leftin int to a char"
  (char (if (> x 9) (+ (- x 10) (c->int \a))
          (+ x (c->int \0)))))

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


;;;;;;;;;;;;;;;;;

(defn stringOp[op x y b]
  (let [xl (to-list x)
        yl (to-list y)]
    (to-string (op xl yl b))))

(defn ^:export addStrings[x y b] (stringOp add x y b))
(defn ^:export subtractStrings[x y b] (stringOp subtract x y b))
(defn ^:export multiplyStrings[x y b] (stringOp multiply x y b))

(defn ^:export powerStrings[x n b]
  (let [xl (to-list x)
        nl (s->int n)]
    (to-string (power-int xl nl b))))
