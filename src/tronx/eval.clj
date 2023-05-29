(ns tronx.eval)

(def ret-sym :___tronx__return__1289)

(defn val-or-ret [x]
  (if (instance? clojure.lang.Atom x)
    (@x ret-sym)
    x))

(def env-map
  {'writeln (fn [args env]
              (apply println (map str (map val-or-ret args))))
   'write   (fn [args env]
              (apply print (map str (map val-or-ret args))))
   '+       (fn [args env]
              (apply + (map val-or-ret args)))
   '-       (fn [args env]
              (apply - (map val-or-ret args)))
   '*       (fn [args env]
              (apply * (map val-or-ret args)))
   '/       (fn [args env]
              (apply / (map val-or-ret args)))
   '=       (fn [args env]
              (apply = (map val-or-ret args)))
   '<       (fn [args env]
              (apply < (map val-or-ret args)))
   '>       (fn [args env]
              (apply > (map val-or-ret args)))
   '<=      (fn [args env]
              (apply <= (map val-or-ret args)))
   '>=      (fn [args env]
              (apply >= (map val-or-ret args)))
   'not     (fn [args env]
              (apply not (map val-or-ret args)))})

(declare tronx-eval)
(declare tronx-eval-code)

(def env
  (atom env-map))

(defn new-env []
  (atom env-map))

(defn new-env-from [env]
  (->> @env
       (into {} (map (fn [[k v]] [k v])))
       (atom)))

(defn t-number? [x]
  (number? x))

(defn t-string? [x]
  (string? x))

(defn t-var? [x]
  (symbol? x))

(defn t-lookup [x env]
  (x @env))

(defn t-assignment? [x]
  (and (seq? x)
       (= (count x) 3)
       (= (first x) ':=)
       (symbol? (second x))))

(defn t-assign [code env]
  (swap! env merge {(second code) (tronx-eval
                                    (nth code 2)
                                    env)}))

(defn t-proc-call? [code]
  (and (list? code)
       (symbol? (first code))))

(defn t-proc-invoke [code env]
  (let [proc-name (first code)
        proc-args (map #(tronx-eval % env)
                       (rest code))
        proc-defn (t-lookup proc-name env)]
    (proc-defn proc-args env)))

(defn t-proc-defn? [code]
  (and
    (list? code)
    (= (first code) 'proc)
    (symbol? (second code))
    (list? (nth code 2))
    (every? symbol? (nth code 2))))

(defn t-proc [name proc-args proc-body]
  (fn [args env]
    (let [proc-env @env
          new-env (atom
                    (merge proc-env
                           (into {} (map vector proc-args args))))]
      (tronx-eval-code proc-body new-env))))

(defn t-return? [code]
  (and
    (list? code)
    (= (first code) 'return)))

(defn t-make-return [code env]
  (let [r (second code)
        r-val (tronx-eval r env)]
    (swap! env
           merge {ret-sym r-val})))

(defn t-if? [code]
  (and
    (list? code)
    (= (first code) 'if)
    (>= (count code) 3)))

(defn t-exec-if [code env]
  (let [condition (second code)
        if-true (nth code 2)
        if-false (if (> (count code) 3)
                   (nth code 3)
                   nil)]
    (if (tronx-eval condition env)
      (tronx-eval if-true env)
      (tronx-eval if-false env))))

(defn t-do? [code]
  (and (list? code)
       (= (first code) 'do)))

(defn t-do [code env]
  (tronx-eval-code (rest code) (new-env-from env)))

(defn t-make-proc [code env]
  (let [proc-name (second code)
        proc-args (nth code 2)
        proc-body (rest (rest (rest code)))]
    (swap! env
           merge
           {proc-name (t-proc proc-name proc-args proc-body)})))

(defn t-while? [code]
  (and
    (list? code)
    (= (first code) 'while)))

(defn t-do-while [code env]
  (let [cnd (second code)
        codes (rest (rest code))]
    (loop []
      (when (tronx-eval cnd env)
        (tronx-eval-code codes env)
        (recur)))))

(defn tronx-eval [code env]
  (cond
    (t-number? code) code
    (t-string? code) code
    (t-var? code) (t-lookup code env)
    (t-assignment? code) (t-assign code env)
    (t-do? code) (t-do code env)
    (t-if? code) (t-exec-if code env)
    (t-while? code) (t-do-while code env)
    (t-return? code) (t-make-return code env)
    (t-proc-defn? code) (t-make-proc code env)
    (t-proc-call? code) (t-proc-invoke code env)
    :else nil))

(defn tronx-eval-code [codes env]
  (doseq [code codes]
    (tronx-eval code env))
  env)


(comment

  (tronx-eval-code '((:= x 10)
                     (:= y 20)
                     (writeln x y)
                     (writeln (+ x y)))
                   env)

  (tronx-eval-code '((:= x 10)
                     (:= y 20)
                     (writeln x y)
                     (writeln (+ x y))
                     (:= z (* x (- (+ x y) y)))
                     (writeln z))
                   env)

  (tronx-eval-code '((proc plus (x y)
                           (writeln (+ x y)))
                     (plus 3 4))
                   env)

  (tronx-eval-code '((proc do-something (a b)
                           (:= x a)
                           (:= y b)
                           (writeln x y)
                           (writeln (+ x y))
                           (:= z (* x (- (/ x y) y)))
                           (writeln z))
                     (do-something 11 12))
                   env)

  (tronx-eval-code '((:= x 10)
                     (if (> x 9)
                       (do (write x)
                           (write ", ")
                           (writeln (+ x 1)))
                       (writeln (+ x 1))))
                   env)

  (tronx-eval-code '((proc op (o x y)
                           (writeln "execute op")
                           (:= r (o x y))
                           (writeln "result = " r))
                     (proc plus-2 (x y)
                           (return (+ 2 x y)))
                     (op + 3 4)
                     (op plus-2 3 4))
                   env)

  (tronx-eval-code '((:= y 100)
                     (do
                       (:= x (* y (+ y 1)))
                       (writeln "inner x " x))
                     (writeln "outer x " x)
                     (writeln "y " y))
                   env)

  (tronx-eval-code '(do
                      (proc f (n)
                            (g (+ n 1)))
                      (proc g (n)
                            (writeln n)
                            (return (+ 1 n)))
                      (:= x 10)
                      (f x)
                      (writeln (g (+ x 2))))
                   (new-env))

  (tronx-eval-code '(do
                      (proc fibo (n)
                            (if (<= n 1)
                              (return 1)
                              (return (+ (fibo (- n 1))
                                         (fibo (- n 2))))))
                      (proc factorial (n)
                            (if (<= n 1)
                              (return 1)
                              (return (* n (factorial (- n 1))))))
                      (writeln (fibo 4))
                      (writeln (factorial 4))
                      (writeln (factorial 10)))
                   (new-env))

  (tronx-eval-code '(do
                      (:= x 1)
                      (while (<= x 10)
                        (writeln x)
                        (:= x (+ x 1))))
                   (new-env))

  (tronx-eval-code '((proc fact (n)
                           (:= p 1)
                           (while (>= n 1)
                             (:= p (* p n))
                             (:= n (- n 1)))
                           (return p))
                     (writeln "Factorial of 5 = " (fact 5)))
                   (new-env))

  )