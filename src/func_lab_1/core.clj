(ns func-lab-1.core
  (:gen-class))

(defprotocol FamilyProtocol
  (is-with-twins [this]))

; Реализация варианта №3 заданий 7-10
(defrecord Name [first last middle])
(defrecord Child [name birthday sex monthly-income is-twin])
(defrecord Parent [name birthday sex monthly-income])
(defrecord Family [wife husband children]
  FamilyProtocol
  (is-with-twins [_] (boolean (some #(= (:is-twin %) true) children))))

(defn flatten-people
  [families]
  (flatten (map (fn [f] [(:wife f) (:husband f) (:children f)]) families)))

; #1
(defn people-with-income-less
  [income families]
  (filter #(< (:monthly-income %) income) (flatten-people families)))

; #2
(defn children-younger-than
  [year families]
  (->> (seq families)
       (map #(:children %))
       (flatten)
       (filter #(> (:birthday %) year))))

; #3
(defn unemployed-wives-younger-than
  [year families]
  (flatten (map #(if (and (zero? (-> % :wife :monthly-income)) (> (-> % :wife :birthday) year)) (:wife %) []) families)))

; #4
(defn children-with-parents-diff
  [diff families]
  (flatten (map #(if (>= (Math/abs (- (-> % :husband :birthday) (-> % :wife :birthday))) diff) (:children %) []) families)))

; #5
(defn without-twins-count
  [families]
  (count (filter #(not (is-with-twins %)) families)))

; examples
(def families [(Family.
                (Parent. (Name. "Anna" "Brown" "Alice") 1980 "female" 3200)
                (Parent. (Name. "Jonh" "Brown" "Jake") 1975 "male" 3555)
                [(Child. (Name. "Bred" "Brown" "Bond") 2007 "male" 0 false)
                 (Child. (Name. "Tonya" "Brown" "Ann") 2005 "female" 0 false)])
               (Family.
                (Parent. (Name. "Светлана" "Иванова" "Ивановна") 1970 "female" 1200)
                (Parent. (Name. "Петр" "Иванов" "Федорович") 1971 "male" 0)
                [(Child. (Name. "Егор" "Иванов" "Петрович") 2007 "male" 0 false)])
               (Family.
                (Parent. (Name. "Наталья" "Петрова" "Николаевна") 1960 "female" 0)
                (Parent. (Name. "Андрей" "Петров" "Александрович") 1955 "male" 10000)
                [(Child. (Name. "Даниил" "Петров" "Андреевич") 1980 "male" 3100 false)
                 (Child. (Name. "Иван" "Петров" "Андреевич") 1987 "male" 4000 true)
                 (Child. (Name. "Сайонара" "Петрова" "Андреевна") 1987 "female" 1337 true)])])

(people-with-income-less 1500 families)
(children-younger-than 2000 families)
(unemployed-wives-younger-than 1950 families)
(children-with-parents-diff 3 families)
(without-twins-count families)

(defn insert-few
  "Вариант №24.
   Написать программу вставки m раз в список произвольной константы,
   начиная с N-й позиции."
  [element m N list]
  (concat (take N list) (concat (repeat m element) (drop N list))))

(defn merge-odd-even
  "Вариант №6.
   Написать программу объединения двух списков в третий так,
   чтобы нечетные (по номеру) элементы были из первого cписка,
   а четные - из второго."
  [odd-list even-list]
  (loop [odd odd-list
         even even-list
         result '()]
    (cond
      (empty? odd) result
      (empty? even) (recur '() '() (concat result [(first odd)]))
      :else (recur (rest odd) (rest even) (concat result [(first odd) (first even)])))))

; examples
(def list1 '("a" "b" "c" "d"))
(def list2 '("1" "2" "3"))
(insert-few "A" 3 1 list2)
(merge-odd-even list1 list2)

(defn -main [])