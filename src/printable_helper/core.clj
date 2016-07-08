(ns printable-helper.core
  (:gen-class))

(def printable-bytes
  (map int "%_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))


(defn n-length-perms [byte-perms max-subtractions]
  (if (zero? max-subtractions) '(())
      (mapcat (fn [c]
                (map #(cons c %)
                     (n-length-perms byte-perms
                                     (dec max-subtractions))))
              byte-perms)))


(defn all-perms [byte-perms max-subtractions]
  (if (zero? max-subtractions) '()
      (lazy-cat (n-length-perms byte-perms max-subtractions)
                (all-perms byte-perms (dec max-subtractions)))))


(def byte-combinations
  (lazy-cat (all-perms printable-bytes 1)
            (all-perms printable-bytes 2)
            (all-perms printable-bytes 3)
            (all-perms printable-bytes 4)))


(defn calc-words [[start-bytes end-bytes] carry results]
  (let [sb (first start-bytes) eb (first end-bytes)]
    (if (and sb eb)
      (let [match (first (filter #(= sb (bit-and (apply + carry eb %) 0x000000ff)) byte-combinations))
            new-carry (bit-shift-right (bit-and (apply + carry eb match) 0x0000ff00) 8)
            new-results (conj results match)]
        (recur [(rest start-bytes) (rest end-bytes)] new-carry new-results))
      results)))


(defn get-bytes [start end]
  (let [sb1 (bit-and start 0x000000ff)
        sb2 (bit-shift-right (bit-and start 0x0000ff00) 8)
        sb3 (bit-shift-right (bit-and start 0x00ff0000) 16)
        sb4 (bit-shift-right (bit-and start 0xff000000) 24)
        eb1 (bit-and end 0x000000ff)
        eb2 (bit-shift-right (bit-and end 0x0000ff00) 8)
        eb3 (bit-shift-right (bit-and end 0x00ff0000) 16)
        eb4 (bit-shift-right (bit-and end 0xff000000) 24)]
    [[sb1 sb2 sb3 sb4] [eb1 eb2 eb3 eb4]]))

(def byte-offsets [0 8 16 24])

(defn format-words [words]
  (map #(format "0x%08x" %)
       (remove zero?
               (reduce
                (fn [coll itm]
                  (conj coll (apply bit-or
                                    (map-indexed
                                     (fn [idx i]
                                       (bit-shift-left
                                        i
                                        (get byte-offsets idx)))
                                     itm))))
                []
                (for [n (range 0 4)]
                  (map #(nth % n 0) words))))))


(defn print-subtractions' [[start end]]
  (println "Calculating required subtractions...")
  (println "Starting address: " start)
  (time (doseq [word (-> (get-bytes (read-string start) (read-string end))
                         (calc-words 0 [])
                         (format-words))]
          (println word)))
  (println "Ending address:   " end))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-subtractions' args))
