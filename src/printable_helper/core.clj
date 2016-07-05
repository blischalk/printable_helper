(ns printable-helper.core
  (:gen-class))

(def printable-bytes
  (map int "%_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))


(def byte-perms
  (for [byte-a printable-bytes
        byte-b printable-bytes
        byte-c printable-bytes
        byte-d printable-bytes]
    (bit-or byte-a
            (bit-shift-left byte-b 8)
            (bit-shift-left byte-c 16)
            (bit-shift-left byte-d 24))))


(defn winning-subtraction-set? [start end words]
  (= end (bit-and (apply - start words) 0xffffffff)))


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


(defn find-n-subtractions [start end permutations]
  (first (filter (partial winning-subtraction-set? start end) permutations)))


(defn print-subtractions [[start end max-subtractions]]
  (println "Calculating required subtractions...")
  (println "Starting address: " start)
  (doseq [subtraction (find-n-subtractions
                       (read-string start)
                       (read-string end)
                       (all-perms byte-perms (read-string max-subtractions)))]
    (println (format "                 - 0x%08x" subtraction)))
  (println "Ending address:   " end))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-subtractions args))
