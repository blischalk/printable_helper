(ns printable-helper.core
  (:gen-class))


;; printable_helper.c
;; running time for comparison
;;
;;start: 0x00000000
;;
;;     - 0x77777777
;;     - 0x77777746
;;     - 0x32635254
;;-------------------
;;end:   0xdeadbeef
;;
;;real    0m0.109s
;;user    0m0.013s
;;sys     0m0.007s

;; Current Clojure Benchmark
;;
;; Calculating required subtractions...
;; Starting address:  0x00000000
;;                  - 0x5f5f5f25
;;                  - 0x5f786772
;;                  - 0x627a7a7a
;; Ending address:    0xdeadbeef
;; "Elapsed time: 65.539823 msecs"


(def printable-bytes
  "All the printable bytes we can use in a memory address cast to their
  integer equivalents."
  (map int "%_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))

(def bits-in-byte 8)


(defn get-bytes
  "Takes a starting and ending 4 byte word memory address
  and breaks out the individual bytes of each address into
  a collection of starting bytes and ending bytes. The bytes
  are ordered lowest to highest order."
  [start end]
  (for [v [start end]]
    (for [byte (range 4)]
      (unsigned-bit-shift-right
       (bit-and v (bit-shift-left 0xff (* byte bits-in-byte)))
       (* byte bits-in-byte)))))


(defn n-length-perms
  "Calculates all the permutions of items including n items."
  [items n]
  (if (zero? n) '(())
      (mapcat (fn [c]
                (map #(cons c %)
                     (n-length-perms items (dec n))))
              items)))


(defn all-perms
  "Calculates all the permutations of items including
  1 up to and including n items."
  [items n]
  (if (zero? n) '()
      (lazy-cat (all-perms items (dec n))
                (n-length-perms items n))))


(def byte-combinations
  "lazy sequence of all permutations of 1 - 4 byte
  combinations of the allowed printable bytes."
  (all-perms printable-bytes 4))


(defn calc-bytes-for-words
  "
  Builds a sequence of columns of bytes which when subtracted would
  produce the desired byte as the difference.
  "
  [[[sb & rsb] [eb & reb]] carry results search-bytes]
  (if (and sb eb)
    (let [match (first (filter #(= sb (bit-and (apply + carry eb %)
                                               0x000000ff))
                               search-bytes))
          new-carry (unsigned-bit-shift-right (bit-and (apply + carry eb match)
                                              0x0000ff00)
                                     bits-in-byte)
          new-results (conj results match)
          new-search-bytes (filter #(= (count %) (count match)) search-bytes)]
      (recur [rsb reb] new-carry new-results new-search-bytes))
    results))


(defn build-word
  "Builds a word out of a collection of bytes, starting from the lowest
  order byte working toward the highest, shifting by the appropriate
  offsets"
  [itm]
  (apply bit-or
         (map-indexed
          (fn [idx i]
            (bit-shift-left i (* idx bits-in-byte)))
          itm)))


(defn columns-to-bytes-for-words
  "Takes a collection of byte columns and converts
  them into sequences of bytes, lowest order first
  ready to be combined into a memory address."
  [words]
  (for [n (range 0 4)] (map #(nth % n 0) words)))


(defn build-words
  "Takes a sequence of columns of bytes and returns a sequence
  of memory addresses with memory addresses of zero removed."
  [words]
  (remove zero?
          (reduce
           (fn [coll itm] (conj coll (build-word itm)))
           []
           (columns-to-bytes-for-words words))))


(defn print-subtractions' [[start end]]
  (println "Calculating required subtractions...")
  (time
   (do
     (println "Starting address: " start)
     (doseq [word (-> (get-bytes (read-string start) (read-string end))
                      (calc-bytes-for-words 0 [] byte-combinations)
                      build-words)]
       (println (format "%18s 0x%08x" "-" word)))
     (println "Ending address:   " end))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-subtractions' args))
