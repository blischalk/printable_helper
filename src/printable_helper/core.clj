(ns printable-helper.core
  (:gen-class))

(def printable-bytes
  "All the printable bytes we can use in a memory address cast to their
  integer equivalents."
  (map int "%_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))


(def byte-offsets
  "Byte offsets useful for shifting."
  [0 8 16 24])


(defn get-bytes
  "Takes a starting and ending 4 byte word memory address
  and breaks out the individual bytes of each address into
  a collection of starting bytes and ending bytes. The bytes
  are ordered lowest to highest order."
  [start end]
  (let [sb1 (bit-and start 0x000000ff)
        sb2 (bit-shift-right (bit-and start 0x0000ff00) 8)
        sb3 (bit-shift-right (bit-and start 0x00ff0000) 16)
        sb4 (bit-shift-right (bit-and start 0xff000000) 24)
        eb1 (bit-and end 0x000000ff)
        eb2 (bit-shift-right (bit-and end 0x0000ff00) 8)
        eb3 (bit-shift-right (bit-and end 0x00ff0000) 16)
        eb4 (bit-shift-right (bit-and end 0xff000000) 24)]
    [[sb1 sb2 sb3 sb4] [eb1 eb2 eb3 eb4]]))


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
  [[start-bytes end-bytes] carry results search-bytes]
  (let [sb (first start-bytes) eb (first end-bytes)]
    (if (and sb eb)
      (let [match (first (filter #(= sb (bit-and (apply + carry eb %)
                                                 0x000000ff))
                                 search-bytes))
            new-carry (bit-shift-right (bit-and (apply + carry eb match)
                                                0x0000ff00)
                                       8)
            new-results (conj results match)
            new-search-bytes (filter #(= (count %) (count match)) search-bytes)]
        (recur [(rest start-bytes) (rest end-bytes)]
               new-carry
               new-results
               new-search-bytes))
      results)))


(defn build-word
  "Builds a word out of a collection of bytes, starting from the lowest
  order byte working toward the highest, shifting by the appropriate
  offsets"
  [itm]
  (apply bit-or
         (map-indexed
          (fn [idx i]
            (bit-shift-left i (get byte-offsets idx)))
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
