(ns printable-helper.core
  (:gen-class))

(def printable-bytes
  "All the printable bytes we can use in a memory address."
  (map int "%_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-"))


(def byte-offsets
  "Byte offsets useful for shifting."
  [0 8 16 24])


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
  1 up to and includig n items."
  [items n]
  (if (zero? n) '()
      (lazy-cat (all-perms items (dec n))
                (n-length-perms items n))))


(def byte-combinations
  "lazy sequency of all permutations of 1 - 4 byte
  combinations of the allowed printable bytes."
  (all-perms printable-bytes 4))


(defn calc-words
  "Recursively builds reversed memory addresses as collections  of bytes e.g
  lowest order byte first in each collection. A column of bytes, e.g the byte in
  the same position of each collection includes the required bytes
  that when subtracted from a starting byte will achieve the desired difference.

  Each collection is a reversed word.
  Each position within a collection will end up subtracted from the same
  position in the next collection the same way you would do normal subtraction,
  starting with the lowest order term of each number, subtracting, and
  working your way towards the highest order terms.
  "
  [[start-bytes end-bytes] carry results]
  (let [sb (first start-bytes) eb (first end-bytes)]
    (if (and sb eb)
      (let [match (first (filter #(= sb (bit-and (apply + carry eb %)
                                                 0x000000ff))
                                 byte-combinations))
            new-carry (bit-shift-right (bit-and (apply + carry eb match)
                                                0x0000ff00)
                                       8)
            new-results (conj results match)]
        (recur [(rest start-bytes) (rest end-bytes)] new-carry new-results))
      results)))


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


(defn format-words
  "Pad's words with 0's and turns the reversed collections of
   bytes from calc-words into actual memory addresses. Proceeds to
   format each word as a 4 byte string for printing."
  [words]
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
