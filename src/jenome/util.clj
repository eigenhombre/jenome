;; Byte swap stuff, since I originally couldn't figure out how to get Gloss to do this:

(defn word-to-bytes [w]
  (for [amt (range 0 32 8)]
    (bit-and (bit-shift-right w amt) 0xFF)))

(defn bytes-to-word-swap [bs]
  (reduce
   (fn [a b] (bit-or (bit-shift-left a 8) b))
   bs))

(defn bytes-to-word [bs]
  (bytes-to-word-swap (reverse bs)))

(defn swap-order [w]
  (bytes-to-word-swap (word-to-bytes w)))

(expect (word-to-bytes 0x11223344) [0x44 0x33 0x22 0x11])
(expect (word-to-bytes 0xFFEE8877) [0x77 0x88 0xEE 0xFF])
(expect (bytes-to-word (word-to-bytes 0xFFEE8877)) 0xFFEE8877)
(expect (swap-order 0xFFEE8877) 0x7788EEFF)

;; Actual decoder

