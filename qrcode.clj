#!/usr/bin/env inlein
; https://github.com/hyPiRion/inlein/releases/download/0.2.0/inlein
'{:dependencies [[org.clojure/clojure "1.11.1"] [com.google.zxing/core "3.5.2"] [org.babashka/cli "0.4.39"]]}

(import com.google.zxing.BarcodeFormat)
(import com.google.zxing.Binarizer)
(import com.google.zxing.BinaryBitmap)
(import com.google.zxing.DecodeHintType)
(import com.google.zxing.EncodeHintType)
(import com.google.zxing.LuminanceSource)
(import com.google.zxing.MultiFormatReader)
(import com.google.zxing.MultiFormatWriter)
(import com.google.zxing.RGBLuminanceSource)
(import com.google.zxing.Result)
(import com.google.zxing.common.BitMatrix)
(import com.google.zxing.common.HybridBinarizer)

(import javax.imageio.ImageIO)
(import java.awt.Color)
(import java.awt.image.BufferedImage)


(System/setProperty "java.awt.headless" "true")

(defn qrcode-scan [path]
  (let [image (ImageIO/read (clojure.java.io/input-stream (clojure.java.io/file path)))
        width (.getWidth image)
        height (.getHeight image)
        pixels (int-array (* width height))]
    (doseq [width (range width) height (range height)]
      (aset pixels (+ (* height (.getWidth image)) width) (.getRGB image width height)))
    (->> (RGBLuminanceSource. width height pixels)
         (HybridBinarizer.)
         (BinaryBitmap.)
         (#(.decode (MultiFormatReader.) % {DecodeHintType/CHARACTER_SET "UTF-8" DecodeHintType/POSSIBLE_FORMATS [BarcodeFormat/QR_CODE]}))
         (.getText))
    )
  )

(defn qrcode-matrix [content side-len]
  (let [side-len (if (int? side-len) side-len (Long/valueOf side-len))
        bitMatrix (.encode (MultiFormatWriter.) content BarcodeFormat/QR_CODE side-len side-len {EncodeHintType/CHARACTER_SET "UTF-8"})
        matrix (transient [])]
    (doseq [height (range side-len) width (range side-len)]
      (conj! matrix (.get bitMatrix width, height)))
    (let [matrix (persistent! matrix)]
      (mapv #(subvec matrix (* % side-len) (* (inc %) side-len)) (range side-len)))
    ))

(defn matrix-2-ascii [matrix up-side-down]
  (loop [print-context (transient []) matrix matrix]
    (if (empty? matrix)
      (clojure.string/join (persistent! print-context))
      (let [[up down] matrix]
        (doseq [[up down] (map list up down)]
          (conj! print-context
                 (cond
                   (and up down) (if up-side-down "█" " ")
                   up (if up-side-down "▀" "▄")
                   down (if up-side-down "▄" "▀")
                   :else (if up-side-down " " "█")))
          )
        (recur (conj! print-context "\n") (nthrest matrix 2))
        )
      )
    )
  )

(defn matrix-2-image [matrix up-side-down format path]
  (let [side-len (count matrix)
        pixels (int-array (* side-len side-len))]
    (doseq [height (range side-len) width (range side-len)]
      (aset pixels (+ (* height side-len) width) (.getRGB (if (get (get matrix height) width) (if up-side-down Color/WHITE Color/BLACK) (if up-side-down Color/BLACK Color/WHITE)))))
    (let [image (BufferedImage. side-len side-len BufferedImage/TYPE_INT_RGB)]
      (.setDataElements (.getRaster image) 0 0 side-len side-len pixels)
      (ImageIO/write image format (clojure.java.io/file path))
      )
    )
  )

(defn qrcode-ascii
  ([content] (qrcode-ascii content 80))
  ([content side-len] (qrcode-ascii content side-len false))
  ([content side-len up-side-down] (matrix-2-ascii (qrcode-matrix content side-len) up-side-down))
  )

(defn qrcode-image
  ([^String content path] (qrcode-image content path 100))
  ([^String content path side-len] (qrcode-image content path side-len :PNG))
  ([^String content path side-len format] (qrcode-image content path side-len format false))
  ([^String content path side-len format up-side-down]
   (when (#{"PNG" "JPEG"} (clojure.string/upper-case (name format)))
     (matrix-2-image (qrcode-matrix content side-len) up-side-down (clojure.string/upper-case (name format)) path)))
  )

(def scan qrcode-scan)
(def ascii qrcode-ascii)
(def image qrcode-image)


(some-> (apply (resolve (symbol (first *command-line-args*))) (rest *command-line-args*)) println)

;(defn help []
;  (println "
;Commands:
;  ascii   生成字符QRCode
;  image   生成图片QRCode
;  scan    扫描QRCode图片
;  help
;
;Usage:
;  qrcode [commands] [options]
;
;
;")
;
;  )
;
;(def table
;  [{:cmds ["ascii"] :fn qrcode-ascii :args->opts [:text :path :side :reverse] :alias {:t :text :p :path :s :side :r :reverse}}
;   {:cmds ["image"] :fn qrcode-image :args->opts [:text :path :format :side :reverse] :alias {:t :text :p :path :f :format :s :side :r :reverse}}
;   {:cmds ["scan"] :fn qrcode-scan :args->opts [:path] :alias {:p :path}}
;   {:cmds ["help"] :fn delete }
;   {:cmds [] :fn help}])
;
;(defn -main [& args]
;  (babashka.cli/dispatch table args))
