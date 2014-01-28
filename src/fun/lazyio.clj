;; Self contained example - Lazy I/O etc
;; (C) Johan Astborg, 2014

(def filename "out.gz")

(defn lazy-gzip-read  [file]
  "lazy gzip file reader"
  (let [zip (java.util.zip.GZIPInputStream. (java.io.FileInputStream. file))
      reader (java.io.BufferedReader. (java.io.InputStreamReader. zip "UTF-8"))]
  (letfn [(helper [rdr]
                  (lazy-seq
                    (if-let [line (.readLine rdr)]
                      (cons line (helper rdr))
                      (do (.close rdr) nil))))]
         (helper reader))))

(defn eager-gzip-write [file l]
  "writes seq to gzip file"
  (let [zip (java.util.zip.GZIPOutputStream. (java.io.FileOutputStream. file))
      writer (java.io.BufferedWriter. (java.io.OutputStreamWriter. zip "UTF-8"))]
    (letfn [(helper [wtr data]
                    (doto wtr (.append data) (.newLine)))]
      (doall (map (partial helper writer) l)) (.close writer))))

;; Write sample data
(def sample-data (map str (take 10 (reductions + (cycle [(-> 1 Math/exp)])))))
(eager-gzip-write filename sample-data)

;; Read the same data and check
(def input-data (take 10 (lazy-gzip-read filename)))
(= input-data sample-data)

