(reduce + [1 2 3 4])

(def split-at-by
  (fn [list strings]
    (loop [acc [] c 0 l list s strings]
      (if (or (empty? l) (> (inc c) (count strings))) (println "==> " acc)
      (if (= c (first l))
        (do (println "==> " acc) (recur (conj [] (first s)) (inc c) (next l) (next s)))
          (do (recur (conj acc (first s)) (inc c) l (next s))))))))

(split-at-by [2 3 5] ["johan" "bor" "i" "skogen" "hela" "dagen"])


;;;;;;;;;;;;;;;;;;;; Generate code

(def funs1 ['Math/sin 'Math/cos 'Math/tan 'Math/exp])
(def funs2 ['+ '- '* '/])
(defn gennode1 [] (list (nth funs1 (rand-int (count funs2))) 1))
(defn gennode2 []
  (list (nth funs2 (rand-int (count funs2)))
        (rand-int 10)
        (rand-int 10)))

(gennode1)
(gennode2)

(defn dummy [a]
  (if (neg? a) 1
      (list (nth funs2 (rand-int (count funs2))) (rand-int 10) (dummy (dec a)))))

(dummy 5)

;;;;;;;;;;;;;;;;;;;; Create image and save to disk

(defn erase [image]
	(let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)]
   (.setColor graphics (java.awt.Color. (+ (rand-int 25) 225) (+ (rand-int 25) 225) (+ (rand-int 25) 225)))
   (.fillRect graphics 0 0 w h)))

(defn paint-some [image]
  (let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)
        rw (rand-int w)
        rh (rand-int h)]
    (.setColor graphics (java.awt.Color. 0 0 0 100))
    (.fillRect graphics (+ rw 5) (+ rh 5) 100 100)
    (.setColor graphics (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255) 100))
    (.fillRect graphics rw rh 100 100)))

(defn paint-some-random [image]
  (let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)
        rx (rand-int w)
        ry (rand-int h)
        rh (rand-int w)
        rw (rand-int h)]
    (.setColor graphics (java.awt.Color. 0 0 0 100))
    (.fillRect graphics (+ rx 5) (+ ry 5) rw rh)
    (.setColor graphics (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255) 100))
    (.fillRect graphics rx ry rw rh)))

(defn paint-some-more [image n]
  (let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)]
    ;(.setColor graphics (java.awt.Color. 0 0 0 150))
    ;(.fillOval graphics (+ rw 5) (+ rh 5) 100 100)
    (dotimes [n n]
        (.setColor graphics (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255) 100))
        (.fillOval graphics (rand-int w) (rand-int h) 100 100))))

(defn paint-poly-art [image n c]
  (let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)]
    (dotimes [n n]
      (.setColor graphics (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255) 25))
      (.fillPolygon graphics
        (int-array (take c (repeatedly #(rand-int w))))
        (int-array (take c (repeatedly #(rand-int h)))) c))))

(defn paint-poly-art-cluster [image n c s]
  (let [graphics (.getGraphics image)
        w (.getWidth image)
        h (.getHeight image)
        cx (rand w)
        cy (rand h)]
    (dotimes [n n]
      (.setColor graphics (java.awt.Color. (rand-int 255) (rand-int 255) (rand-int 255) 25))
      (.fillPolygon graphics
        (int-array (take c (repeatedly #(+ (rand-int s) cx) )))
        (int-array (take c (repeatedly #(+ (rand-int s) cy) ))) c))))

;; Work on buffered image, then save to disk
(def buffered-image (java.awt.image.BufferedImage. 1024 768 java.awt.image.BufferedImage/TYPE_INT_ARGB))

(paint-poly-art-cluster buffered-image 10 200 150)
(paint-some buffered-image)
(paint-poly-art buffered-image 5 100)
(paint-some-random buffered-image)

;; demo
(erase buffered-image)
(paint-poly-art buffered-image 50 10)

(defn save-buffered-image [filename image]
  (let [file (java.io.File. filename)]
    (javax.imageio.ImageIO/write image "png" file)))

(save-buffered-image "output.png" buffered-image)

;; Pixel-per-pixel image comparison
(defn read-buffered-image [filename]
  (let [file (java.io.File. filename)]
    (javax.imageio.ImageIO/read file)))

(def *source-image* (read-buffered-image "/home/gecemmo/Downloads/abstract2.jpg"))

;; copy random pixels to output file...
(def w 600)
(def h 800)
(def *target-image*
  (java.awt.image.BufferedImage.
   (.getWidth *source-image*)
   (.getHeight *source-image*)
   java.awt.image.BufferedImage/TYPE_INT_ARGB))

(defn alter-color [color]
  (let [r (.getRed color)
        g (.getGreen color)
        b (.getBlue color)]
    (java.awt.Color. r g b (rand-int 100))))

(alter-color (java.awt.Color. 200 100 20 10))

(defn create-random-samples [source target]
  (let [gs (.getGraphics source)
        gt (.getGraphics target)]
    (dotimes [x 25000]
      (let [x (rand-int (.getWidth source))
            y (rand-int (.getHeight target))]
        (.setColor gt (alter-color (java.awt.Color. (.getRGB source x y))))
        (.fillRect gt x y (rand-int 10) (rand-int 10))))))

(erase *target-image*)
(create-random-samples *source-image* *target-image*)
(save-buffered-image "output2.png" *target-image*)

;; AST-tree to draw polygons

(def test-sb (list 'set-brush 100 100 23 100)
(def test-dp (list 'draw-poly [11 200 90 40] [40 50 100 120]))
(def test-cmd (list test-sb test-dp))

(def *image* (java.awt.image.BufferedImage. 600 800 java.awt.image.BufferedImage/TYPE_INT_ARGB))

(my-eval test-cmd *image*)

(erase *image*)

(paint-poly-art *image* 5 25)

;; Save image
(save-buffered-image "output.png" *image*)

(defn do-stuff [[c xs ys]]
    (let [graphics (.getGraphics *image*)]
      (.setColor graphics (java.awt.Color. (:r c) (:g c) (:b c) (:a c)))
      (.fillPolygon graphics
                    (int-array xs)
                    (int-array ys)
                    (count xs))))

;(def test-cmd [[100 100 23 100] [11 200 90 140] [40 50 100 190]])
(do-stuff [(Color. 10 10 100 100) [19 29 99 299] [40 90 100 110]])

;; Create random draw command
(apply vector (take 10 (repeatedly #(rand-int 100))))

(def w 600)
(def h 800)

(do-stuff [(apply vector (take 4 (repeatedly #(rand-int 255))))
           (apply vector (take 3 (repeatedly #(rand-int w))))
           (apply vector (take 3 (repeatedly #(rand-int h))))])

;; 1. AST drawing
;; 2. Compare images (fitness function)
;; 3. Mutate and cross-over (breed new generation)
;; 4. Output snapshot (most fit candidate) to JPanel

(defrecord Color [r g b a])
(def color1 (Color. 100 100 100 50))

(defn draw-command [np w h]
  (vector
   (Color. (rand-int 255) (rand-int 255) (rand-int 255) (rand-int 150))
   (apply vector (take np (repeatedly #(rand-int w))))
   (apply vector (take np (repeatedly #(rand-int w))))))

(defn list-of-commands [nc]
  (apply vector (take nc (repeatedly #(draw-command (+ (rand-int 10) 3) 800 600)))))

(draw-command 5 800 600)

(map do-stuff *image* (list-of-commands 10))

(defn gen-image []
  (erase *image*)
  (map (partial do-stuff *image*) (list-of-commands 10))
  (save-buffered-image "output.png" *image*))

(gen-image)

;; Works now, using doseq
(defn gen-image []
  (let [image (java.awt.image.BufferedImage. 600 800 java.awt.image.BufferedImage/TYPE_INT_ARGB)
        cmds (list-of-commands 10)
        draw-one (fn [image [c xs ys]]
                   (let [graphics (.getGraphics image)]
                    (.setColor graphics (java.awt.Color. (:r c) (:g c) (:b c) (:a c)))
                    (.fillPolygon graphics
                        (int-array xs)
                        (int-array ys)
                        (count xs))))]
    (doseq [c cmds] (draw-one image c)) image))

(save-buffered-image "output.png" (gen-image))

;; Rotate pattern
(defn draw-quadro []
  (let [image (java.awt.image.BufferedImage. 800 800 java.awt.image.BufferedImage/TYPE_INT_ARGB)
        cmds (list-of-commands 10)
        draw-one (fn [image c xs ys]
                   (let [graphics (.getGraphics image)]
                    (.setColor graphics (java.awt.Color. (:r c) (:g c) (:b c) (:a c)))
                    (.fillPolygon graphics
                        (int-array xs)
                        (int-array ys)
                        (count xs))))]
    (do (erase image)
    (doseq [r (range 0 (* 2 (Math/PI)) (/ (Math/PI) 9.0))]
      (let [nxs (map first (rotate r))
            nys (map second (rotate r))
            color (Color. (rand-int 255) (rand-int 255) (rand-int 255) 25)]
      (draw-one image color (map #(+ 400 %) nxs) (map #(+ 400 %) nys))))) image))

;; Rotate (x,y) f angles
(map #(double (/ % 10)) (range (* 2 (Math/PI))))

(range 0 6.2 0.1)

(defn rotate [r]
  (map #(vector (- (* (Math/cos r) %1) (* (Math/sin r) %2))
                (+ (* (Math/sin r) %1) (* (Math/cos r) %2)))
       [-220 220 220 -220] [220 220 -220 -220]))

(save-buffered-image "output.png" (draw-quadro))

;; Agent based JPanel

(def width 900)
(def height 600)

(defn render
 [g]
 (let [img (new java.awt.image.BufferedImage width height
                 (. java.awt.image.BufferedImage TYPE_INT_ARGB))
       bg (. img (getGraphics))]
   (doto bg
      (.setColor (. java.awt.Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight)))
      (.setColor (. java.awt.Color red))
      (.drawOval 200 200 (rand-int 100) (rand-int 50)))
   (. g (drawImage img 0 0 nil))
   (. bg (dispose))
   ))

(def panel (doto (proxy [javax.swing.JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new java.awt.Dimension
                                     width
                                     height))))
(.setVisible panel true)
(def animator (agent nil))

(defn animation
  [x]
  (send-off *agent* #'animation)
  (. panel (repaint))
  (. Thread (sleep 100)))

(send-off animator animation)

(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(macroexpand '(with-action (javax.swing.JButton. "Start") e (send flipper start)))
;;=> (. (javax.swing.JButton. "Start") user/addActionListener
;;      (clojure.core/proxy [java.awt.event.ActionListener] []
;;      (user/actionPerformed [e] (send flipper start))))

;;=> (. (javax.swing.JButton. "Start") user/addActionListener
;;      (clojure.core/proxy [java.awt.event.ActionListener] []
;;      (user/actionPerformed [e])))

(comment
(def counter (agent 0))
(send counter inc)
@counter

(add-watch a :key (fn [k r os ns] (print k r os ns)))
)

(let [click (ref nil)
      panel (proxy [javax.swing.JPanel] []
              (paintComponent [g]
                (proxy-super paintComponent g)
               ; (.drawImage g (.getImage
                          ;  (javax.swing.ImageIcon. "play.png"))
;                            0 0 (.getWidth this) (.getHeight this) nil)
                (if @click
                  (.fillRect g (:x @click) (:y @click) 10 10))))]
  (.addMouseListener panel
      (proxy [java.awt.event.MouseAdapter] []
        (mouseClicked [e]
          (let [p (.getPoint e)]
            (dosync (ref-set click {:x (.x p), :y (.y p)})))
          (javax.swing.SwingUtilities/invokeLater #(.repaint panel)))))
  (doto (javax.swing.JFrame.)
    (.setContentPane panel)
    (.setSize 200 200)
    (.show)))




;; TODO - Simple 3D Engine
