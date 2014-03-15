;;;; wo-splot.lisp

(in-package #:wo-splot)

;;; "wo-splot" goes here. Hacks and glory await!
;;;
(defclass draw-area ()
  ((canvas-width :initarg :canvas-width)
   (canvas-height :initarg :canvas-height)
   (min-time :accessor min-time)
   (max-time :accessor max-time)))


(defclass event ()
  ((color :accessor color :initarg :color)
   (ident :accessor ident :initarg :ident)))


;;; SEGMENT CLASS
(defclass segment (event)
  ((start :accessor start :initarg :start)
   (stop :accessor stop :initarg :stop)))'

(defmethod duration ((segment segment))
 (- (stop segment) (start segment)))
;;; MOMENT CLASS
(defclass moment (event)
  ((moment :accessor moment :initarg :moment)))

(defmethod start ((moment moment))
  (moment moment))

(defmethod stop ((moment moment))
  (moment moment))

(defmethod duration ((moment moment))
  0)
;;;;;;;;;;;;;;;;;;;;;

;;; CONVERSION METHODS
(defmethod x-point ((value integer) (area draw-area))
  (with-slots (canvas-width min-time max-time) area
    (coerce  (* canvas-width (/ (- value min-time) (- max-time min-time))) 'float)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DRAWING METHODS PNG
(defmethod set-stroke-color ((event event))
  (apply #'set-rgb-stroke (color event)))

(defmethod draw-at-height ((segment segment) (height number) (area draw-area))
  (move-to (x-point (start segment) area) height)
  (line-to (x-point (stop segment) area) height)
  (set-stroke-color segment)
  (stroke))


(defmethod draw-at-height ((moment moment) (height number) (area draw-area))
  (centered-circle-path (x-point (moment moment) area) height 1.0)
  (set-stroke-color moment)
  (stroke))


;;; DRAW AREA METHODS

(defun configure-area-from-data (area data)
  (loop :for segment :in data
     :minimizing (start segment) :into min-start
     :maximizing (stop segment) :into max-end
     :finally (progn (setf (min-time area) min-start) (setf (max-time area) max-end))))


(defun ident-height-map (data)
  (loop :with result = (make-hash-table :test 'equalp)
     :with max = -1
     :for element :in data
     :finally (return result)
     :do
     (unless (gethash (ident element) result)
       (setf (gethash (ident element) result) (incf max)))))

(defun draw-data (data file-name
		  &key
		    (width 2000)
		    (height (hash-table-count (ident-height-map data))))
  "Draws the data to the png with file-name"
  (with-canvas (:width width :height height)
    (set-rgb-fill 1.0 1.0 1.0)
    (rectangle 0 0 width height)
    (fill-path)
    (let ((area (make-instance 'draw-area :canvas-height height :canvas-width width)))
      (configure-area-from-data area data)
      (loop 
	 :with height-map = (ident-height-map data)
	 :for element :in data 
	 :for height = (gethash (ident element) height-map)
	 :do
	 (draw-at-height element (+ 0.5 height) area)))
    (save-png file-name)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAWING SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod draw-svg-at-height (scene (segment segment) (height number) (area draw-area))
  (let* ((x1 (x-point (start segment) area))
	 (x2 (max (+ 1.0 x1) (x-point (stop segment) area))))
    (cl-svg:draw scene (:line :x1 x1 #+nil (x-point (start segment) area)
			      :y1 height
			      :x2 x2 #+nil (x-point (stop segment) area)
			      :y2 height
			      :stroke "blue"))))


(defmethod draw-svg-at-height (scene (moment moment) (height number) (area draw-area))
  (cl-svg:draw scene (:circle :cx (x-point (moment moment) area)
			      :cy height
			      :r 0.5
			      :fill "red")))


(defun draw-data-svg (data file-name
		      &key 
			(width 2000)
			(height (hash-table-count (ident-height-map data))))
  (with-standard-io-syntax
    (cl-svg:with-svg-to-file
	(scene 'cl-svg:svg-1.1-toplevel :width width :height height)
      (file-name :if-exists :supersede :if-does-not-exist :create)
      (let ((area (make-instance 'draw-area :canvas-height height :canvas-width width)))
	(configure-area-from-data area data)
	(loop
	   :with height-map = (ident-height-map data)
	   :for element :in data
	   :for height = (gethash (ident element) height-map)
	   :do
	   (draw-svg-at-height scene element (+ 0.5 height) area))))))


;;; PARSING

(defun scan-match (parser line)
  "Return a generalized true value if the PARSER matches LINE.
If the PARSER has parenthesized expressions, return the first captured group."
  (multiple-value-bind (whole parts)
      (cl-ppcre:scan-to-strings parser line)
    (if (and parts (> (length parts) 0))
	(aref parts 0)
	(and whole t))))

(defun parse-file-2 (file-name config-name)
  "Parses the log in `file-name' by the patterns specified by the `config-name'.
Return  a list of event entries."
  (let* ((config (with-open-file (s config-name) (read s)))
	 (scanners (loop :for (key parser) :on config :by #'cddr
		      :do (format t "Key: ~A, Parser: ~A~%" key parser)
		      :collect `(,key ,(cl-ppcre:create-scanner parser)))))
    
    (format t "Created scanners: ~A" scanners)
    (with-open-file (s file-name :external-format :utf-8)
      (loop 
	 :with result = (list)
	 :for line = (read-line s nil nil) :while line
	 :finally (return (nreverse result))
	 :for element = (list)
	 :do
	 (loop :for (key parser) :in scanners :do
	    (alexandria:when-let (match (scan-match parser line))
	      (push match element)
	      (push key element)))
	 (when element
	   (alexandria:when-let (time-value (getf element :time))
	     (push (net.telent.date:parse-time time-value) element)
	     (push :parsed-time element))
	   (push element result))))))



(defun visualize-log (log-file-name config-file-name png-file-name)
  (draw-data (convert-simple-data (parse-file-2 log-file-name config-file-name)) png-file-name))

;;; CONVERSION METHODS

(defun plist-element-< (a b)
  "Compare p-list A and B by the value of :parsed-time.
If the parsed times are equal, put order an element with :start before the others.

This is a slight hack to make the rest of the code work better if a start and end have the same parsed time."
  (let ((time-a (getf a :parsed-time))
	(time-b (getf b :parsed-time)))
    (if (= time-a time-b)
	(getf a :start)
	(< time-a time-b))))


(defun convert-simple-data (list)
  (let ((slist list  #+nil (sort (copy-seq list) #'plist-element-<)))
    (loop 
       :with start = (make-hash-table :test #'equal)
       :with result = (list)

       :for element :in slist
       :for ident = (getf element :ident "<NONE>")
       :for time = (getf element :parsed-time)
       :finally (return (nreverse result))
       :do
       (cond 
	 ((getf element :start) 
	  (setf (gethash ident start) time))
	 ((getf  element :stop) 
	  (multiple-value-bind (start found) (gethash ident start)
	    (when found
	      (push (make-instance 'segment 
				   :ident ident
				   :start start 
				   :stop time 
				   :color (list 0.0 0.0 1.0)) 
		    result))))
	 ((getf element :moment)
	  (push (make-instance 'moment 
			       :ident ident
			       :moment time 
			       :color (list 1.0 0.0 0.0))
		result))))))

