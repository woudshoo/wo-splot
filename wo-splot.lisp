;;;; wo-splot.lisp

(in-package #:wo-splot)

;;; "wo-splot" goes here. Hacks and glory await!


;;; The format of the data is build up from line segments:
;;;
;;;   (segment :start time :end time :color ....)
;;;
;;; The segments are collected in series:
;;;
;;;   (serie :segment...)
;;;
;;;
(defclass draw-area ()
  ((canvas-width :initarg :canvas-width)
   (canvas-height :initarg :canvas-height)
   (min-time :accessor min-time)
   (max-time :accessor max-time)))

;(defmethod move)

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
    (* canvas-width (/ (- value min-time) (- max-time min-time)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DRAWING METHODS
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

(defun draw-data (data file-name &key 
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
       

;;; PARSING

(defparameter *start-expression* "going to sleep$")
(defparameter *stop-expression* "waking up$")
(defparameter *time-expression* ".{15}")
(defparameter *moment-expression* nil)
(defparameter *ident-expression* nil)

(defun scan-match-to-string (expr line)
  (multiple-value-bind (whole parts) 
      (cl-ppcre:scan-to-strings expr line)
    (if (and parts (> (length parts) 0))
	(aref parts 0)
	whole)))

(defun parse-file (file-name config-name)
  "Parses the log in `file-name' by the patterns specified by the `config-name'.
Return  a list of event entries."
  (let (*start-expression* *stop-expression* *time-expression* *moment-expression* *ident-expression*)
    (load config-name)
    (with-open-file (s file-name :external-format :utf-8)
      (loop 
	 :with start-expr = (and *start-expression* (cl-ppcre:create-scanner *start-expression*))
	 :with stop-expr = (and *stop-expression* (cl-ppcre:create-scanner *stop-expression*))
	 :with time-expr = (and *time-expression* (cl-ppcre:create-scanner *time-expression*))
	 :with moment-expr = (and *moment-expression* (cl-ppcre:create-scanner *moment-expression*))
	 :with ident-expr = (and *ident-expression* (cl-ppcre:create-scanner *ident-expression*))
	 :with result = (list)
	 :for line = (read-line s nil nil) :while line
	 :finally (return result)
	 :for element = (list)
	 :do
	 (when (and start-expr (cl-ppcre:scan start-expr line))
	   (push :start element))
	 (when (and stop-expr (cl-ppcre:scan stop-expr line))
	   (push :stop element))
	 (when (and moment-expr (cl-ppcre:scan moment-expr line))
	   (push :moment element))
	 (when element
	   (let ((time-string (cl-ppcre:scan-to-strings time-expr line)))
	     (push (net.telent.date:parse-time time-string) element))
	   (push :ident element)
	   (push (and ident-expr (scan-match-to-string ident-expr line)) element)
	   (push (nreverse element) result))))))

(defun visualize-log (log-file-name config-file-name png-file-name)
  (draw-data (convert-simple-data (parse-file log-file-name config-file-name)) png-file-name))

;;; CONVERSION METHODS

(defun alist-element-< (a b)
  (let ((time-a (second a))
	(time-b (second b)))
    (if (= time-a time-b)
	(string< (first a) (first b))
	(< time-a time-b))))

(defun convert-simple-data (list)
  (let ((slist (sort (copy-seq list) #'alist-element-<)))
    (loop :for (type time . rest) :in slist
       :for ident = (getf rest :ident "<NONE>")
       :with start = (make-hash-table :test #'equal)
       :with result = (list)
       :finally (return result)
       :do
       (case type
	 (:start 
	  (setf (gethash ident start)  time))
	 (:stop 
	  (multiple-value-bind (start found) (gethash ident start)
	    (when found
	      (push (make-instance 'segment 
				   :ident ident
				   :start start 
				   :stop time 
				   :color (list 0.0 0.0 1.0)) 
		    result))))
	 (:moment
	  (push (make-instance 'moment 
			       :ident ident
			       :moment time 
			       :color (list 1.0 0.0 0.0))
		result))))))

