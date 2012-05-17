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

(defmethod move)

(defclass segment ()
  ((start :accessor start :initarg :start)
   (stop :accessor stop :initarg :stop)
   (color :accessor color :initarg :color)))

(defmethod x-point ((value integer) (area draw-area))
  (with-slots (canvas-width min-time max-time) area
    (* canvas-width (/ (- value min-time) (- max-time min-time)))))

(defmethod set-stroke-color ((segment segment))
  (apply #'set-rgb-stroke (color segment)))

(defmethod draw-at-height ((segment segment) (height number) (area draw-area))
  (move-to (x-point (start segment) area) height)
  (line-to (x-point (stop segment) area) height)
  (set-stroke-color segment)
  (stroke))


(defun make-test-data ()
  (list 
   (make-instance 'segment :start 10 :stop 20 :color '(1.0 0.0 0.0))
   (make-instance 'segment :start 18 :stop 40 :color '(0.0 1.0 0.0))))

(defun configure-area-from-data (area data)
  (loop :for segment :in data
     :minimizing (start segment) :into min-start
     :maximizing (stop segment) :into max-end
     :finally (progn (setf (min-time area) min-start) (setf (max-time area) max-end))))


(defun draw-data (data file-name &key (width 2000) (height (length data)))
  "Draws the data to the png with file-name"
  (with-canvas (:width width :height height)
    (set-rgb-fill 1.0 1.0 1.0)
    (rectangle 0 0 width height)
    (fill-path)
    (let ((area (make-instance 'draw-area :canvas-height height :canvas-width width)))
      (configure-area-from-data area data)
      (loop :for element :in data 
	 :for height :from 0.5 :by 1
	 :do
	 (draw-at-height element height area)))
    (save-png file-name)))
       


(defparameter *start-expression* "going to sleep$")
(defparameter *stop-expression* "waking up$")
(defparameter *time-expression* ".{15}")

(defun parse-file (file-name config-name)
  (let (*start-expression* *stop-expression* *time-expression*)
    (load config-name)
    (with-open-file (s file-name :external-format :utf-8)
      (loop :for line = (read-line s nil nil)
	 :while line
	 :with result = (list)
	 :finally (return result)
	 :do
	 (when (cl-ppcre:scan *start-expression* line)
	   (let ((time-string (cl-ppcre:scan-to-strings *time-expression* line)))
	     (push (list :start (net.telent.date:parse-time time-string)) result)))
	 (when (cl-ppcre:scan *stop-expression* line)
	   (let ((time-string (cl-ppcre:scan-to-strings *time-expression* line)))
	     (push (list :stop (net.telent.date:parse-time time-string)) result)))))))


(defun parse-sleep-file (file-name)
  (with-open-file (s file-name :external-format :utf-8)
    (loop :for line = (read-line s nil nil)
       :while line
       :with result = (list)
       :finally (return result)
       :do
       (when (cl-ppcre:scan *start-expression* line)
	 (let ((time-string (cl-ppcre:scan-to-strings *time-expression* line)))
	   (push (list :start (net.telent.date:parse-time time-string)) result)))
       (when (cl-ppcre:scan *stop-expression* line)
	 (let ((time-string (cl-ppcre:scan-to-strings *time-expression* line)))
	   (push (list :stop (net.telent.date:parse-time time-string)) result)))
       )))


(defun convert-simple-data (list)
  (let ((slist (sort (copy-seq list) #'< :key #'second)))
    (loop :for (type time) :in slist
       :with start = nil
       :with stop = nil
       :with result = (list)
       :finally (return result)
       :do
       (case type
	 (:start 
	  (when stop
	    (push (make-instance 'segment :start stop :stop time :color (list 1.0 0.0 0.0)) result))
	  (setf start time))
	 (:stop (when start
		  (push (make-instance 'segment :start start :stop time :color (list 0.0 0.0 1.0)) result))
		(setf stop time))))))
