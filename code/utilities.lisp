(defpackage :rss-utilities
  (:use :common-lisp :ccl)
  (:nicknames :ut))

(in-package :ut)

(defun center-view (view)
  "Centers the view in its container, or on the screen if it has no container;
   reduces view-size if needed to fit on screen."
  (let* ((container (view-container view))
         (max-v (if container
                  (point-v (view-size container))
                  (- *screen-height* *menubar-bottom*)))
         (max-h (if container
                  (point-h (view-size container))
                  *screen-width*))
         (v-size (min max-v (point-v (view-size view))))
         (h-size (min max-h (point-h (view-size view)))))
    (set-view-size view h-size v-size)
    (set-view-position view
                       (/ (- max-h h-size) 2)
                       (+ *menubar-bottom* (/ (- max-v v-size) 2)))))
(export 'center-view)

(defmacro square (x)
  `(if (> (abs ,x) 1e10) 1e20 (* ,x ,x)))
(export 'square)

(defun with-probability (p &optional (state *random-state*))
  (> p (random 1.0 state)))
(export 'with-probability)

(defun with-prob (p x y &optional (random-state *random-state*))
  (if (< (random 1.0 random-state) p)
      x
      y))
(export 'with-prob)

(defun random-exponential (tau &optional (state *random-state*))
  (- (* tau
        (log (- 1
                (random 1.0 state))))))
(export 'random-exponential)

(defun random-normal (&optional (random-state cl::*random-state*))
  (do ((u 0.0)
       (v 0.0))
      ((progn
         (setq u (random 1.0 random-state)	; U is bounded (0 1)
               v (* 2.0 (sqrt 2.0) (exp -0.5)	; V is bounded (-MAX MAX)
                    (- (random 1.0 random-state) 0.5)))
         (<= (* v v) (* -4.0 u u (log u))))	; < should be <=
       (/ v u))
    (declare (float u v))))
(export 'random-normal)

;stats

(defun mean (l)
  (float
    (/ (loop for i in l sum i)
       (length l))))
(export 'mean)

(defun mse (target values)
      (mean (loop for v in values collect (square (- v target)))))
(export 'mse)

(defun rmse (target values)			;root mean square error
  (sqrt (mse target values))) (export 'rmse)
(export 'rmse)

(defun stdev (l)
  (rmse (mean l) l))
(export 'stdev)

(defun stats (list)
  (list (mean list) (stdev list)))
(export 'stats)

(defun multi-stats (list-of-lists)
  (loop for list in (reorder-list-of-lists list-of-lists)
        collect (stats list)))
(export 'multi-stats)

(defun multi-mean (list-of-lists)
  (loop for list in (reorder-list-of-lists list-of-lists)
        collect (mean list)))
(export 'multi-mean)

(defun logistic (s)
  (/ 1.0 (+ 1.0 (exp (max -20 (min 20 (- s)))))))
(export 'logistic)

(defun reorder-list-of-lists (list-of-lists)
  (loop for n from 0 below (length (first list-of-lists))
        collect (loop for list in list-of-lists collect (nth n list))))
(export 'reorder-list-of-lists)

(defun flatten (list)
  (if (null list)
      (list)
      (if (atom (car list))
          (cons (car list) (flatten (cdr list)))
          (flatten (append (car list) (cdr list))))))
(export 'flatten)


(defun interpolate (x fs xs)
  "Uses linear interpolation to estimate f(x), where fs and xs are lists of corresponding
   values (f's) and inputs (x's).  The x's must be in increasing order."
  (if (< x (first xs))
      (first fs)
      (loop for last-x in xs
            for next-x in (rest xs)
            for last-f in fs
            for next-f in (rest fs)
            until (< x next-x)
            finally (return (if (< x next-x)
                                (+ last-f
                                   (* (- next-f last-f)
                                      (/ (- x last-x)
                                         (- next-x last-x))))
                                next-f)))))
(export 'interpolate)


(defun normal-distribution-function (x mean standard-deviation)
  "Returns the probability with which a normally distributed random number with the given
   mean and standard deviation will be less than x."
  (let ((fs '(.5 .5398 .5793 .6179 .6554 .6915 .7257 .7580 .7881 .8159 .8413 .8643 .8849
              .9032 .9192 .9332 .9452 .9554 .9641 .9713 .9772 .9938 .9987 .9998 1.0))
        (xs '(0 .1 .2 .3 .4 .5 .6 .7 .8 .9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0
                2.5 3.0 3.6 100.0))
        (z (if (= 0 standard-deviation)
               1e10
               (/ (- x mean) standard-deviation))))
    (if (> z 0)
        (interpolate z fs xs)
        (- 1.0 (interpolate (- z) fs xs)))))
(export 'normal-distribution-function)

(defconstant +sqrt-2-PI (sqrt (* 2 3.1415926)) "Square root of 2 PI")

(defun normal-density (z)
  "Returns value of the normal density function at z; mean assumed 0, sd 1"
  (/ (exp (- (* .5 (square (max -20 (min 20 z))))))
     +sqrt-2-PI))
(export 'normal-density)

(defun poisson (n lambda)
  "The probability of n events according to the poisson distribution"
  (* (exp (- lambda))
     (/ (expt lambda n)
        (factorial n))))
(export 'poisson)

(defun factorial (n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))
(export 'factorial)

(defun q (&rest ignore)
  (declare (ignore ignore))
  (values))			;evaluates it's arg and returns nothing
(export 'q)

(defmacro swap (x y)
  (let ((var (gensym)))
    `(let ((,var ,x))
       (setf ,x ,y)
       (setf ,y ,var))))
(export 'swap)

(defmacro setq-list (list-of-vars list-of-values-form)
  (append (list 'let (list (list 'list-of-values list-of-values-form)))
          (loop for var in list-of-vars
                for n from 0 by 1
                collect (list 'setf var (list 'nth n 'list-of-values)))))
(export 'setq-list)

(defmacro bound (x limit)
  `(setf ,x (max (- ,limit) (min ,limit ,x))))
(export 'bound)

(defmacro limit (x limit)
  `(max (- ,limit) (min ,limit ,x)))
(export 'limit)

(defvar *z-alphas* '((2.33 .01) (1.645 .05) (1.28 .1)))
(defmacro z-alpha (za) `(first ,za))
(defmacro z-level (za) `(second ,za))
(defun z-test (mean1 stdev1 size1 mean2 stdev2 size2)
  (let* ((stdev (sqrt (+ (/ (* stdev1 stdev1) size1)
                         (/ (* stdev2 stdev2) size2))))
         (z (/ (- mean1 mean2) stdev)))
        (dolist (za *z-alphas*)
                (when (> (abs z) (z-alpha za))
                      (return-from z-test (* (signum z) (z-level za)))))
        0.0))
(export 'z-test)

;; STRUCTURE OF A SAMPLE
(defmacro s-name  (sample) `(first ,sample))
(defmacro s-mean  (sample) `(second ,sample))
(defmacro s-stdev (sample) `(third ,sample))
(defmacro s-size  (sample) `(fourth ,sample))

(defun z-tests (samples)
  (mapcar #'(lambda (sample) (z-tests* sample samples)) samples))
(defun z-tests* (s1 samples)
  `(,(s-name s1)
    ,@(mapcar #'(lambda (s2)
                  (let ((z (z-test (s-mean s1) (s-stdev s1) (s-size s1)
                                   (s-mean s2) (s-stdev s2) (s-size s2))))
                       `(,(if (minusp z) '>
                              (if (plusp z) '< '=))
                         ,(s-name s2) ,(abs z))))
          samples)))
(export 'z-tests)


(export 'point-lineseg-distance)
(defun point-lineseg-distance (x y x1 y1 x2 y2)
 "Returns the euclidean distance between a point and a line segment"
 ; In the following, all variables labeled dist's are SQUARES of distances.
 ; The only tricky part here is figuring out whether to use the distance
 ; to the nearest point or the distance to the line defined by the line segment.
 ; This all depends on the angles (the ones touching the lineseg) of the triangle
 ; formed by the three points.  If the larger is obtuse we use nearest point,
 ; otherwise point-line.  We check for the angle being greater or less than
 ; 90 degrees with the famous right-triangle equality A^2 = B^2 + c^2.
 (let ((near-point-dist (point-point-distance-squared x y x1 y1))
       (far-point-dist (point-point-distance-squared x y x2 y2))
       (lineseg-dist (point-point-distance-squared x1 y1 x2 y2)))
   (if (< far-point-dist near-point-dist)
       (swap far-point-dist near-point-dist))
   (if (>= far-point-dist
           (+ near-point-dist lineseg-dist))
       (sqrt near-point-dist)
       (point-line-distance x y x1 y1 x2 y2))))

(export 'point-line-distance)
(defun point-line-distance (x y x1 y1 x2 y2)
  "Returns the euclidean distance between the first point and the line given by the other two points"
  (if (= x1 x2)
      (abs (- x1 x))
      (let* ((slope (/ (- y2 y1)
                       (float (- x2 x1))))
             (intercept (- y1 (* slope
                                 x1))))
        (/ (abs (+ (* slope x)
                   (- y)
                   intercept))
           (sqrt (+ 1 (* slope slope)))))))

(export 'point-point-distance-squared)
(defun point-point-distance-squared (x1 y1 x2 y2)
  "Returns the square of the euclidean distance between two points"
  (+ (square (- x1 x2))
     (square (- y1 y2))))

(export 'point-point-distance)
(defun point-point-distance (x1 y1 x2 y2)
  "Returns the euclidean distance between two points"
  (sqrt (point-point-distance-squared x1 y1 x2 y2)))

(defun lv (vector) (loop for i below (length vector) collect (aref vector i)))

(defun l1 (vector)
  (lv vector))

(defun l2 (array)
  (loop for k below (array-dimension array 0) do
    (print (loop for j below (array-dimension array 1) collect (aref array k j))))
  (values))

(export 'l)
(defun l (array)
  (if (= 1 (array-rank array))
      (l1 array)
      (l2 array)))

(export 'subsample)
(defun subsample (bin-size l)
  "l is a list OR a list of lists"
  (if (listp (first l))
      (loop for list in l collect (subsample list bin-size))
      (loop while l
            for bin = (loop repeat bin-size while l collect (pop l))
            collect (mean bin))))

(export 'copy-of-standard-random-state)
(defun copy-of-standard-random-state ()
  (make-random-state #.(ccl::random-mrg31k3p-state)))

(export 'permanent-data)
(export 'permanent-record-file)
(export 'record-fields)
(export 'record)
(export 'read-record-file)
(export 'record-value)
(export 'records)
(export 'my-time-stamp)
(export 'prepare-for-recording!)
(export 'prepare-for-recording)

(defvar permanent-data nil)
(defvar permanent-record-file nil)
(defvar record-fields '(:day :hour :min :alpha :data))

(defun prepare-for-recording! (file-name &rest data-fields)
  (setq permanent-record-file file-name)
  (setq permanent-data nil)
  (setq record-fields (append '(:day :hour :min) data-fields))
  (with-open-file (file file-name
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~A~%" (apply #'concatenate 'string "(:record-fields"
                               (append (loop for f in record-fields collect
                                             (concatenate 'string " :"
                                                          (format nil "~A" f)))
                                       (list ")"))))))

(defun record (&rest record-data)
  "Record data with time stamp in file and permanent-data"
  (let ((record (append (my-time-stamp) record-data)))
    (unless (= (length record) (length record-fields))
      (error "data does not match template "))
    (when permanent-record-file
      (with-open-file (file permanent-record-file
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
        (format file "~A~%" record)))
    (push record permanent-data)
    record))

(defun read-record-file (&optional (file (choose-file-dialog)))
  "Load permanent-data from file"
  (with-open-file (file file :direction :input)
    (setq permanent-data
          (reverse (let ((first-read (read file nil nil))
                         (rest-read (loop for record = (read file nil nil)
                                          while record collect record)))
                     (cond ((null first-read))
                           ((eq (car first-read) :record-fields)
                            (setq record-fields (rest first-read))
                            rest-read)
                           (t (cons first-read rest-read))))))
    (setq permanent-record-file file)
    (cons (length permanent-data) record-fields)))

(defun record-value (record field)
  "extract the value of a particular field of a record"
  (unless (member field record-fields) (error "Bad field name"))
  (loop for f in record-fields
        for v in record
        until (eq f field)
        finally (return v)))

(defun records (&rest field-value-pairs)
  "extract all records from data that match the field-value pairs"
  (unless (evenp (length field-value-pairs)) (error "odd number of args to records"))
  (loop for f-v-list = field-value-pairs then (cddr f-v-list)
        while f-v-list
        for f = (first f-v-list)
        unless (member f record-fields) do (error "Bad field name"))
  (loop for record in (reverse permanent-data)
        when (loop for f-v-list = field-value-pairs then (cddr f-v-list)
                   while f-v-list
                   for f = (first f-v-list)
                   for v = (second f-v-list)
                   always (OR (equal v (record-value record f))
                              (ignore-errors (= v (record-value record f)))))
        collect record))

(defun my-time-stamp ()
  (multiple-value-bind (sec min hour day) (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (list day hour min)))


;; For writing a list to a file for input to Cricket-Graph

(export 'write-for-graphing)

(defun write-for-graphing (data)
  (with-open-file (file "Macintosh HD:Desktop Folder:temp-graphing-data"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (if (atom (first data))
      (loop for d in data do (format file "~8,4F~%" d))
      (loop with num-rows = (length (first data))
            for row below num-rows
            do (loop for list in data do (format file "~8,4F	" (nth row list)))
            do (format file "~%")))))



(export 'standard-random-state)
(export 'standardize-random-state)
(export 'advance-random-state)

(defvar standard-random-state #.(ccl::random-mrg31k3p-state))
#|
        #S(FUTURE-COMMON-LISP:RANDOM-STATE
            :ARRAY
            #(1323496585 1001191002 -587767537 -1071730568 -1147853915 -731089434 1865874377 -387582935
                         -1548911375 -52859678 1489907255 226907840 -1801820277 145270258 -1784780698 895203347
                         2101883890 756363165 -2047410492 1182268120 -1417582076 -2101366199 -436910048 92474021
                         -850512131 -40946116 -723207257 429572592 -262857859 1972410780 -828461337 154333198
                         -2110101118 -1646877073 -1259707441 972398391 1375765096 240797851 -1042450772 -257783169
                         -1922575120 1037722597 -1774511059 1408209885 -1035031755 2143021556 785694559 1785244199
                         -586057545 216629327 -370552912 441425683 803899475 -122403238 -2071490833 679238967
                         1666337352 984812380 501833545 1010617864 -1990258125 -1465744262 869839181 -634081314
                         254104851 -129645892 -1542655512 1765669869 -1055430844 -1069176569 -1400149912)
            :SIZE 71 :SEED 224772007 :POINTER-1 0 :POINTER-2 35))
|#
(defmacro standardize-random-state (&optional (random-state 'cl::*random-state*))
  `(setq ,random-state (make-random-state ut:standard-random-state)))

(defun advance-random-state (num-advances &optional (random-state *random-state*))
  (loop repeat num-advances do (random 2 random-state)))

(export 'firstn)
(defun firstn (n list)
  "Returns a list of the first n elements of list"
  (loop for e in list
        repeat n
        collect e))
