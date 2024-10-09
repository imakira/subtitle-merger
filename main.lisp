(require :asdf)
(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  ;; (asdf:load-system :com.inuoe.jzon)
  (let ((*error-output* (make-broadcast-stream)))
    (asdf:load-system :alexandria)
    (asdf:load-system :arrow-macros)
    (asdf:load-system :serapeum)
    (asdf:load-system :defmain)
    (asdf:load-system :trivia)
    (asdf:load-system :cl-ppcre)))

(defpackage :subtitle-merger
  (:use :cl :alexandria :arrow-macros :trivia)
  (:shadowing-import-from :arrow-macros <>)
  (:shadowing-import-from :serapeum dict href)
  (:local-nicknames
   (:sp :serapeum)))

(in-package :subtitle-merger)

(declaim (optimize (debug 3)))
(sp:toggle-pretty-print-hash-table t)

(defun and-then (original-predicate next-predicate)
  "Returns a new predicate constructed from ORIGINAL-PREDICATE and
NEXT-PREDICATE.  The new predicate compares two elements, x and y, by
checking first with ORIGINAL-PREDICATE.  If x is less than y under
ORIGINAL-PREDICATE, then the new predicate returns true.  If y is less
than x under ORIGINAL-PREDICATE, then the new predicate returns false.
Otherwise, the new predicate compares x and y using NEXT-PREDICATE."
  (lambda (x y)
    (cond
     ((funcall original-predicate x y) t)
     ((funcall original-predicate y x) nil)
     (t (funcall next-predicate x y)))))

(defun to-tuples (subtitle-str)
  (let ((timestamps-header-regex (ppcre:create-scanner
                                  "(\\d{2}):(\\d{2}):(\\d{2})[\\.,](\\d+)\\s+-->\\s+(\\d{2}):(\\d{2}):(\\d{2})[\\.,](\\d+).*"))
        (delimiter-regex (ppcre:create-scanner "(\\r\\r|\\n\\n|\\r\\n\\r\\n)" )))
    (labels ((calculate-timestamp (h m s ms)
               (+ (* (+ (* (+ (* 60 h) m) 60) s) 1000) ms))
             (calcul (start result)
               (if start
                   (trivia:ematch
                       (multiple-value-list
                        (ppcre:scan timestamps-header-regex
                                    subtitle-str :start start))
                     ((list _ end matches-start matches-end)
                      (let* ((time-stamps (-<>> (map 'list #'list
                                                     matches-start
                                                     matches-end)
                                            (mapcar (lambda (l)
                                                      (trivia:ematch l
                                                        ((list s e)
                                                         (subseq subtitle-str s e)))))
                                            (mapcar #'parse-integer)
                                            (sp:batches <> 4)
                                            (sp:mapply #'calculate-timestamp)))
                             (delimiter (second (multiple-value-list
                                                 (ppcre:scan delimiter-regex subtitle-str :start end))))
                             (content (sp:trim-whitespace (subseq subtitle-str end delimiter))))
                        (calcul delimiter
                                (cons (dict :start (first time-stamps)
                                            :end (second time-stamps)
                                            :content content)
                                      result))))
                     (_
                      result))
                   (progn result))))
      (reverse (calcul 0 nil)))))

(defun read-file (filename)
  (read-file-into-string (uiop:ensure-pathname filename)))

(defun merge-subtitles (&rest files)
  (let ((tuples (-<>> files
                  (mapcar #'read-file)
                  (mapcar #'to-tuples)
                  (mapcan (lambda (index tuples)
                            (mapcar (lambda (tuple)
                                      (setf (sp:href tuple :id)
                                            index)
                                      tuple)
                                    tuples))
                          (iota (length files)))
                  (sp:filter (lambda (item)
                               (< (href item :start)
                                  (href item :end))))))
        (timeline (sp:vect)))
    (loop :for tuple :in tuples :do
      (vector-push-extend (dict :time (href tuple :start)
                                :type :start
                                :item tuple)
                          timeline)
      (vector-push-extend (dict :time (href tuple :end)
                                :type :end
                                :item tuple)
                          timeline))
    (setf timeline (sort timeline
                         (and-then (lambda (a b)
                                     (< (href a :time)
                                        (href b :time)))
                                   (lambda (a b)
                                     (and (equal (href a :type)
                                                 :end)
                                          (equal (href b :type)
                                                 :start))))))
    (let ((start-time 0)
          (active-items (dict))
          (result (sp:vect)))
      (loop :for fragment :across timeline :do
        (cond ((equal (href fragment :type)
                      :start)
               (when (= 0 (hash-table-count active-items))
                 (setf start-time (href fragment :time)))
               (setf (href active-items (href fragment :item))
                     t))
              (t
               (vector-push-extend (dict :start start-time
                                         :end (href fragment :time)
                                         :content
                                         (-<> (hash-table-keys active-items)
                                           (sort
                                            (lambda (a b)
                                              (< (href a :id)
                                                 (href b :id))))
                                           (mapcar (lambda (item)
                                                     (href item :content))
                                                   <>)
                                           (sp:string-join (format nil "~%"))))
                                   result)
               (setf start-time (href fragment :time))
               (remhash (href fragment :item)
                        active-items))))
      result)))

(defun millisecond->quadruple (millisecond)
  (let ((h 0)
        (m 0)
        (s 0)
        (ms 0)
        (tmp millisecond))
    (setf ms (mod tmp 1000))
    (setf tmp (floor tmp
                     1000))
    (setf s (mod tmp 60))
    (setf tmp (floor tmp
                     60))
    (setf m (mod tmp 60))
    (setf tmp (floor tmp
                     60))
    (setf h tmp)
    (list h m s ms)))

(defmethod millisecond->timestamp (millisecond (type (eql :vtt)))
  (ematch (millisecond->quadruple millisecond)
    ((list h m s ms)
     (format nil "~2,'0d:~2,'0d:~2,'0d.~d" h m s ms))))

(defmethod millisecond->timestamp (millisecond (type (eql :srt)))
  (ematch (millisecond->quadruple millisecond)
    ((list h m s ms)
     (format nil "~2,'0d:~2,'0d:~2,'0d,~d" h m s ms))))

(defmethod format-to-string (subtitle-vector (type (eql :vtt)))
  (-<>> subtitle-vector
    (map 'vector (lambda (item)
                   (format nil "~A --> ~A~%~A"
                           (millisecond->timestamp (href item :start) type)
                           (millisecond->timestamp (href item :end) type)
                           (href item :content))))
    (sp:string-join <>
                    (format nil "~%~%"))
    (sp:concat (format nil "WEBVTT~%~%"))))

(defmethod format-to-string (subtitle-vector (type (eql :srt)))
  (-<>> subtitle-vector
    (map 'vector
         (lambda (index item)
           (format nil "~A~%~A --> ~A~%~A"
                   index
                   (millisecond->timestamp (href item :start) type)
                   (millisecond->timestamp (href item :end) type)
                   (href item :content)))
         (sp:range 1 (+ 1 (length subtitle-vector))))
    (sp:string-join <>
                    (format nil "~%~%"))))

(defmain:defmain (main) ((output "output file, suffix with .vtt or .srt is supported"
                                 :short "o")
                         &rest rest)
  (let* ((unspecific-suffix? (equal (pathname-type (uiop:ensure-pathname output))
                                    :unspecific))
         (type (cond ((ends-with-subseq ".vtt" output)
                      :vtt)
                     ((ends-with-subseq ".srt" output)
                      :srt)
                     (unspecific-suffix?
                      :srt)
                     (t
                      (error (sp:concat "suffix " (pathname-type (uiop:ensure-pathname output)) " is not supported"))))))
    (write-string-into-file (format-to-string (apply #'merge-subtitles rest)
                                              type)
                            (sp:concat output
                                       (if unspecific-suffix?
                                           ".vtt"
                                           ""))
                            :if-exists :overwrite
                            :if-does-not-exist :create)))

(unless (or #+:swank t
            #+:slynk t
            nil)
  (apply #'main uiop:*command-line-arguments*))
