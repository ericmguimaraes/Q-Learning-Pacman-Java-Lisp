(defun ran (dir)
    ;;(print (jarray-ref-raw (jobject-lisp-value dir) 0))
    (defparameter last-dir (jobject-lisp-value dir))
	(defparameter n (random 4))
	(cond
        ((or (and (equal 0 last-dir) (equal n 2))
			(and (equal 2 last-dir) (equal n 0))
			(and (equal 1 last-dir) (equal n 3))
		    (and (equal 3 last-dir) (equal n 1)))
            (ran dir)
		)
		(t
			n
		)
	)
)

(defun string-to-list (string)
  (mapcar #'parse-integer (split-by-one-space string))
)

(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun call-java (param)
  (let* ((class (jclass "Main"))
	 (intclass (jclass "int"))
	 (method (jmethod class "addTwoNumbers" intclass intclass))
	 (result (jcall method param 2 4)))
    (format t "in void-function, result of calling addTwoNumbers(2, 4): ~a~%" result)))