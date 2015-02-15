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