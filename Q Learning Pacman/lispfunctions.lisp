(defparameter javaobj 0)

;; JAVA-CONNECTION Functions
(defun init(param)
  (setf javaobj param)
)

(defun call-java (javamethod str)
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (strclass (jclass "java.lang.String"))
	 (method (jmethod class javamethod strclass))
	 (result (jcall method javaobj str)))
    result))

(defun string-to-list (string)
  (mapcar #'parse-integer (split-by-one-space string))
)

(defun list-to-string (lst)
    (format nil "~{~A~^,~}" lst)
)

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; Other functions

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

(defun test()
  (call-java "test" (list-to-string '(1 2 3 4)))
)

;;Qlearning

(defparameter epsilon 0.05) ;; learning rate
(defparameter gamma 0.8);; exploration rate
(defparameter alpha 0.2);; discount factor
(defparameter weights '())


;;Compute the action to take in the current state.  With
;;probability epsilon, we should take a random action and
;;take the best policy action otherwise.  Note that if there are
;;no legal actions the return is nil
(defun get-action(state)
  (if (flip-coin (epsilon)) 
    (get-random-action(get-list-of-actions(state))) 
    (compute-action-from-qvalues(state))
  )
)

;;update your weights based on transition
(defun update()
  
)

;;return Q(state,action) = w * featureVector
(defun get-qvalue(state, action)
  (apply '+ (mapcar #'* weights (get-features(state, action))))
)

(defun flip-coin (e)
  (< (random 100) e*100) 
)

(defun get-random-action(actions)
  (nth (random (lenght actions)) actions)
)

(defun compute-action-from-qvalues (state)
  (setf actions (get-list-of-actions(state)))
  (setf best-action '())
  ;;choose best action based in the getQValue(state, action)
  ;;TODO
)




