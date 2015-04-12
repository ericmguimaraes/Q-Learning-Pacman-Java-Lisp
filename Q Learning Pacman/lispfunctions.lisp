(defparameter javaobj 0)

;; JAVA-CONNECTION Functions
(defun init(param)
  (setf javaobj param)
)

(defun call-java (javamethod)
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (method (jmethod class javamethod))
	 (result (jcall method javaobj)))
    result))

(defun call-java-param-str (javamethod str)
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (strclass (jclass "java.lang.String"))
	 (method (jmethod class javamethod strclass))
	 (result (jcall method javaobj str)))
    result))

(defun call-java-param-int (javamethod str)
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (intclass (jclass "int"))
	 (method (jmethod class javamethod intclass))
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

(defun get-random-action (last-dir)
	(print "get-random-action")
  (defparameter n (random 4))
	(cond
        ((or (and (equalp 0 last-dir) (equalp n 2))
			(and (equalp 2 last-dir) (equalp n 0))
			(and (equalp 1 last-dir) (equalp n 3))
		    (and (equalp 3 last-dir) (equalp n 1)))
            (get-random-action last-dir)
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

(defparameter epsilon 0) ;; exploration rate
(defparameter gamma 0.1);; discount factor
(defparameter alpha 0.1);; learning rate
(defparameter weights '(0 0 0 0 0 0 0))
(defparameter count 0)


;;Compute the action to take in the current state.  With
;;probability epsilon, we should take a random action and
;;take the best policy action otherwise.  Note that if there are
;;no legal actions the return is nil
(defun get-action()
  ;;(setf count (+ count 1))
  ;;(setf epsilon (/ 1 (/ count 2)))
  (if (flip-coin epsilon) 
    (get-random-action (get-last-dir))
    (compute-action-from-qvalues)
  )
)

;;choose best action based in the getQValue(state, action)
(defun compute-action-from-qvalues ()
  (setf actions (get-legal-actions))
  (setf best-action (nth 0 actions))
  (setf q (get-q-value-from-action (nth 0 actions)))
  (loop for action in actions do (if (>= (get-q-value-from-action action) q) (progn 
                                          (setf best-action action) 
                                          (setf q (get-q-value-from-action action))
                                         )))
  (setf best-actions (cons best-action '()))
  (loop for action in actions do (if (and (equal (get-q-value-from-action action) q) (not (equal action best-action))) 
                                    (setf best-actions (cons action best-actions))))
  (if (equal 1 (length best-actions)) 
    (nth 0 best-actions) 
    (nth (random (length best-actions)) best-actions))
)

;;update your weights based on transition
;;for each feature and weight
;;difference = (reward + gamma  *maxQ) - self.getQValue(state, action)
;;weights[feature] = weights[feature] + self.alpha * difference * features[feature]
(defun update(state action next-state reward)
  (setf n 100000)
  (setf state (mapcar #'(lambda (x) (/ x n)) (string-to-list (jobject-lisp-value state))))
  (setf action (jobject-lisp-value action))
  (setf next-state (mapcar #'(lambda (x) (/ x n)) (string-to-list (jobject-lisp-value next-state))))
  (setf reward (jobject-lisp-value reward))
  (setf feat '(dotDist numGhost dot PowerDot eatGhost GhostDist beEaten))
  (print (mapcar #'cons feat state))
  (print reward)
  ;;(print (mapcar #'cons feat next-state))
  (setf max-q (get-qmax))
  (setf difference (- (+ reward (* gamma max-q)) (get-q-value-from-features state)))
  (setf weights (mapcar #'(lambda (w f) (+ w (* alpha difference f))) weights state))
  (print (mapcar #'cons feat weights))
)

;;return Q(state,action) = w * featureVector
(defun get-q-value-from-action(action)
  (apply '+ (mapcar #'* weights (get-features-act action)))
)

(defun get-q-value-from-features(state)
  (apply '+ (mapcar #'* weights state))
)

(defun get-qmax ()
  (setf actions (get-legal-actions))'
  (setf best-action (nth 0 actions))
  (setf q (get-q-value-from-action (nth 0 actions)))
  (loop for action in actions do (if (> (get-q-value-from-action action) q) (progn 
                                          (setf best-action action) 
                                          (setf q (get-q-value-from-action action))
                                         )))
  q
)

(defun flip-coin (e)
  (< (random 100) (* e 100)) 
)

(defun get-features ()
  (mapcar #'(lambda (x) (/ x 100000)) (string-to-list (call-java "getFeatures")))
)

(defun get-features-act (action)
(setf n 100000)
  (mapcar #'(lambda (x) (/ x 100000)) (string-to-list (call-java-param-int "getFeatures" action)))
)

(defun get-legal-actions ()
  (string-to-list (call-java "getActions"))
)

(defun get-last-dir ()
  (jobject-lisp-value (call-java "getLastDir"))
)