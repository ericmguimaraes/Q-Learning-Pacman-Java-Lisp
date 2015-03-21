(defparameter javaobj 0)

;; JAVA-CONNECTION Functions
(defun init(param)
  ;;(print "init")
  (setf javaobj param)
)

(defun call-java (javamethod)
  ;;(print "call-java")
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (method (jmethod class javamethod))
	 (result (jcall method javaobj)))
    result))

(defun call-java-param-str (javamethod str)
  ;;(print "call-java-param")
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (strclass (jclass "java.lang.String"))
	 (method (jmethod class javamethod strclass))
	 (result (jcall method javaobj str)))
    result))

(defun call-java-param-int (javamethod str)
  ;;(print "call-java-param-int")
  (let* ((class (jclass "eguimaraes.qlearning.pacman.LispFunction"))
	 (intclass (jclass "int"))
	 (method (jmethod class javamethod intclass))
	 (result (jcall method javaobj str)))
    result))

(defun string-to-list (string)
  ;;(print "string-to-list")
  (mapcar #'parse-integer (split-by-one-space string))
)

(defun list-to-string (lst)
  ;;(print "list-to-string")  
  (format nil "~{~A~^,~}" lst)
)

(defun split-by-one-space (string)
  ;;(print "split-by-one-space")
  ;;(print string)  
  (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;; Other functions

(defun get-random-action (last-dir)
    ;;(print (jarray-ref-raw (jobject-lisp-value dir) 0))
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

(defparameter epsilon 0.05) ;; exploration rate
(defparameter gamma 0.5);; discount factor
(defparameter alpha 0.2);; learning rate
(defparameter weights '(1 1 1 1 1 1))


;;Compute the action to take in the current state.  With
;;probability epsilon, we should take a random action and
;;take the best policy action otherwise.  Note that if there are
;;no legal actions the return is nil
(defun get-action()
 ;; (print "get-action")
  ;;(print "Weights: ")
  (if (flip-coin epsilon) 
    (get-random-action (get-last-dir))
    (compute-action-from-qvalues)
  )
)

;;choose best action based in the getQValue(state, action)
(defun compute-action-from-qvalues ()
  (print "compute-action-from-qvalues")
  (setf actions (get-legal-actions))
  (setf best-action (nth 0 actions))
  (setf q (get-q-value-from-action (nth 0 actions)))
  ;;(print q)
  (loop for action in actions do (if (>= (get-q-value-from-action action) q) (progn 
                                          (setf best-action action) 
                                          (setf q (get-q-value-from-action action))
                                         )))
  ;;(print "Action Computed")
  best-action
)

;;update your weights based on transition
;;for each feature and weight
;;difference = (reward + gamma  *maxQ) - self.getQValue(state, action)
;;weights[feature] = weights[feature] + self.alpha * difference * features[feature]
(defun update(state action next-state reward)
  (setf state (string-to-list (jobject-lisp-value state)))
  (setf action (jobject-lisp-value action))
  (setf next-state (string-to-list (jobject-lisp-value next-state)))
  (setf reward (jobject-lisp-value reward))
  (print "update")
  ;;(print state)
  ;;(print reward)
  ;;(print next-state)
  (setf max-q (get-qmax))
  (print max-q)
  (setf difference (- (+ reward (* gamma max-q)) (get-q-value-from-features state)))
  (print difference)
  (setf weights (mapcar #'(lambda (w f) (+ w (* alpha difference f))) weights state))
  #|
  (loop for i from 0 to (length weights) do(progn(
       (setf wi (nth i weights))
       (setf fi (nth i state))
       (setf wei (+ wi (* alpha difference fi)))
       (print wi) (print fi) (print wei)                                           
       (setf new-weights (cons wei))
   )))
   (setf weights new-weights) |#
  (print weights)
  ;;(print "weights updated")
)

;;return Q(state,action) = w * featureVector
(defun get-q-value-from-action(action)
 ;; (print "get-q-value-from-action")
  (apply '+ (mapcar #'* weights (get-features-act action)))
)

(defun get-q-value-from-features(state)
 ;; (print "get-q-value-from-features")
  (apply '+ (mapcar #'* weights state))
)

(defun get-qmax ()
;;  (print "get-qmax")
  (setf actions (get-legal-actions))'
 ;; (print actions)
  (setf best-action (nth 0 actions))
  (setf q (get-q-value-from-action (nth 0 actions)))
  (loop for action in actions do (if (> (get-q-value-from-action action) q) (progn 
                                        ;;  (print "entrou no if")                                     
                                          (setf best-action action) 
                                          (setf q (get-q-value-from-action action))
                                         )))
  q
)

(defun flip-coin (e)
 ;; (print "flip-coin")
  (< (random 100) (* e 100)) 
)

(defun get-features ()
 ;; (print "get-features")
  (string-to-list(call-java "getFeatures"))
)

(defun get-features-act (action)
 ;; (print "get-features-act")
  (string-to-list (call-java-param-int "getFeatures" action))
)

(defun get-legal-actions ()
  ;;(print "get-legal-actions")
  (string-to-list (call-java "getActions"))
)

(defun get-last-dir ()
 ;; (print "get-last-dir")
  (jobject-lisp-value (call-java "getLastDir"))
)


