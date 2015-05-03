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

(defun get-random-action-aux (last-dir)
  (get-random-action (jobject-lisp-value last-dir))
 )

(defun get-random-action (last-dir)
	;;(print "get-random-action")
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
(defparameter gamma 0.01);; discount factor
(defparameter alpha 0.01);; learning rate
(defparameter weights '(0 0 0 0 0 0 0))
(defparameter count 0)


;;Compute the action to take in the current state.  With
;;probability epsilon, we should take a random action and
;;take the best policy action otherwise.  Note that if there are
;;no legal actions the return is nil
(defun get-action()
 (review-alpha)
 (if (flip-coin epsilon) 
    (get-random-action (get-last-dir))
    (compute-action-from-qvalues)
 )
)

(defun review-alpha()
  (setf count (+ count 1))
  ;(if (> count 60) (setf alpha 0.1))
  ;(if (> count 300) (setf alpha 0.05))
 ; (if (> count 400) (setf alpha 0.01))
  ;(if (> count 500) (setf alpha 0.001))
  (if (> count 1500) (setf alpha 0.0001)) 
  (if (eq count 1) (setf alpha 0.01))
 ; (print alpha)
  ;(print count)
)

;;choose best action based in the getQValue(state, action)
(defun compute-action-from-qvalues ()
  (setf actions (get-legal-actions))
  (setf best-action (nth 0 actions))
  (setf q (get-q-value-from-action (nth 0 actions)))
 (print "*********1************")
  (loop for action in actions do (progn
                                   (print action)
                                   (print (get-q-value-from-action action))
                                   (setf feat '(1/dotDist numGhost dot PowerDot eatGhost GhostDist beEaten))
                                    (print (mapcar #'cons feat (get-features-act action)))
                                   ))
  (print "*********2************")
                                   
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
  (setf feat '(1/dotDist numGhost dot PowerDot eatGhost GhostDist beEaten))
;  (print (mapcar #'cons feat state))
;  (print reward)
  ;;(print (mapcar #'cons feat next-state))
  (setf max-q (get-qmax))
  (setf difference (- (+ reward (* gamma max-q)) (get-q-value-from-features state)))
  (setf weights (mapcar #'(lambda (w f) (+ w (* alpha difference f))) weights state))
  (print (mapcar #'cons feat weights))
)

;;return Q(state,action) = w * featureVector
(defun get-q-value-from-action(action)
  ;(print "get-q-value-from-action")
  ;(print (apply '+ (mapcar #'float (mapcar #'* (mapcar #'float weights) (mapcar #'float (get-features-act action))))))
 ; (setf features (get-features-act action))
;  (setf sum 0)
  ;(loop 
      
	;	for f in features
	;	for w in weights do (progn
          ;              (print "feature embaixo:")
            ;            (print f)
            ;            (print "weight embaixo:")
               ;         (print w)
                 ;       (setf sum (+ sum (* w f)))))
  (apply '+ (mapcar #'* weights (get-features-act action)))
 ; sum
  
)

(defun get-q-value-from-features(state)
  ;(print "get-q-value-from-features")
 ; (print (apply '+ (mapcar #'* weights state)))
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
  (mapcar #'(lambda (x) (/ x 100000)) (string-to-list (call-java-param-int "getFeatures" action)))
)

(defun get-legal-actions ()
  (string-to-list (call-java "getActions"))
)

(defun get-last-dir ()
  (jobject-lisp-value (call-java "getLastDir"))
)


;;Statistics

(defun write-in-file(string file append)
  (with-open-file (str (concatenate 'string "./output/" file)
                     :direction :output
                     :if-exists (if append :append :supersede)
                     :if-does-not-exist :create)
  (format str string)
  (format str "~%"))
)

(defun save-data(string)
  (write-in-file (jobject-lisp-value string) "data.txt" t)
)


;;mode+" "+triesCounter+" "+level+" "+score;
(defun calc-save-stats()
	(setf stats nil)
	(with-open-file (stream "./output/data.txt"
		:direction :INPUT
		:if-does-not-exist nil)
		(if stream (progn
			(setf line (get-line stream))
			(loop while (not (null line)) do (progn
				(setf mode (nth 0 line))
				(setf tries (nth 1 line))
				(if (null stats) 
				(setf n nil)
				(setf n (find-mode-tries-match stats mode tries)))
				(if n ;;find-mode-tries-match
					(progn
					(setf s (nth n stats))
					(setf (stats-mode s) mode)
					(setf (stats-tries s) tries)
					(setf (stats-level s) (+ (stats-level s) (nth 2 line)))
					(setf (stats-score s) (+ (stats-score s) (nth 3 line)))
					(setf (stats-count s) (+ (stats-count s) 1))
					) 
					(setf stats (cons (make-instance 'statistic :mode mode ;;else
					:tries tries :level (nth 2 line) 
					:score (nth 3 line) :count 1) 
					stats))
				)
			(setf line (get-line stream))                              
			))
			;;(print stats)
			;;computing averages
			(setf result "")
			(setf graph-result "")
			(loop for st in stats do (progn
				;;(setf (stats-level st) (/ (stats-level st) (stats-count st)))
				;;(setf (stats-score st) (/ (stats-score st) (stats-count st)))
				(setf result (concatenate 'string result (stats-to-string st) "~%"))
				(setf graph-result (concatenate 'string graph-result (stats-to-string-graph-file st) "~%"))
			))
			(write-in-file result "statistics.txt" nil)
			(write-in-file graph-result "graph-file-statistics.txt" nil)
		))
	)	
)

(defun stats-to-string (st)
  (concatenate 'string "Mode: " (get-mode-string (slot-value st 'mode)) 
    " Try: " (write-to-string (slot-value st 'tries))
    " AVG Level: " (write-to-string (float (/ (slot-value st 'level) (slot-value st 'count))))
    " Rounds: " (write-to-string (slot-value st 'count))
	" AVG Score: " (write-to-string (float (/ (slot-value st 'score) (slot-value st 'count))))
	)
)

(defun stats-to-string-graph-file (st)
  (concatenate 'string (get-mode-string (slot-value st 'mode)) 
    ";" (write-to-string (slot-value st 'tries))
    ";" (write-to-string (float (slot-value st 'level))) 
    ";" (write-to-string (float (slot-value st 'score)))
	";" (write-to-string (float (slot-value st 'count)))
	)  
)

(defun get-mode-string(mode)
  (cond 
    ((equal mode 0) "Human")
    ((equal mode 1) "Random")
    ((equal mode 2) "QLearning")
    ((equal mode 3) "QLearning")
  )
)
  
(defun get-line (stream)
  (setf line (read-line stream nil))
  (if line (string-to-list line) line)
 )

(defun find-mode-tries-match (stats mode tries)
	(setf n nil)
	(loop 
		for i from 0 to (length stats)
		for st in stats do (if (and (equal mode (slot-value st 'mode)) (equal tries (slot-value st 'tries))) 
			(setf n i) 
	))
	n
)

(defclass statistic ()
  ((mode
    :initarg :mode
    :accessor stats-mode)
   (tries
    :initarg :tries
    :accessor stats-tries)
   (level
    :initarg :level
    :accessor stats-level)
   (score 
     :initarg :score
     :accessor stats-score)
    (count 
     :initarg :count
     :accessor stats-count)
   )
)

(defun reset-learning ()
	(setf weights '(0 0 0 0 0 0 0))
    (setf count 0)
)
