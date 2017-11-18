
;Reinforcement Learning Project
;
;This should not be particularly difficult, and you could probably get it done this week if you wished.
;A Q-learner that learns to play Nim.
;
;This project is an easy assignment.
;What I'm asking you to do is to complete the assignment, then
;"enhance" it in some way.  Try three heaps; try prioritorized sweeping
;(ask me); try comparing approaches to alpha, gamma, action-selection procedures,
;ways of implementing the "opponent, etc.  Try extending to 3-Heap Nim.
;Something fun.
;
;You might also try to analyze what happens when you try random opponents versus co-adaptive ones.  Advantages?  Disadvantages?
;
;ABOUT NIM
;---------
;There are several versions of Nim.  The version we will play is called 1-Heap Nim
;and it goes like this:
;
;1. Put N sticks in a pile (called a "heap")
;2. Players take turns taking 1, 2, or 3 sticks out of the heap.
;3. Whoever has to take out the last stick loses.
;
;
;LEARNING NIM
;------------
;
;Our Q-learner will build a Q-table solely through playing itself over and over
;again.  The table will tell it, ultimately, how smart it is to do a given move
;(take 1, 2, or 3 sticks) in a given state (number of sticks taken out so far).
;Q values will all start at 0.
;
;We will define the actions as follows:
;
;Action 0: take 1 stick out
;Action 1: take 2 sticks out
;Action 2: take 3 sticks out
;
;Thus the action number is exactly 1- the number of sticks to take out.  Keep
;this in mind -- the Q table will store Q values by action number, NOT by
;sticks taken out.
;
;We will define the states as follows:
;
;State 0: no sticks removed from heap
;State 1: 1 stick removed from heap
;...
;State N: N sticks removed from heap
;
;You will probably find it useful for the number of states in the Q table to
;be, believe it or not, about 6 larger than the heap size.  Thus there are
;some states at the high end of the table which represent, more or less,
;"negative heap sizes".  Of course, you can never play a negative heap size;
;such q-values will stay 0.
;
;Our Q table will be a STATE x ACTION array.  I have given you some functions
;which should make it easy to use this array:  NUM-STATES, NUM-ACTIONS,
;MAKE-Q-TABLE, MAX-Q, and MAX-ACTION.
;
;The Q learner will learn by playing itself: the learner records the current
;state, makes a move, lets the ``opponent'' make a move, then notes the new
;resulting state.  The action is the move the learner made.  Now we have s,
;a, and s'.  Note that s' is the state AFTER the opponent made his move.
;
;After the Q learner has learned the game, then you can play the learner
;and see how well it does.
;
;
;WHAT YOU NEED TO DO
;-------------------
;
;Your job is to implement several functions:
;
;Q-LEARNER
;  (the Q update function)
;LEARN-NIM
;  (the learning algorithm, tweaked for Nim -- the longest function)
;PLAY-NIM
;  (lets you play against the learned Q table)
;BEST-ACTIONS
;  (prints out the best actions believed so far)
;
;To help you, I've written a basic ALPHA function, and MAKE-USER-MOVE
;and ASK-IF-USER-GOES-FIRST functions.  I predict you will find them helpful.
;
;
;
;THE SECRET OF NIM (ugh, that was bad)
;-----------------
;
;You can get an idea for how well these settings perform by seeing what's
;usually the smallest number of iterations necessary before BEST-ACTIONS starts
;reporting the correct actions.
;
;So what ARE the correct actions in Nim?  There is a very simple rule for playing
;Nim.  If there are N sticks left in the pile, you want to remove sticks so that
;N = 1 + 4A where A is some number.  Then whatever your opponent takes out, you take
;4 minus that number, so your sticks and your opponent's sticks removed sum to 4.
;Keep on doing this, and eventually the A's will get dropped and your opponent will
;be left with 1 stick, which he must take.
;
;Depending on the size of the Nim heap, the game is either a guaranteed win for
;the first player or for the second player.  It all depends on who can get it down
;to 1 + 4A first.
;
;You will discover a certain pattern emerge in your BEST-ACTIONS list.  The first
;couple of values may be odd, but then from there on out you'll see
;2, 1, 0, <any>, 2, 1, 0, <any>, etc.  This is because in each of those heap
;values, the right move is to remove 3, 2, or 1 sticks, or (in the <any> value)
;it doesn't matter because you're guaranteed to lose at that heap size.  In essence
;you want to get your OPPONENT down to the <any> value (it's the 1 + 4A number).
;
;
;VERY STRONG HINT
;
;Keep in mind how the Q table is structured: actions are stored in the slot
;1 less than the number of sticks removed by that action.  And states go UP
;as more sticks are removed.   You may need to do some 1-'s and 1+'s to play
;the right action.
;
;
;INTERESTING TRIVIA
;
;Nim's been done a lot.  I was going to do tic-tac-toe, but decided it was too
;evil.  :-)



(defun random-elt (sequence)
  "Returns a random element from a sequence"
  (elt sequence (random (length sequence))))

(defun num-states (q-table)
  "Returns the number of states in a q-table"
  (first (array-dimensions q-table)))

(defun num-actions (q-table &optional state)
  "Returns the number of actions in a q-table"
  (second (array-dimensions q-table)))

(defun make-q-table (num-states num-actions)
  "Makes a q-table, with initial values all set to 0"
  (make-array (list num-states num-actions) :initial-element 0))

(defun max-q (q-table state)
  "Returns the highest q-value for a given state over all possible actions. If the state is outside the range, then utility-for-outside-state-range is returned."
  (let* ((num-actions (num-actions q-table))
	 (best (aref q-table state (1- num-actions))))  ;; q of last action
    (dotimes (action (1- num-actions) best)  ;; all but last action...
      (setf best (max (aref q-table state action) best)))))

(defun max-action (q-table state &optional val)
  "Returns the action which provided the highest q-value.  If val is not provided, ties are broken at random; else val is returned instead when there's a tie. If state is outside the range, then an error is generated  (probably array-out-of-bounds)."
  ;; a little inefficient, but what the heck...
  (let ((num-actions (num-actions q-table))
	(best (max-q q-table state))
	bag)
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
	(push action bag)))
    (if (and val (rest bag))
	val
      (random-elt bag))))

(defparameter *basic-alpha* 0.5 "A simple alpha constant")
(defun basic-alpha (iteration)
  (declare (ignore iteration)) ;; quiets compiler complaints
  *basic-alpha*)

(defun q-learner (q-table reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table and returns it.  alpha-func is a function which must be called to provide the current alpha value."
  (let ((alpha (funcall alpha-func iteration)))
    (setf (aref q-table current-state action) 
          (+ (* (- 1 alpha) (aref q-table current-state action)) ;(1 - alpha) * Q(S, A)
             (* alpha (+ reward (* gamma (max-q q-table next-state)))))) ;alpha * (reward + gamma * Max-Q(S'))
    q-table))

(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"
  (if (listp heap-size) (learn-nim-plus heap-size gamma alpha-func num-iterations)
  (let ((q-table (make-q-table (+ heap-size 6) 3)))   
    (dotimes (i num-iterations)
      (let ((state 0) my-action opp-action reward )
        (loop
          (let ((current-state state))
            (setf my-action (max-action q-table state))
            (setf state (+ state my-action 1))
            (if (>= state heap-size)
              (setf reward -1)
              (progn 
                (setf opp-action (max-action q-table state))
                (setf state (+ state opp-action 1))  
                (if (>= state heap-size)
                  (setf reward 1)
                  (setf reward 0))))
            (setf q-table (q-learner q-table reward current-state my-action state gamma alpha-func i))
            (if (> state heap-size)
              (return))))))
    (return-from learn-nim q-table))))

(defun ask-if-user-goes-first ()
  "Returns true if the user wants to go first"
  (y-or-n-p "Do you want to play first?"))

(defun make-user-move ()
  "Returns the number of sticks the user wants to remove"
  (let ((result))
    (loop
     (format t "~%Take how many sticks?  ")
     (setf result (read))
     (when (and (numberp result) (<= result 3) (>= result 1))
       (return result))
     (format t "~%Answer must be between 1 and 3"))))

(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,then has the user play back and forth with the game until one of them wins.  Reports the winner."  
  (if (listp heap-size) (play-nim-plus q-table heap-size)
  (let ((turn 0) (current-state 0) (user 0) )
  (if (ask-if-user-goes-first) (setf user 0) (setf user 1))
  (loop while (< current-state heap-size)
        do (if (= turn user) (setf current-state (+ current-state (make-user-move))) (setf current-state (+ current-state (print (+ (max-action q-table current-state) 1)))))
        do (if (= turn 0) (setf turn 1) (setf turn 0))
        do (format t "Sticks remaining: ~d" (- heap-size current-state)))
  (if (= turn user) "Player wins!" "Computer wins!"))))

(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  (let ((state-count (num-states q-table)) (bag))
    (dotimes (i state-count bag)
              (push (max-action q-table (- state-count (1+ i)) '-) bag))))


;----------------end of original assignment---------------------------
;below are additional functions added to include additional functionality, including other alpha functions

(defun learn-nim-plus (heaps gamma alpha-func num-iterations)
  "Trains a policy set to win nim.
Includes functionality for multi-heap nim, and nim with different maximums for 'sticks taken per turn', including the option to have different maximums for different piles.
'Heaps' argument can be in one of four formats:
x - single heap of size x, up to 3 sticks per turn
(x1 x2 x3 ... xn) - n heaps of sizes x1, x2, x3 ... xn, up to 3 sticks per turn
((x1 x2 ... xn) y) - n heaps of sizes x1, x2 ... xn, up to y sticks per turn
((x1 x2 ... xn) (y1 y2 ... yn)) - n heaps of sizes x1, x2 ... xn, up to y1 sticks from x1, y2 from x2, ... or yn from xn per turn"
  (let
    ((heap-sizes (get-heaps-sizes heaps)) (heap-actions (get-heaps-actions heaps))
    (q-table (make-q-table-plus (get-heaps-sizes heaps) (get-heaps-actions heaps))))
    (dotimes (i num-iterations q-table)
      (let ((state heap-sizes) my-action opp-action reward)
        (loop
         (let ((current-state state))
           (setf my-action (max-action-plus q-table state heap-sizes heap-actions))
           (setf state (get-next-state state my-action))
           (if (game-over state)
             (setf reward -1)
             (progn
               (setf opp-action (max-action-plus q-table state heap-sizes heap-actions))
               (setf state (get-next-state state opp-action))
               (if (game-over state)
                 (setf reward 1)
                 (setf reward 0))))
           (setf q-table (q-learner-plus q-table heap-sizes heap-actions reward current-state my-action state gamma alpha-func i))
           (if (game-over state) (return))))))))

(defun order-state (state)
  "Converts a state to be in 'proper' order (descending magnitude)
Used to collapse redundant states"
  (sort state #'>))

(defun get-heaps-sizes (heaps)
  "Returns the heap sizes of the 'heaps' argument"
  (sort (if (listp heaps) (if (listp (first heaps)) (first heaps) heaps) (list heaps)) #'>))

(defun get-heaps-actions (heaps)
  "Returns the heap actions of the 'heaps' argument"
  (if (and (listp heaps) (listp (first heaps))) 
    (if (listp (second heaps)) (second heaps) (make-list (length (first heaps)) :initial-element (second heaps))) 
    (make-list (if (listp heaps) (length heaps) 1) :initial-element 3)))

(defun state-to-index (state q-sizes)
  "Converts a list of heap sizes into a lookup index for a q-table with the starting heaps provided. Works opposite to index-to-state"
  (reduce #'+ (mapcar #'* (get-size-scalars q-sizes) state)))

(defun index-to-state (index q-sizes)
  "Converts a lookup index for a q-table with the starting heaps provided into a list of heap sizes. Works opposite to state-to-index"
  (mapcar (lambda (x)
            (let ((modulo 0))
              (loop while (>= index x)
                    do (decf index x)
                    do (incf modulo))
              modulo))
          (get-size-scalars q-sizes)))

(defun get-size-scalars (q-sizes)
  "Returns the scale values needed to convert between state and index"
  (let ((product 1) scalar)
    (dolist (x (reverse (sort q-sizes #'>)) scalar)
      (push product scalar)
      (setf product (* product (1+ x))))))

(defun index-to-action (index q-actions)
  "Returns the action with the given index, according to q-actions. Works opposite to action-to-index"
  (incf index)
  (reverse (mapcar 
   (lambda (x) 
     (let ((val (if (and (> index 0) (>= x index)) index 0)))
       (decf index x)
       val))
   (reverse q-actions))))

(defun action-to-index (action q-actions)
  "Returns the index of the given action, according to q-actions. Works opposite to index-to-action"
  (let ((val 0))
    (reduce #'+ (mapcar (lambda (x) (if (not (= 0 x)) (setf val action) (if (not (= 0 val)) (setf val (+ 1 x)))) val) q-actions))))

(defun max-q-plus (q-table state q-sizes q-actions)
  "Returns the highest value for any LEGAL action available from the given state according the provided q-table. Returns nil if there are no legal moves"
  (let ((best) (num-actions (reduce #'+ q-actions)) (index (state-to-index state q-sizes)) (next-action))
    (dotimes (action num-actions best)
      (setf next-action (aref q-table index action))
      (setf best (if (not best) next-action (if (not next-action) best (max best next-action)))))))

(defun max-action-plus (q-table state q-sizes q-actions &optional val)
  "Returns the LEGAL action with the highest value available from the given state according to the provided q-table, in the form of a list. If var is specified, it will be returned in the case of a tie. Otherwise, it will return one of the highest valued actions at random. If there are no legal moves, a random one is returned if val is not specified."
  (let ((num-actions (reduce #'+ q-actions)) (best (max-q-plus q-table state q-sizes q-actions)) (bag))
    (dotimes (action num-actions)
      (when (= (aref q-table state action) best)
        (push action bag)))
    (if (and val (rest bag))
        val
      (index-to-state (random-elt bag) q-sizes))))

(defun get-next-state (state action)
  "Returns the state that results from the chosen action"
  (mapcar (lambda (x) (max 0 x)) (mapcar #'- state action)))

(defun game-over (state)
  "Returns whether the game is over or not (in other words, if all of the piles are empty)"
  (= 0 (reduce #'+ state)))

(defun best-actions-plus (q-table heaps)
  "Returns the best action calculated by q-table for each state, formatted to be human-readable.
'Heaps' argument uses same convention as learn-nim-plus"
  (let ((state-count (num-states q-table)) (q-sizes (get-heaps-sizes heaps)) (q-actions (get-heaps-actions heaps)) (bag) (state))
    (dotimes (index state-count bag)
      (setf state (index-to-state index q-sizes))
      (if (= state (order-state state))
          (push (list state (max-action-plus q-table state q-sizes q-actions -)) bag)))))

(defun play-nim-plus (q-table heaps)
  "Allows the user to play a game against a trained policy set.
'Heaps' argument uses same convention as learn-nim-plus"
  (let ((turn 0) (current-state) (q-sizes (get-heaps-sizes heaps)) (user 0) (q-actions (get-heaps-actions heaps)))
    (setf current-state q-sizes)
    (if (ask-if-user-goes-first) (setf user 0) (setf user 1))
    (loop while (not (game-over current-state))
          do (if (= turn user)
                 (setf current-state (get-next-state current-state (player-move-plus current-state q-actions)))
               (setf current-state (get-next-state current-state (max-action-plus q-table current-state q-sizes q-actions))))
          do (if (= turn 0) (setf turn 1) (setf turn 0))
          do (format t "Sticks remaining: ~d" current-state))
    (if (= user turn) "Player wins!" "Computer wins!")))

(defun player-move-plus (state q-actions)
  "Asks the user to choose a LEGAL move, returns the chosen move"
  (let (pile quantity (len (length q-actions)))
    (loop
     (format t "~%Take sticks from which pile? (piles 1 to ~d)" len)
     (setf pile (1- (read)))
     (if (and (numberp pile) (< pile len) (>= pile 0) (> (elt state pile) 0))
         (progn
           (format t "~%Take how many sticks?")
           (setf quantity (read))
           (when (and (numberp quantity) (<= quantity (elt q-actions pile)) (>= quantity 1))
             (return (setf (elt (make-list len :initial-element 0) pile) quantity)))
           (format t "~%Answer must be between 1 and ~d" (elt q-actions pile)))
       (format t "~%Answer must be between 1 and ~d, pile must not be empty" len)))))
             
(defun q-learner-plus (q-table q-sizes q-actions reward current-state action next-state gamma alpha-func iteration)
  "Modifies the q-table to incorporate the feedback from the provided action sequence"
  (let 
      ((alpha (funcall alpha-func iteration)) 
       (current-index (state-to-index current-state q-sizes)) 
       (action-index (action-to-index action q-actions))
       (next-index (state-to-index next-state q-sizes)))
    (setf (aref q-table current-index action-index)
          (+ (* (- 1 alpha) (aref q-table current-index action-index))
(* alpha (+ reward (* gamma (max-q q-table next-index))))))
    q-table))

(defun make-q-table-plus (q-sizes q-actions)
  "Makes a q-table, with initial values all set to 0"
  (let* ((states (reduce #'* (mapcar #'1+ q-sizes))) (actions (reduce #'+ q-actions))
         (q-table (make-array (list states actions))))
    (dotimes (i states q-table)
      (dotimes (j actions)
        (setf (aref q-table i j) 
              (if (reduce (lambda (x y) (and x y)) (mapcar #'>= (index-to-state i q-sizes) (index-to-action j q-actions))) 
                  0 nil))))))
    

;--------------------JUNK------------------------------------------------------

(defun player-move-minus (state heap-sizes)
  (let ((pile) (quantity))
    (loop
     (format t "~%Take sticks from which pile? (piles 1 to ~d)" (length heap-sizes))
     (setf pile (read))
     (if (and (numberp pile) (<= pile (length heap-sizes)) (>= pile 1) (> (elt state (1- pile)) 0))
       (progn
         (format t "~%Take how many sticks?  ")
         (setf quantity (read))
         (when (and (numberp quantity) (<= quantity 3) (>= quantity 1))
           (return (+ (* 3 (1- pile)) (1- quantity)))
         (format t "~%Answer must be between 1 and 3"))
       (format t "~%Answer must be between 1 and ~d, pile must not be empty" (length heap-sizes)))))))

(defun play-nim-minus (q-table heap-sizes)
  (let ((turn 0) (current-state heap-sizes) (user 0))
  (if (ask-if-user-goes-first) (setf user 0) (setf user 1))
  (loop while (not (game-over current-state))
        do (if (= turn user) 
             (setf current-state (take-action current-state (player-n-heaps-move current-state heap-sizes))) 
             (setf current-state (take-action current-state (max-action q-table (list-to-state current-state heap-sizes)))))
        do (if (= turn 0) (setf turn 1) (setf turn 0))
        do (format t "Sticks remaining: ~d" current-state))
  (if (= user 1) "Computer wins!" "Player wins!")))

(defun learn-nim-minus (heap-sizes gamma alpha-func num-iterations)
  (let ((q-table (ban-actions (make-q-table (1+ (list-to-state (make-list (length heap-sizes) :initial-element -5) heap-sizes)) (* 3 (length heap-sizes))) heap-sizes)))
    (dotimes (i num-iterations q-table)
      (let ((state 0) my-action opp-action reward)
        (loop
          (let ((current-state state))
            (setf my-action (max-action q-table state))
            (setf state (take-action-raw state my-action heap-sizes))
            (if (game-over-raw state heap-sizes)
              (setf reward -1)
              (progn 
                (setf opp-action (max-action q-table state))
                (setf state (take-action-raw state opp-action heap-sizes))
                (if (game-over-raw state heap-sizes)
                  (setf reward 1)
                  (setf reward 0))))
            (setf q-table (q-learner q-table reward current-state my-action state gamma alpha-func i))
            (if (game-over-raw state heap-sizes)
              (return))))))))

(defun best-actions-minus (q-table heap-sizes)  
  (let ((state-count (num-states q-table)) (bag))
    (dotimes (i state-count bag)
      (push (list 
             (state-to-list (- state-count (1+ i)) heap-sizes)
             (max-action q-table (- state-count (1+ i)) '-))
      bag))))

(defun ban-actions (q-table heap-sizes)
  (dotimes (state (num-states q-table) q-table)
    (dotimes (heap (length heap-sizes))
      (when (<= (elt (state-to-list state heap-sizes) heap) 0)
        (dotimes (x 3)
          (setf (aref q-table state (+ x (* 3 heap))) 1))))))

(defun take-action-raw (state action heap-sizes)
  (list-to-state (take-action (state-to-list state heap-sizes) action) heap-sizes))

(defun take-action (list-state action)
  (decf (elt list-state (floor action 3)) (1+ (mod action 3)))
  list-state)



(defun game-over-raw (state heap-sizes)
  (game-over (state-to-list state heap-sizes)))

(defun game-over (list-state)
  (dolist (x list-state t)
    (if (> x 0) (return nil))))



(defun state-to-list (state heap-sizes)
  (let ((list-state ()))
    (dolist (x heap-sizes (reverse list-state))
      (push (- x (mod state (+ 6 x))) list-state)
      (setf state (floor state (+ 6 x))))))

(defun list-to-state (list-state heap-sizes))
    
(defun test-lts ()
  (dotimes (x 9) (dotimes (y 9) (dotimes (z 9) (print (list-to-state (print (list (- 3 x) (- 3 y) (- 3 z))) '(3 3 3)))))))

(defun test-stlts ()
  (dotimes (x 990) (print "")(print (list-to-state (print (state-to-list (print x) '(3 4 5))) '(3 4 5)))))

