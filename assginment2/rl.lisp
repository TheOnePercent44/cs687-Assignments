
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
  ;ANSON
	;I'll need to review algorithm pseudocode, but basically update the utility value of the appropriate state-action pair using reward * alpha(?) (and then backprop?) (figure out how next-state, gamma, and iteration are used. Iteration as param for alpha-func I think)
	;uses (basic-alpha)
	
  ;;; IMPLEMENT ME
								  
  (let ((alpha (funcall alpha-func iteration)))
    ; (print alpha)
    (print reward)
    ; (print next-state)
    ; (print current-state)
    ; (print gamma)
    ; (print iteration)
    ; (print action)
    

	(setf (aref q-table current-state action) 
	(+ (* (- 1 alpha) (aref q-table current-state action))
		(* alpha (+ reward (* gamma (max-q q-table next-state))))))
	q-table)

  ;(print q-table)
  )


;; Top-level nim learning algorithm.  The function works roughly like this...
;;
;; Make a q table.  Hint: make it 6 states larger than needed.  Can you see why?
;; Iterations times:
;;   Set state to 0  (no sticks removed from heap yet)
;;   Loop:
;;       old state <- state
;;       Determine and make my move, update state
;;       If I lost, set my reward to -1
;;       Determine and make my opponent's move, update state
;;       If the opponent lost, set my reward to +1
;;       If no reward has been set yet, set it to 0
;;       Update q table with the reward, old-state, my move, and current ("next") state
;;       If new state is bigger than the heap size, exit loop
;; Return q table

(defun learn-nim (heap-size gamma alpha-func num-iterations)
  "Returns a q-table after learning how to play nim"

  ;;; IMPLEMENT ME
  
  ; SHIKA
  ; a very rough pseudocde! ---
  ; refer- https://gist.github.com/vo/9045230
  ; do times num iter i
  ;   {
  ;     state=0
  ;     loop{
  ;       myaction=best-actions(q,state) ;Shika: use max-action here. I don't think best-actions is intended for this ~AS
  ;       state=state+myaction+1
  ;       if (state >=noof sticks)
  ;         reward =-1
  ;       else
  ;         opponent plays:
  ;         oppaction = best-actions(q, state)
  ;         state=state +oppaction+1
  ;         if(state >= noofsticks)
  ;           reward = +1
  ;         else
  ;           rewad =0
  ;       upate q tabele
  ;       if(state > noofsticks)
  ;         return
  ;     }
  ;   }
    (let* ((num-states (+ heap-size 6)) (num-actions heap-size) (q-table (make-q-table num-states num-actions)))  
      (dotimes (i num-iterations)
        (let ((state 0) my-action opp-action reward )
          (loop 
            (let ((current-state state))
              ; (print i)
              (setf my-action (max-action q-table state))
              (setf state (+ state my-action 1))
              (if (> state heap-size)
                (setf reward -1) ; we lose
                (progn 
                  (setf opp-action (max-action q-table state))
                  (setf state (+ state opp-action 1))  
                  (if (> state heap-size)
                    (setf reward 1) ; we win 
                    (setf reward 0) ; tie
                  )
                )
              )
              (setf q-table (q-learner q-table reward current-state my-action state gamma alpha-func i))
              (if (> state heap-size)
                (return) ; break loop
              )
            )
          )
        )
      )
      (return-from learn-nim q-table)
    )
  )



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

;Anthony Will Do!
(defun play-nim (q-table heap-size)
  "Plays a game of nim.  Asks if the user wants to play first,then has the user play back and forth with the game until one of them wins.  Reports the winner."
  ;;; IMPLEMENT ME
  ;ANSON
  ;ask-if-user-goes-first, initialize state, update alternatingly with actions from q-table and make-user-move, report winner
  ;uses (make-user-move), (ask-if-user-goes-first)
  (let ((turn 0) (current-state 0) (user 0) (actions (best-actions q-table)))
  (if (ask-if-user-goes-first) (setf user 0) (setf user 1))
  (loop while (< current-state heap-size)
        do (if (= turn user) (setf current-state (+ current-state (make-user-move))) (setf current-state (+ current-state (print (+ (elt actions current-state) 1)))))
        do (if (= turn 0) (setf turn 1) (setf turn 0))
        do (format t "Sticks remaining: %d\n" (- heap-size current-state)))))


;Anthony!
;;Is it just me, or does this seem too easy? It talks in places about a list, and actions is plural... but why would we want a list of actions? just getting the best one should suffice, right? What others would there be? Or is it meant to be a list of actions for all states up to game finish? That's what I'm going to do for now...

  ;ANSON
  ;mapcar q-table through max-action function with “-” as val parameter
  ;uses (max-action)
  ;I'm 99% positive that it's meant to be the best action for EACH state, like you said.
  ;ie [State 0: Action A, State 1: Action B, State 2: Action C...]
  ;I think it's meant to be used as a way of "printing" the policy set for human reading
  
(defun best-actions (q-table)
  "Returns a list of the best actions.  If there is no best action, this is indicated with a hyphen (-)"
  ;; hint: see optional value in max-action function
  (let ((current-state (num-states q-table)) bag)
    (dotimes (num-states q-table)
              (push (max-action q-table current-state "-") bag)
              (- current-state 1))
    (print bag))
  ;;; IMPLEMENT ME
  ;too easy? I should think so
  ;Also would require me to either A) add a current-state input parameter to the function or B) have us use a global *current-state* variable. I assume Luke means for the above instead
  ;(max-action q-table *current-state* "-")
  )




;; example:
;; 
;; (setq *my-q-table* (learn-nim 22 0.1 #'basic-alpha 50000))
;;
;; to get the policy from this table:
;;
;; (best-actions *my-q-table*)
;;
;; to play a game of your brain versus this q-table:
;;
;; (play-nim *my-q-table* 22)   ;; need to provide the original heap size
;;
;; You might try changing to some other function than #'basic-alpha...

