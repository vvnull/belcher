(load "~/.quicklisp/setup")
(with-open-stream (*standard-output* (make-broadcast-stream))
  (ql:quickload "split-sequence"))

; import card class
(load "src/lisp/card.lisp")
(load "src/lisp/game.lisp")
(load "src/lisp/display.lisp")

(defvar *library* '()
  "The Deck")

(defvar *hand* '()
  "The Hand")

(defvar *v* t
  "Verbose output: nil = simulation (train); t = live (test)")

(defvar *log* '("" "" "" "" "")
  "Output log")

(defvar *imp* '()
  "Imprint scores")


; ============
;  functions
; ============

(defun push-log (line game)
    "Pushes output line onto log buffer"
  (when *v*
    (push line *log*)
    (render *log* game)
    (sleep 1)))

(defun parse-card (line)
    "Parse card from deck input file"
  (let ((card-array (split-sequence:split-sequence #\, line)))
  (loop for i from 1 to (parse-integer (car card-array))
    do (push
         (make-instance 'card
                :idx        (parse-integer (nth 1 card-array))
                :color      (char (nth 2 card-array) 0)
                :cost       (parse-integer (nth 3 card-array))
                :draw-power (parse-integer (nth 4 card-array))
                :mana-power (nth 5 card-array)
                :dest       (char (nth 6 card-array) 0)
                :spellp     (when (plusp (length (nth 7 card-array)))
                              t)
                :artifactp  (when (plusp (length (nth 8 card-array)))
                              t)
                :cardname   (nth 9 card-array))
         *library*))))

(defun shuffle (seq)
    "Randomize (destructive)"
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i)(elt seq (+ i (random (- n i)))))))
  (return-from shuffle seq))

(defun reveal (pile)
    "Reveal hand to console"
  (loop for c in pile
    do (format *v* "~%               ~S" (card-name c))))

(defun show-mana (mana)
    "Display mana"
  (format nil "~%=+= Mana =+=
    C: ~D  R: ~D  G: ~D"
         (getf mana #\C)
         (getf mana #\R)
         (getf mana #\G)))

(defun draw (n game)
    "Draw n cards from library"
  (loop for i from 1 to n
    do (push (pop (lib game)) (hand game)))
  (return-from draw game))

(defun draw-init (n)
    "Draw n cards from *library*"
  (loop for i from 1 to n
    do (push (pop *library*) *hand*))
  (reveal *hand*))

(defun can-use (card game)
    "Whether the card is usable"
  (let ((mana-total (loop for c in (cdr (mana game)) by #'cddr
                      sum c))
        (have-mana nil)
        (have-color nil))
    ; determine whether we have enough mana
    (when (equal (card-cost card) 0)
      (return-from can-use t))
    (setf have-mana (or (<= (card-cost card) mana-total)
                        (and (equal (card-index card) 05) ; belcher?
                             (<= (- (card-cost card) 3) mana-total)
                             (find-card-by-index (bf game) 07))))
    ; determine whether we have the right color mana
    (case (card-color card)
      (#\C (setf have-color t))
      (#\R (setf have-color (plusp (getf (mana game) #\R))))
      (#\G (setf have-color (plusp (getf (mana game) #\G))))
      (#\H (setf have-color (plusp (+ (getf (mana game) #\R)
                                      (getf (mana game) #\G)))))
      (otherwise (setf have-color nil)))
  (and have-mana have-color)))

(defun use (card game)
    "Cast/play the card"
  ; use card unless it's a wincon
  (unless (or (equal (card-index card) 03)
              (equal (card-index card) 05))
    (push-log (format nil "~%Using ~S" (card-name card)) game))

    ; pay mana cost
    ; unless it's a wincon
    (unless (or (equal (card-index card) 03)
                (equal (card-index card) 05))
      (setf game (pay-cost card game)))

    ; pass priority
    (when *v*
      (priority (read-char) game))

    ; add mana (if applicable)
    (loop for m across (card-mana-power card)
      do
        (if (equal #\H m)
          (if (find-card-by-index (hand game) 15)
            ; if we have tinder wall in hand, add G
            (progn
              (incf (getf (mana game) #\G))
              (push-log (format nil "~%      adding mana: ~S" #\G) game))
            ; else add red
            (progn
              (incf (getf (mana game) #\R))
              (push-log (format nil "~%      adding mana: ~S" #\G) game)))
          (progn
            (incf (getf (mana game) m))
            (push-log (format nil "~%      adding mana: ~S" m) game))))

    ; draw cards (if applicable)
    (setf game (draw (card-draw-power card) game))

    (when (spellp card)
      (incf (storm game)))

    ; other card functions
    (case (card-index card)
      ; chrome mox
      (00 (let ((pitch (imprint-select (hand game))))
            (if pitch
              (progn
                (push-log (format nil "~%Imprinting ~S" (card-name pitch)) game)
                (setf (hand game) (delete pitch (hand game) :count 1))
                (push pitch (exile game))
                (push-log (format nil "~%      adding mana: ~S" (card-color pitch)) game)
                (if (equal #\H (card-color pitch))
                  (if (find-card-by-index (hand game) 15)
                    ; if we have tinder wall in hand,add G
                    (incf (getf (mana game) #\G))
                    ; else add red
                    (incf (getf (mana game) #\R)))
                  (incf (getf (mana game) (card-color pitch)))))
              (push-log (format nil "~%No imprint") game))))
      ; empty the warrens
      (03 (push-log (format nil "~%Don't cast ~S" (card-name card)) game)
          ; don't use until we're going off
          (return-from use nil))
      ; goblin charbelcher
      (05 (push-log (format nil "~%Don't cast ~S" (card-name card)) game)
          ; don't use until we're going off
          (return-from use nil))
      ; land grant
      (06 
          (setf game (tutor game 14 t)))
      ; rite of flame
      (11 
          (loop for c in (gy game)
            do (when (equal (card-index c) 11)
                 (push-log (format nil "~%      adding mana: #\R") game)
                 (incf (getf (mana game) #\R))))))
  ; send card to destination
    (case (dest card)
      (#\B (push card (bf game)))
      (#\E (push card (exile game)))
      (#\G (push card (gy game)))
      (otherwise (print "(don't know where to put card)")))

    (return-from use game))

(defun pay-cost (card game)
    "Pay mana costs of cards"
  (when (> (card-cost card) 0)
    ; pay colored mana
    (if (equal #\H (card-color card))
      ; if card is hybrid
      (if (and (find-card-by-index (hand game) 15)
               (plusp (getf (mana game) #\R)))
        ; if we have tinder wall in hand,
        ; and have red mana, pay with R
        (decf (getf (mana game) #\R))
        ; else pay with G, if we have it
        (if (plusp (getf (mana game) #\G))
          (decf (getf (mana game) #\G))
          (decf (getf (mana game) #\R))))
      ; if card is not hybrid
      (decf (getf (mana game) (card-color card))))
    ; pay remaining mana
    (let ((rem-cost (1- (card-cost card))))
      (loop while (plusp rem-cost)
        do
;          (format *v* "~%rem-cost: ~S" rem-cost)
;          (show-mana (mana game))
          (when *v* (sleep 0.5))
          (if
            (cond
              ((plusp (getf (mana game) #\C))
                (decf (getf (mana game) #\C)))
              ((and (plusp (getf (mana game) #\R))
                    (> (getf (mana game) #\R)
                       (getf (mana game) #\G)))
                (decf (getf (mana game) #\R)))
              ((plusp (getf (mana game) #\G))
                (decf (getf (mana game) #\G)))
              (t nil))
            (decf rem-cost)
            (print "mana error")))))
  (return-from pay-cost game))

(defun imprint-select (hand)
    "Imprint a card for Chrome Mox"
  (if *v*
    (loop for c in hand
      do
        (unless (or (artifactp c)
                    (equal (card-index c) 14))
          (return-from imprint-select c)))
  ; else
    (loop for i upto (length *imp*)
      do 
        (let ((idx (get-index-nth-highest *imp* i)))
          (loop for c in hand
            do
              (when (equal idx (card-index c))
                (unless (or (artifactp c)
                            (equal (card-index c) 14))
                  (return-from imprint-select c)))))))
  nil)

(defun get-index-nth-highest (l n)
    "Get the index of the nth highest value"
  (let ((high-val 0)
        (high-idx 0))
    (loop for i from 1 to n
      do
          (loop for i upto (length l)
            do
              (when (> (elt l i) high-val)
                (setf high-val (elt l i))
                (setf high-idx i)))
          (setf l (remove high-val l :count 1)))
    high-idx))

(defun tutor (game idx do-reveal)
    "Get card from library and add to hand"
  (let ((card (find-card-by-index (lib game) idx)))
    (unless (equal (lib game) (remove card (lib game) :count 1))
      (push card (hand game))
      (setf (lib game) (delete card (lib game) :count 1))
      (when do-reveal 
        (format *v* "~%Tutor for ~S" (card-name card))))
    (return-from tutor game)))

(defun find-card-by-index (pile idx)
    "Finds a card in lib having index idx"
  (loop for c in pile do
    (when (equal (card-index c) idx)
      (return-from find-card-by-index c))))

(defun belch (game)
    "Fire Goblin Charbelcher"
  (let ((cnt 0)
        (reveal nil))
    (loop do
      (setf reveal (pop (lib game)))
      ; if we don't flip a card, return count
      (if reveal
        (progn
          (push-log (format nil "~%        ~S" (card-name reveal)) game)
          (push reveal (exile game))
          (incf cnt)
          (when (equal (card-index reveal) 14)
            (return-from belch (* 2 cnt))))
        ; else
          (return-from belch cnt)))))




(defun play (game)
    "Play a game of Magic"
  (let ((playing nil))
    (loop do
      (when *v* (sleep 1))
      (setf playing nil)
      (loop for c in (hand game)
        do
          (when (can-use c game)
            (let ((cast-success nil))
              (setf cast-success (use c game))
              (when cast-success
                (setf game cast-success)
                (setf playing t)
                (setf (hand game) (delete c (hand game) :count 1))))))
      while playing))

  ; cast a wincon
  (let ((win nil))
    (setf win (block wincon
                (loop for c in (hand game)
                  do
                    (push-log (format nil "~%Checking ~S" (card-name c)) game)
                    (when (can-use c game)
                      (push-log (format nil "~%Using ~S" (card-name c)) game)
                      (setf (hand game) (delete c (hand game) :count 1))
                      (case (card-index c)
                        ; empty the warrens
                        (03
                          (incf (storm game))
                          (push-log (format nil "~%    ~S 1/1 Goblins" (* 2 (storm game))) game)
                          (return-from wincon c))
                        ; goblin charbelcher
                        (05
                          (push-log (format nil "~%    Firing Charbelcher:") game)
                          (push-log (format nil "~%    ~S damage!" (belch game)) game)
                          (return-from wincon c)))))))
    (show-game-zones game)
    (return-from play win)))

(defun show-game-zones (game)
    "Reveal game zones"
  (show-mana (mana game))
  (format *v*  "~%=*= Hand =*=")
  (reveal (hand game))
  (format *v*  "~%=.= Batlefield =.=")
  (reveal (bf game))
  (format *v*  "~%=-= Graveyard =-=")
  (reveal (gy game))
  (format *v*  "~%=x= Exile =x=")
  (reveal (exile game)))

(defun priority (response game)
    "Priority"
  (case response
    (#\return return)
    ((#\c #\C)
      (push-log (format nil "~%Countered!") game)
      (read-char))
    ((#\r #\R)
      (push-log (format nil "Responding...") game))))

; ============
;  play game
; ============

(defun init ()
    "Shuffle and draw starting hand"
  ; randomize random
  (setf *print-readably* t)
  (let ((seed (make-random-state t)))
    (if (> (length (ext:command-args)) 1)
      ; use seed if provided
      (with-open-file (stream (elt (ext:command-args) 1))
        (setf seed (read stream)))
      ; else write new seed
      (with-open-file (stream "seed"
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (print seed stream)))
    (setf *random-state* seed))


  ; read library
  (with-open-file (stream "belcher.deck")
    (loop for line = (read-line stream nil 'eof)
        until (eq line 'eof)
      do (parse-card line)))

  ; initialize imprint scores
  (defparameter *imp* (make-list (card-index (car (last *library*))) :initial-element 0))

  ; shuffle library
  (format t "~%Shuffling ~D cards" (length *library*))
  (shuffle *library*)

  ; draw opening hand
  (draw-init 7)
  (format t "~%Drew ~D cards" (length *hand*))
  (sleep 2))

(defun meta-play ()
  (defparameter *v* nil)
  (let ((win 0) (loss 0))
    (loop for i from 1 to 100
      do
        (let ((game (make-instance 'game
                :lib  (copy-list *library*)
                :hand (copy-list *hand*))))
          (shuffle (lib game))
          (if (play game)
            (incf win)
            (incf loss))))
    (format t "~%Wins:   ~S~%Losses: ~S" win loss)
    (format t "~%Win rate:  ~S" (/ win (+ win loss))))

  (defparameter *v* t)
  (play (make-instance 'game
                :lib  *library*
                :hand *hand*)))
