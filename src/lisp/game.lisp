(defclass game ()
  ((library
    :initarg :lib
    :accessor lib)
   (hand
    :initarg :hand
    :initform '()
    :accessor hand)
   (battlefield
    :initform '()
    :accessor bf)
   (graveyard
    :initform '()
    :accessor gy)
   (exile
    :initform '()
    :accessor exile)
   (stack
    :initform '()
    :accessor stack)
   (storm
    :initform 0
    :accessor storm)
   (mana
    :initform (list
                #\C 0
                #\W 0
                #\U 0
                #\B 0
                #\R 0
                #\G 0)
    :accessor mana)))
