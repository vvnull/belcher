(defclass card ()
  ((index
    :initarg :idx
    :reader card-index)
   (score
    :initarg :score
    :accessor card-score)
   (name
    :initarg :cardname
    :accessor card-name)
   (color
    :initarg :color
    :accessor card-color)
   (cost
    :initarg :cost
    :accessor card-cost)
   (draw-power
    :initarg :draw-power
    :accessor card-draw-power)
   (mana-power
    :initarg :mana-power
    :accessor card-mana-power)
   (destination
    :initarg :dest
    :accessor dest)
   (is-spell 
    :initarg :spellp
    :accessor spellp)
   (is-artifact
    :initarg :artifactp
    :accessor artifactp)))
