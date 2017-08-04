(defun render (game)
  ; clear screen
  (format *v* "~A[H~@*~A[J" #\escape)

  (format *v* "~%")
  (format *v* "  Game Zones                   Mana Pool~%")
  (format *v* " -------------                 ---------~%")
  (format *v* " Library:   ~2d                  W: ~2d ~%" (length (lib game))       (getf (mana game) #\W))
  (format *v* " Graveyard: ~2d                  U: ~2d ~%" (length (graveyard game)) (getf (mana game) #\U))
  (format *v* " Exile:     ~2d                  B: ~2d ~%" (length (exile game))     (getf (mana game) #\B))
  (format *v* " Hand:      ~2d                  R: ~2d ~%" (length (hand game))      (getf (mana game) #\R))
  (format *v* "                                 G: ~2d ~%"                           (getf (mana game) #\G))
  (format *v* " Storm:     ~2d                  C: ~2d ~%" (storm game)              (getf (mana game) #\C))
  (format *v* "                                        ~%" ))
