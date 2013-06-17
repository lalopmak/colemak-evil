;;; License

;; This software is released under the CC0 1.0 Universal license. You are
;; free to use, modify, and redistribute it as you please. This software
;; comes with NO WARRANTIES OR GUARANTEES WHATSOEVER. For details, see
;; http://creativecommons.org/publicdomain/zero/1.0/

;;Keyboard Layout related functions

(defun colemak-to-qwerty () 
  "Returns the colemak to qwerty map"
  '(("f"."e") ("p"."r") ("g"."t") ("j"."y") ("l"."u") ("u"."i") ("y"."o") (";"."p") (":"."P")
    ("r"."s") ("s"."d") ("t"."f") ("d"."g") ("n"."j") ("e"."k") ("i"."l") ("o".";") ("O".":")
    ("k"."n")))

(defun colemak-to-colemak ()
  "Returns the colemak to colemak map, aka itself"
  '())

(defun key-to-layout (key layout)
  "Given a key and a layout map, uses it"
  (if (stringp key)   ;currently can only do strings
      (let ((result (assoc key (funcall layout)))                  ;result of the layout map
            (lowerResult (assoc (downcase key) (funcall layout)))  ;result of lowercase
            (upperResult (assoc (upcase key) (funcall layout)))    ;result of uppercase
            (hyphen (string-match "-" key)))
        (cond (result (rest result))  ;if it's in our (funcall layout) map, we take it
              (hyphen (let ((prefix (substring key 0 hyphen)) ;if we have hyphen, eg M-x, to M-(key-to-(funcall layout) x)
                            (suffix (substring key (1+ hyphen))))
                        (concat prefix "-" (key-to-layout suffix layout))))  
              (lowerResult (upcase (rest lowerResult)))   ;key originally uppercase
              (upperResult (downcase (rest upperResult)))  ;key originally lowercase
              (t key))) ;returns itself as fallback
    key))  

(provide 'lalopmak-layouts)
