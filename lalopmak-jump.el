
;;  Copyright (C) 2013

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;If the user wants to set a default number of lines to search for jumps
(defvar lalopmak-evil-ace-jump-num-lines nil)


;; Sets number of lines to search in an ace jump
(evil-ex-define-cmd "acejumplines" (lambda (n) 
                                     (interactive "n# lines to search in ace jump (negative to unset): ")
                                     (if (>= n 0)
                                         (setq lalopmak-evil-ace-jump-num-lines n)
                                       (setq lalopmak-evil-ace-jump-num-lines nil))))


(defvar ace-query "Query Char:")

;;max lines, words, chars to search
(defvar ace-jump-max-lines 20)
(defvar ace-jump-max-words 400)
(defvar ace-jump-max-chars 1000)

;;;
;;; Useful region macros
;;;

(defmacro save-excursion-point (movement)
  "Executes movement (a piece of code going somewhere), saves resulting point."
    `(condition-case nil
         (save-excursion ,movement (point))
       ;;Accounts for buffer errors
       (beginning-of-buffer (point-min))
       (end-of-buffer (point-max))))

(defmacro do-within-positions (start end &rest code)
  "Restricts to region determined by positions start, end, executes code."
  `(unwind-protect
       (progn
         (narrow-to-region ,start ,end)
         ,@code)
     (widen)))

(defmacro do-within-movements (goto-start goto-end &rest code)
  "Restructs to region determined by movements goto-start, goto-end, executes code."
  `(do-within-positions (save-excursion-point ,goto-start)
                        (save-excursion-point ,goto-end)
                        ,@code))

(defmacro do-within-n-lines (n &rest code)
  "Narrows region to within n lines, executes code"
  `(do-within-movements (forward-line (- ,n)) (forward-line (1+ ,n)) ,@code))

(defmacro do-within-n-words (n &rest code)
  "Narrows region to within n words, executes code"
  `(do-within-movements (forward-word (- ,n)) (forward-word (1+ ,n)) ,@code))

(defmacro do-within-n-chars (n &rest code)
  "Narrows region to within n chars, executes code"
  `(do-within-movements (forward-char (- ,n)) (forward-char (1+ ,n)) ,@code))


;;;
;;; Misc functions
;;;

(defun get-user-input-character (prompt)
  "Prompts, gets user character as input."
  (interactive "p")
  (read-char prompt))


(defun count-char-in-buffer (char)
  "Counts the number of char in buffer"
  (save-excursion (goto-char (point-min)) 
                  (count-matches (make-string 1 char))))

(defmacro max-regions-for-one-ace-jump (char region-restrictor maxLimit)
  "Max number of lines around cursor for which we can limit an ace jump of char so that it completes in a single step.
Limited by ace-jump-max-lines."
  `(let ((x 0)
         (numKeys (length ace-jump-mode-move-keys)))
     (while (and (<= x ,maxLimit)
                 (<= (,region-restrictor x (count-char-in-buffer ,char))
                     numKeys))
       (setq x (1+ x)))
     (if (eq x 0)  
         x         ;zero is the lowest we can go
       (1- x))))

;;;
;;; normal jump mode
;;;

(defmacro perform-ace-jump-char-within-n-regions (char region-restrictor &optional n)
  "Calls ace-jump-char on char, limiting possible results to within n (default 0) lines of the pointer."
  `(let ((ace-jump-mode-scope 'window))        ;;makes sure we don't leak to other scopes
     (,region-restrictor (or ,n 0) (ace-jump-char-mode ,char))))
                                  
(defmacro lalopmak-evil-ace-char-jump-mode-for-region (count region-restrictor max-regions)
  `(let* ((char (get-user-input-character ace-query))
          (numRegions (or ,count 
                          lalopmak-evil-ace-jump-num-lines
                          (max-regions-for-one-ace-jump char
                                                        ,region-restrictor
                                                        ,max-regions))))
     (evil-enclose-ace-jump-for-motion 
      (perform-ace-jump-char-within-n-regions char ,region-restrictor numRegions))))
  
(evil-define-motion lalopmak-evil-ace-jump-char-mode (count)
  "Ace jumps within count lines, or according to user-set lalopmak-evil-ace-jump-num-lines, or the most of region that would result in a single ace-search"
  :type inclusive
  (if (or count 
          lalopmak-evil-ace-jump-num-lines)
      ;;if user provided restriction input we assume it's in lines
      (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-lines ace-jump-max-lines)
    
    ;;Three possible search regions so far: chars, words, lines, in increasing granuity.
    (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-chars ace-jump-max-chars)
    ;; (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-words ace-jump-max-words)
    ;; (lalopmak-evil-ace-char-jump-mode-for-region count do-within-n-lines ace-jump-max-lines)
))


;;;
;;; "jump-to" mode
;;;

(defmacro if-point-changed (action decreasedAction &optional increasedAction samePlaceAction)
  "Checks whether the point decreased, increased, or stayed the same as a result of action,
executes the resulting actions"
  `(let ((old-point (point)))
     ,action
     (cond ((> old-point (point)) ,decreasedAction)
           ((< old-point (point)) ,increasedAction) 
           ((= old-point (point)) ,samePlaceAction))))

(defmacro lalopmak-evil-search-char-to (search)
  "Converts a searcher to a searcher-to"
  `(if-point-changed ,search
                     (forward-char)
                     (backward-char)))

;;Corrects repositories; might not be needed if fixed
(evil-define-motion evil-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  (lalopmak-evil-search-char-to (evil-ace-jump-char-mode count)))
 
(evil-define-motion lalopmak-evil-ace-jump-char-to-mode (count)
  "Ace jumps within count lines, or default.  Stops one character short of result."
  :type inclusive
  (lalopmak-evil-search-char-to (lalopmak-evil-ace-jump-char-mode count)))


(provide 'lalopmak-jump)
