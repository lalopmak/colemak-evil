;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS",
;; WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
;; THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Misc library called by lalopmak-jump.


(defun jump-query (&optional current max)
  "The query with which to prompt the user upon jump

If this jump accepts MAX characters and we're currently on CURRENT, includes that in prompt" 
  (if (and current
           max 
           (> max 1))
      (concat "Query Char (" (number-to-string current) "/" (number-to-string max) "):")
    "Query Char:"))

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
  "Restricts to region determined by positions start, end, then executes code."
  `(unwind-protect
       (progn
         (narrow-to-region ,start ,end)
         ,@code)
     (widen)))

(defmacro do-within-movements (goto-start goto-end &rest code)
  "Restricts to region determined by movements goto-start, goto-end, then executes code."
  `(do-within-positions (save-excursion-point ,goto-start)
                        (save-excursion-point ,goto-end)
                        ,@code))

(defmacro do-within-n-movements (n backward-movement forward-movement &rest code)
  "Narrows region to within n movements, executes code.
One of backward, forward movement is optional (the other should be input as nil),
since often, but not always, one can be generated from the other."
  (let ((back (if backward-movement
                  `(,backward-movement (1+ ,n))
                `(,forward-movement (- ,n))))
        (forwards (if forward-movement
                      `(,forward-movement (1+ ,n))
                    `(,backward-movement (- ,n)))))
    `(do-within-movements ,back ,forwards ,@code)))

(defmacro do-within-n-lines (n &rest code)
  "Narrows region to within n lines, executes code"
  `(do-within-n-movements ,n nil forward-line ,@code))

(defmacro do-within-n-words (n &rest code)
  "Narrows region to within n words, executes code"
  `(do-within-n-movements ,n backward-word forward-word ,@code))

(defmacro do-within-n-chars (n &rest code)
  "Narrows region to within n chars, executes code"
  `(do-within-n-movements ,n nil forward-char ,@code))


;;;
;;; Misc functions
;;;


(defun chars-in-window ()
  (- (window-end) (window-start)))

(defun get-user-input-character (prompt)
  "Prompts, gets user character as input."
  (interactive "p")
  (read-char prompt))

(defun get-ace-user-input (num-chars) 
  "Prompts, gets NUM-CHARS characters as input, concats them into single string."
  (apply #'concat 
         (loop for i from 0 below num-chars
               collect
               (list (get-user-input-character (jump-query i num-chars))))))

(defun count-string-in-buffer (string)
  "Counts the number of char in buffer"
  (save-excursion (goto-char (point-min))
                  (count-matches (regexp-quote string))))

(defmacro max-regions-for-one-jump (string region-restrictor regions-search-limit jumper-limit)
  "Max number of lines around cursor for which we can limit a jump of char so that it completes in a single step.
regions-search-limit is our search bound."
  `(loop for r from 0 to ,regions-search-limit
         until (> (,region-restrictor r (count-string-in-buffer ,string))
                  ,jumper-limit)
         finally return (if (eq r 0)
                            0         ;zero is the lowest we can go
                          (1- r))))

(defmacro if-point-changed (action &optional decreasedAction increasedAction samePlaceAction)
  "Checks whether the point decreased, increased, or stayed the same as a result of action,
executes the resulting actions"
  (declare (indent 2))
  `(let ((old-point (point)))
     ,action
     (cond ((> old-point (point)) ,decreasedAction)
           ((< old-point (point)) ,increasedAction)
           ((= old-point (point)) ,samePlaceAction))))

(defmacro search-to-searchTo (search)
  "Converts a search to a search that lands right before the target"
  `(if-point-changed ,search
       (forward-char)
     (backward-char)))

(defun space-ambiguous-regex (query-chars &optional disallow-match-all)
  "Given a list of jump- queried chars, returns a regex search,

subject to the condition that terminal spaces might represent 

either literal spaces or end of lines.

disallow-match-all disallows results that match everything.

e.g. (space-ambiguous-regex '(100 101 32 32)) = de\($\| $\|  )"
  (let* ( ;;the number of terminal spaces
         (remaining-spaces (let ((first-reversed-space nil)
                                 (i 0))
                             (mapc (lambda (char)
                                     (unless (or first-reversed-space
                                                 (eq char 32))
                                       (setq first-reversed-space i))
                                     (setq i (1+ i)))
                                   (reverse query-chars))
                             (or first-reversed-space
                                 (length query-chars)))) 
         ;;one above the index of the last non-space
         (only-spaces-to-end-index (- (length query-chars)
                                      remaining-spaces))
         ;;the regex formed only from less than that index
         (non-eol-regex (regexp-quote 
                         (apply #'string 
                                (subseq query-chars 
                                        0 
                                        only-spaces-to-end-index))))
         (final-regex (if (> remaining-spaces 0)
                          (let ((full-regex (concat non-eol-regex
                                                    "\\(")))
                            (dotimes (i remaining-spaces 
                                        (concat full-regex
                                                (make-string i 32) 
                                                "\\)"))
                              (unless (and disallow-match-all 
                                           (eq 0 only-spaces-to-end-index)
                                           (eq i 0))
                                (setq full-regex 
                                      (concat full-regex 
                                              (make-string i 32)
                                              "$\\|")))))
                        non-eol-regex))) 
    final-regex)) 



(provide 'lalopmak-jump-libraries)
