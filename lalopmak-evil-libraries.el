;;; License

;; This software is licensed under the CC0 1.0 Public Domain Declaration, as
;; released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Misc library called by lalopmak-evil.

(defvar lalopmak-evil-hintstring "Hints for lalop's colemak-evil configuration.  Accessed via: :hints, :h, :ars, or M-x lalopmak-evil-hints.

To dismiss: retype one of the above commands or press q in the buffer.

Normal mode:
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
|~ Case     |! ExtFlt>  |@ PlyMcr·  |#  <-=     |$  ->|     |% GoMatch  |^  <--     |+ Next<--  |[ Rep :s   |]  =->     |( |<-Sent  |) Sent->|  |_ LastLin  |
|` Go Mk·   |1          |2          |3          |4          |5          |6          |= Format>  |7          |8          |9          |0  |<-     |- TopLine  |
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
|           |           |           | Jump Line |           |           |           |           |           | PasteAbove|           |           |           |
|  NextTab  |           | WinCmd    | Jump Char |ChangeToEOL| Abort Cmd |           |  ▲        |   WORD    |  ▲  ScrlUp|   WORD    |           |  GoMk·|<  |
| <TAB>     |  RepState |JmpCharTill|JmpNbyChTil|Subs Line  | EOF/GotoLn|{          |  ❚        |Back2Indent|  |  5Char |   EOL     |; z-Cmd·   |\" SetReg·  |
| <TAB>     |  Replace  | Jump Char |JmpNrbyChar|Change     | Misc Cmds |[          |  ❚  PageUp|   word    |  |  Char  |   word    |: z-Cmd·   |' RunMacro |
|           |     Q     |     W     |     F     |     P     |     G     |           |     J     |  ◀▬▬▬ L   |     U     |   Y ▬▬▬▶  |           |           |
+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
 Meta-----> |SelectBlock|           |           |           |           |           |           | PasteAtBOL| PasteBelow| PasteAtEOL|           |           |
 Ctrl-----> |Select All | Redo      | Search    |           |  DelWord  |           |  ❚        |   =<Dn>   |  |  ScrlDn|   =<Tab>  |  JmpOldr  |           |
 Shift----> |Select Line| Insert BOL| Append EOL|           ||D Del->|  ||          |  ❚        |   5Char   |  |  5Char |   5Char   |O OpenUp   || GoCol1   |
 Normal---> |  Select   | Insert    | Append    |Substitute |  Delete>  |\          |  ▼  PgDown|   Char    |  ▼  Char  |   Char    |  OpenDn   |\\: (usr)·  |
 Ltr/Direc->|     A     |  ◀--R     |     S--▶  |     T     |     D     |           |     H     |  ◀--- N   |     E     |   I ---▶  |     O     |           |
            +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+
            |           |           |           |           |           |           |           |           |           |           |
            |           |           |           | Paste-Pop |           |           |           |           |           |           |    · = char arg.
            |   Redo    | Cut To EOL| Copy Line |  <-Paste  | Find File | ? <-Find§ |RpetFndBkwd|  Set Mk·  | < ◀-Dedent| > Indent-▶|    > = move arg.
            |   Undo    |   Cut->   |  Copy >   |  Paste->  |  Buffers  | / Find§-> |Repeat Find|CreateMacro| ,         | .         |
            |     Z     |     X     |     C     |     V     |     B     |           |     K     |     M     |           |           |                        
            +-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+-----------+

====Commands====

Help:
:hints = :ars = shows/dismisses this prompt (M-x lalopmak-evil-hints)
:key = describes key (C-h k)
:fun = describes function (C-h f)
:variable = describes variable (C-h v)

Shortcuts:
:relative = M-x linum-relative-toggle
:ccmode = M-x centered-cursor-mode
:comment = :c = M-x comment-or-uncomment-region
:git = M-x magit-status
:terminal = M-x sole-terminal-window = opens up terminal window, one buffer
:newterminal = M-x new-terminal-window = opens up new terminal window
:eval = :ev = Evaluates an elisp expression (C-:)
:ielm = M-x ielm-window = opens up lisp evaluation window
")



(require 'lalopmak-buffer)

(defun lalopmak-evil-hints ()
  "Provides hints about this configuration, or closes said hints."
  (interactive)
  (close-visible-buffer-else-call-helper "Colemak-Evil Hints"
                                       with-output-to-temp-buffer 
                                       (princ lalopmak-evil-hintstring)))

(defun lalopmak-evil-scroll-then-center (count motion)
  "Does a motion, then centers"
  (if count
      (funcall motion count) 
    (funcall motion 1))
  (move-to-window-line nil))

;;;;;;;;;;;; Buffer Manipulation macros/functions ;;;;;;;;;;;;;;


(defmacro terminal-command () `(ansi-term (getenv "SHELL")))

(defun new-terminal-window ()
  "Create a terminal buffer.  Can open several."
  (interactive)
  (do-in-new-buffer "*ansi-term*" (terminal-command)))

(defun sole-terminal-window ()
  "Creates or reopens a unique terminal window."
  (interactive)
  (close-visible-window-else-call-helper "Sole Terminal" 
                                       do-in-buffer 
                                       (terminal-command)))

(defun ielm-window ()
  "Open or close a visible ielm buffer."
  (interactive) 
  (close-visible-window-else-call-helper "*ielm*" 
                                       do-func-in-buffer 
                                       'ielm))

;;;Experimental for web server editing
(defmacro minor-mode-running (mode)
  `(and (boundp ',mode) ,mode))

(defun edit-server-edit-mode-running ()
  (minor-mode-running edit-server-edit-mode))

;;Resizing

(defun frame-to-top-left-corner (&optional frame)
  (interactive)
  (set-frame-position (or frame (selected-frame)) 0 0))
 
;;todo: find out how to get default 
(defun set-frame-to-default-size (&optional frame)
  (interactive)
  (set-frame-size (or frame (selected-frame)) 80 31))

(defun height-chars-to-pixels (c &optional frame)
  (* c (frame-char-height (or frame (selected-frame)))))

(defun width-chars-to-pixels (c &optional frame)
  (* c (frame-char-width (or frame (selected-frame)))))

(defun height-pixels-to-chars (p &optional frame)
  (/ p (frame-char-height (or frame (selected-frame)))))

(defun width-pixels-to-chars (p &optional frame)
  (/ p (frame-char-width (or frame (selected-frame)))))

(defun window-height-pixels (&optional frame)
  (height-chars-to-pixels (window-height) frame))

(defun window-width-pixels (&optional frame)
  (width-chars-to-pixels (window-width) frame))

(defun make-frame-tall (&optional frame)
  (interactive)
  (set-frame-size-pixels (+ 70 (window-width-pixels))
                         (display-pixel-height)
                         frame))

(defun make-frame-wide (&optional frame)
  (interactive)
  (frame-to-top-left-corner)
  (set-frame-size-pixels (display-pixel-width)
                         (+ 30 (window-height-pixels))
                         frame))

(defun set-frame-size-pixels (x y &optional frame)
  (let* ((f (or frame
                (selected-frame)))
         (x-chars (width-pixels-to-chars x f))
         (y-chars (height-pixels-to-chars y f)))
    (set-frame-size f x-chars y-chars)))
   
(defun maximize-frame (&optional frame width-buffer height-buffer)
  (interactive)
  (frame-to-top-left-corner)
  (set-frame-size-pixels (- (display-pixel-width) (or width-buffer 0))
                         (- (display-pixel-height) (or height-buffer 0))
                         frame))

(defun maximize-frame-except-some-width (&optional frame width-buffer)
  (interactive)
  (maximize-frame frame (or width-buffer 400) 0))

;; (defun fit-frame-to-buffer-width (&optional frame min-width max-width)
;;   (let* ((f (or frame (selected-frame)))
;;          (min (or min-width 80))
;;          (max (or max-width 1000))
;;          (height (window-height))
;;          (width 

(provide 'lalopmak-evil-libraries)
