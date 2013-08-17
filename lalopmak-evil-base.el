
;;  Colemak Evil: A set of optimized Vim-like key bindings for Emacs.
;;  Copyright (C) 2013 Patrick Brinich-Langlois

;;  lalopmak-evil: A more geometric fork.
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


;;Commonalities to both lalopmak-evil and minimalistic-lalopmak-evil
(require 'lalopmak-evil-libraries)
(require 'lalopmak-layouts)
(require 'lalopmak-jump)


;; we're using the colemak layout by default
(if (not (boundp 'lalopmak-layout-map))
    (setq lalopmak-layout-map 'colemak-to-colemak)) 

(defmacro lalopmak-evil-define-key (keymap key def &optional modes)
  "Defines key given the lalopmak-evil keymap, in accordance to the lalopmak-layout-map"
  `(define-key ,keymap (key-to-layout ,key lalopmak-layout-map) ,def))

;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil) 
;; but [escape] should switch back to normal state
(lalopmak-evil-define-key evil-insert-state-map [escape] 'evil-normal-state) 

;; make undo more incremental (break into smaller chunks)
(setq evil-want-fine-undo t)

(defmacro lalopmak-evil-local-set-key (keymap key)
  "Defines local key given the lalopmak-evil keymap, in accordance to the lalopmak-layout-map"
  `(local-set-key ,keymap (key-to-layout ,key lalopmak-layout-map)))



;; map multiple states at once (courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674)
(defun set-in-all-evil-states (key def &optional maps)
  (unless maps
    (setq maps (list evil-normal-state-map
                     evil-visual-state-map
                     evil-insert-state-map
                     evil-emacs-state-map
		     evil-motion-state-map)))
  (while maps
    (lalopmak-evil-define-key (pop maps) key def)))


(defun set-in-all-evil-states-but-insert (key def)
  (set-in-all-evil-states key 
                          def 
                          (list evil-normal-state-map
                                evil-visual-state-map
                                evil-emacs-state-map
                                evil-motion-state-map)))

(defun set-in-all-evil-states-but-insert-and-motion (key def)
  (set-in-all-evil-states key
                          def 
                          (list evil-normal-state-map
                                evil-visual-state-map
                                evil-emacs-state-map)))

(defun lalopmak-evil-unmap (char map)
  "Given association list map, deletes all entries with key char"
  (setq map (assq-delete-all char map)))

(defun lalopmak-evil-unmap-motion (char)
  "Deletes all entries in evil-motion-state-map with key char"
  (lalopmak-evil-unmap char evil-motion-state-map))

;; Experiment: make space into a "leader" key

;;default: space does one space, unless remapped in a mode
(lalopmak-evil-define-key evil-motion-state-map " " (lambda () (interactive) (insert " ")))



;; (lalopmak-evil-define-key evil-motion-state-map " (" 'evil-previous-open-paren)
;; (lalopmak-evil-define-key evil-motion-state-map " )" 'evil-next-close-paren)
;; (lalopmak-evil-define-key evil-motion-state-map " {" 'evil-previous-open-brace)
;; (lalopmak-evil-define-key evil-motion-state-map " }" 'evil-next-close-brace)

(defvar lalopmak-evil-lisp-mode-map-symbols '(emacs-lisp-mode-map lisp-mode-map lisp-interaction-mode-map))

(defmacro lalopmak-evil-define-mode-bindings (state-symbols mode-symbols &rest bindings)
  "Given lists of state-symbols and mode-symbols, as well as some number of key bindings,
binds them via evil-define-key for those states in those modes."
  `(mapc (lambda (mode-symbol) 
           (mapc (lambda (state-symbol)
                   (evil-define-key state-symbol (symbol-value mode-symbol) ,@bindings))
                 ,state-symbols))
         ,mode-symbols))

(defmacro lalopmak-evil-define-lisp-motions (&rest bindings)
  "For each lisp mode map represented in lalopmak-evil-lisp-mode-map-symbols,

adds 'motion bindings to that lisp mode map."
    `(lalopmak-evil-define-mode-bindings '(motion) lalopmak-evil-lisp-mode-map-symbols ,@bindings))

(lalopmak-evil-define-lisp-motions "  " (lambda () (interactive) (insert " "))  ;;two spaces for a space


                                   " r(" 'paredit-wrap-sexp
                                   " r{" 'paredit-wrap-curly
                                   " r[" 'paredit-wrap-square
                                   " r<" 'paredit-wrap-angled

                                   " (" 'paredit-open-round
                                   " {" 'paredit-open-curly
                                   " [" 'paredit-open-square
                                   " <" 'paredit-open-angled

                                   " )" 'paredit-close-sexp
                                   " }" 'paredit-close-curly
                                   " ]" 'paredit-close-square
                                   " >" 'paredit-close-angled

                                   ;;navigation on the inside
                                   " l" 'paredit-backward-up
                                   " n" 'paredit-backward
                                   " k" 'paredit-backward-down

                                   " y" 'paredit-forward-up
                                   " i" 'paredit-forward
                                   " ." 'paredit-forward-down

                                   " j" 'paredit-backward-barf-sexp
                                   " h" 'paredit-backward-slurp-sexp

                                   " ;" 'paredit-forward-barf-sexp
                                   " o" 'paredit-forward-slurp-sexp

                                   ;;navigation on the outside
                                   ;; " j" 'paredit-backward-up
                                   ;; " h" 'paredit-backward
                                   ;; " k" 'paredit-backward-down

                                   ;; " ;" 'paredit-forward-up
                                   ;; " o" 'paredit-forward
                                   ;; " ." 'paredit-forward-down

                                   ;; " n" 'paredit-backward-slurp-sexp
                                   ;; " l" 'paredit-backward-barf-sexp

                                   ;; " y" 'paredit-forward-barf-sexp
                                   ;; " i" 'paredit-forward-slurp-sexp

                                   " e" 'paredit-join-sexps
                                   " u" 'paredit-split-sexp

                                   " q" 'raise-sexp
                                   " w" 'paredit-splice-sexp-killing-backward
                                   " f" 'paredit-splice-sexp
                                   " p" 'paredit-splice-sexp-killing-forward
                                   " g" 'paredit-convolute-sexp)

;;; Make the return and backspace keys work in normal mode
;; Backspace in normal mode doesn't work in the terminal.
(lalopmak-evil-define-key evil-motion-state-map (kbd "RET") (lambda () (interactive) (newline)))
(lalopmak-evil-define-key evil-motion-state-map (kbd "<backspace>") 'delete-backward-char)


(set-in-all-evil-states (kbd "C-r") 'isearch-backward)

;;expand-region
(set-in-all-evil-states (kbd "C-f") 'er/expand-region)
(setq expand-region-contract-fast-key "w")

(evil-define-motion lalopmak-evil-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-end count move nil t)))

(evil-define-motion lalopmak-evil-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (lalopmak-evil-forward-word-end count t))

(evil-define-motion lalopmak-evil-backward-word-begin (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-beginning (- (or count 1)) move)))

(evil-define-motion lalopmak-evil-backward-WORD-begin (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (lalopmak-evil-backward-word-begin count t))

;; (evil-define-motion lalopmak-evil-forward-char (count &optional crosslines noerror)
;;   "Forward character, allowing you to fall to the next line"
;;   :type exclusive
;;   (if (and (boundp 'paredit-mode) paredit-mode)
;;       (paredit-forward)
;;     (evil-forward-char count 'crosslines noerror)))

;; (evil-define-motion lalopmak-evil-backward-char (count &optional crosslines noerror)
;;   "Backward character, allowing you to rise to the previous line"
;;   (if (and (boundp 'paredit-mode) paredit-mode)
;;       (paredit-backward)
;;     (evil-backward-char count 'crosslines noerror)))

;;Makes these compatible with undo-tree
(when (boundp 'undo-tree-visualizer-mode-map)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-evil-backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-evil-forward-char]
    'undo-tree-visualize-switch-branch-right))


(defmacro create_lalopmak-evil-if-count-else (then-name else-name docstring metadata then &rest else)
  "Creates evil-motion lalopmak-evil-if-count-[then-name]-else-[else-name]
metadata should be a list, e.g. (:type line :repeat abort) or nil"
  `(evil-define-motion ,(intern (format "lalopmak-evil-if-count-%s-else-%s" then-name else-name)) (count)
     ,docstring
     ,@metadata
     (if count
         ,then
       ,@else)))

(defmacro create_lalopmak-evil-if-count-goto-line-else (else-name docstring metadata &rest else)
  "Creates evil-motion lalopmak-evil-if-count-goto-line-else-[else-name]
which goes to line-number count if it exists, otherwise executes else.

metadata should be a list, e.g. (:type line :repeat abort) or nil"
  `(create_lalopmak-evil-if-count-else "goto-line"
                                       ,else-name
                                       ,docstring
                                       ,metadata
                                       (evil-goto-line count)
                                       ,@else))

;;creates lalopmak-evil-if-count-goto-line-else-open-below
(create_lalopmak-evil-if-count-goto-line-else "open-below" 
                                              "if count goes to line, otherwise opens below"
                                              nil
                                              (evil-open-below 1))

;;creates lalopmak-evil-if-count-goto-line-else-ace-jump-line-mode
(create_lalopmak-evil-if-count-goto-line-else "ace-jump-line-mode" 
                                              "if count goes to line, otherwise ace-jump line"
                                              (:type line :repeat abort)
                                              (lalopmak-evil-ace-jump-line-mode))

(defun lalopmak-evil-write (beg end &optional type filename bang)
  (if  (and (boundp 'edit-server-edit-mode) edit-server-edit-mode)  
      (edit-server-save) 
    (evil-write beg end type filename bang)))

;; (defadvice evil-write (around check-edit-server 
;;                               (beg end &optional type filename bang))
;;   (if (edit-server-edit-mode-running) 
;;       (edit-server-save) 
;;     ad-do-it))

;;;;;;;;;;;;;;;;;; Custom : commands ;;;;;;;;;;;;;;;;;;;;;;;


(evil-ex-define-cmd "f[ile]" 'ido-find-file)
(evil-ex-define-cmd "b[uffer]" 'ido-switch-buffer)

;;hooks for quitting/saving commands
(evil-ex-define-cmd "w[rite]" 'evil-write)

;;git
(evil-ex-define-cmd "git" 'magit-status)

;;linum relative toggle
(evil-ex-define-cmd "relative" 'linum-relative-toggle)
(evil-ex-define-cmd "ccmode" 'centered-cursor-mode)


;;comment
(evil-ex-define-cmd "comment" 'comment-or-uncomment-region)
(evil-ex-define-cmd "c" "comment")

;;M-:
(evil-ex-define-cmd "eval" 'eval-expression)
(evil-ex-define-cmd "ev" "eval")
(evil-ex-define-cmd "ielm" 'ielm-window)
(evil-ex-define-cmd "interactive-eval" "ielm")

;;Terminal
(evil-ex-define-cmd "terminal" 'sole-terminal-window)
(evil-ex-define-cmd "newterminal" 'new-terminal-window)


;;C-h k
(evil-ex-define-cmd "describe-key" 'describe-key)
(evil-ex-define-cmd "key" "describe-key")

;;C-h f
(evil-ex-define-cmd "describe-function" 'describe-function)
(evil-ex-define-cmd "function" "describe-function")
(evil-ex-define-cmd "fun" "describe-function")

;;C-h v

(evil-ex-define-cmd "describe-variable" 'describe-variable)
(evil-ex-define-cmd "variable" "describe-variable")

;;M-x speck-mode (spell checking)

(evil-ex-define-cmd "spell" 'speck-mode)

;; M-x keyfreq mode (key frequency analysis)

(evil-ex-define-cmd "keyfreq" 'keyfreq-show)

;;Ya-snippets

;; inserts yasnippet "around" the visual mode selection, where applicable.
;; works with yas-wrap-around-region, or by inserting `yas/selected-text`
;; (with those quotations) at select point in snippet
(lalopmak-evil-define-key evil-visual-state-map (kbd "<tab>") 'yas-insert-snippet)

;;workaround for Issue #254
(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (evil-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (evil-insert-state)
                  (goto-char p)
                  (set-mark m)))))

(defun evil-snippet (name)
  (interactive "sSnippet shortcut:")
  (evil-insert 1)
  (insert (concat " " name))
  (yas-expand))

(evil-ex-define-cmd "snippet" 'evil-snippet)

;;Frame sizes

;; (evil-ex-define-cmd "fit" 'fit-frame-to-buffer)


(evil-define-motion lalopmak-evil-stretch (count)
  "Stretches the frame count times"
   (stretch-frame count))

(evil-define-motion lalopmak-evil-unstretch (count)
  "Unstretches the frame count times"
   (unstretch-frame count))

(evil-ex-define-cmd "stretch" 'lalopmak-evil-stretch)
(evil-ex-define-cmd "unstretch" 'lalopmak-evil-unstretch)
(evil-ex-define-cmd "wide" 'make-frame-wide)

(evil-define-motion lalopmak-evil-grow (count)
  "Growes the frame count times"
   (grow-frame count))

(evil-define-motion lalopmak-evil-shrink (count)
  "Shrinkes the frame count times"
   (shrink-frame count))

(evil-ex-define-cmd "grow" 'lalopmak-evil-grow)
(evil-ex-define-cmd "shrink" 'lalopmak-evil-shrink)
(evil-ex-define-cmd "tall" 'make-frame-tall)

(evil-ex-define-cmd "small" 'set-frame-to-default-size)
(evil-ex-define-cmd "large" 'maximize-frame-except-some-width)
(evil-ex-define-cmd "fullscreen" 'maximize-frame)

(evil-ex-define-cmd "corner" 'frame-to-top-left-corner)


;; (ad-activate-all)  ;activates all advice

;;FRAGILE
;;Redefines visual updates so as to update the primary, rather than the clipboard, with the selection
;;This also allows you to select a region, copy from outside, then paste into the region
(defun evil-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (evil-visual-state-p)
                   (fboundp 'x-select-text)
                   (or (not (boundp 'ns-initialized))
                       (with-no-warnings ns-initialized))
                   (not (eq evil-visual-selection 'block)))
          (x-set-selection 'PRIMARY (buffer-substring-no-properties
                                     evil-visual-beginning
                                     evil-visual-end))
          (setq x-last-selected-text-primary ))))))

(provide 'lalopmak-evil-base)
