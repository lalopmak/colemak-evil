
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


(require 'lalopmak-evil-base)

;;; Up/down/left/right
(set-in-all-evil-states-but-insert "h" 'lalopmak-evil-backward-char)
(set-in-all-evil-states-but-insert "l" 'lalopmak-evil-forward-char)

(set-in-all-evil-states-but-insert "e" 'evil-previous-line)
(set-in-all-evil-states-but-insert "\C-e" 'evil-scroll-up)
(set-in-all-evil-states-but-insert "n" 'evil-next-line)
(set-in-all-evil-states-but-insert "\C-n" 'evil-scroll-down)

(lalopmak-evil-define-key evil-motion-state-map "k" 'evil-search-next)
(lalopmak-evil-define-key evil-motion-state-map "K" 'evil-search-previous)

;;Ace jump
(set-in-all-evil-states-but-insert "f" 'lalopmak-evil-ace-jump-char-mode)
(set-in-all-evil-states-but-insert "F" 'lalopmak-evil-ace-jump-char-to-mode)
(set-in-all-evil-states-but-insert "t" 'evil-ace-jump-char-mode)
(set-in-all-evil-states-but-insert "T" 'evil-ace-jump-char-to-mode)
;; (set-in-all-evil-states "\C-f" 'evil-ace-jump-char-mode)

(when (fboundp 'undo-tree-undo)
  (lalopmak-evil-define-key evil-normal-state-map "u" 'undo-tree-undo)
  (lalopmak-evil-define-key evil-normal-state-map "U" 'undo-tree-redo))


;;; Make the space, return, and backspace keys work in normal mode
;; Backspace in normal mode doesn't work in the terminal.
(lalopmak-evil-define-key evil-motion-state-map " " (lambda () (interactive) (insert " ")))
(lalopmak-evil-define-key evil-motion-state-map (kbd "RET") (lambda () (interactive) (newline)))
(lalopmak-evil-define-key evil-motion-state-map (kbd "<backspace>") 'delete-backward-char)

;;Line jump
(set-in-all-evil-states-but-insert "j" 'evil-ace-jump-line-mode) ;temporary assignment

;; Spare keys:
;; x 

(set-in-all-evil-states-but-insert "x" 'ido-switch-buffer)
(set-in-all-evil-states-but-insert "X" 'ido-find-file)

(set-in-all-evil-states-but-insert ";" 'evil-ex)
(set-in-all-evil-states-but-insert "," 'ido-switch-buffer)

(provide 'lalopmak-evil-mnemonic)
