
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


(evil-define-motion lalopmak-evil-forward-char (count &optional crosslines noerror)
  "Forward character, allowing you to fall to the next line"
  :type exclusive
  (if (and (boundp 'paredit-mode) paredit-mode)
      (paredit-forward)
    (evil-forward-char count 'crosslines noerror)))

(evil-define-motion lalopmak-evil-backward-char (count &optional crosslines noerror)
  "Backward character, allowing you to rise to the previous line"
  (if (and (boundp 'paredit-mode) paredit-mode)
      (paredit-backward)
    (evil-backward-char count 'crosslines noerror)))

;;Makes these compatible with undo-tree
(when (boundp 'undo-tree-visualizer-mode-map)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-evil-backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-mode-map [remap lalopmak-evil-forward-char]
    'undo-tree-visualize-switch-branch-right))


(provide 'lalopmak-evil-base)
