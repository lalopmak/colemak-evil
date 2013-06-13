;;; License

;; This software is released under the CC0 1.0 Universal license. You are
;; free to use, modify, and redistribute it as you please. This software
;; comes with NO WARRANTIES OR GUARANTEES WHATSOEVER. For details, see
;; http://creativecommons.org/publicdomain/zero/1.0/


;;Buffer helper commands/macros


(defmacro if-visible-buffer (bufferName then-stmt else-stmt)
  "If buffer by that name is visible, then-statement, else, else-stmt."
  `(let ((buffer (get-buffer ,bufferName)))
     (if (and buffer 
              (get-buffer-window buffer)) 
         ,then-stmt
       ,else-stmt)))


;;Pattern: Closing visible thing or else doing something (most likely starting up one)

(defmacro close-visible-buffer-else-do (bufferName &rest body)
  "If buffer by that name is visible, close it and kill the buffer.  Otherwise, execute body."
  `(if-visible-buffer ,bufferName
                      (progn (delete-windows-on ,bufferName)
                             (kill-buffer ,bufferName))
                      ,@body))

(defmacro close-visible-window-else-do (bufferName &rest body)
  "If buffer by that name is visible, close it.  Otherwise, execute body."
  `(if-visible-buffer ,bufferName
                      (delete-windows-on ,bufferName)
                      ,@body))

;;Helper functions/macros

(defmacro do-in-new-buffer (bufferName &rest body)
  "Splits current window, call it bufferName (or unique variant thereof), execute body in the buffer"
  `(progn (split-window-sensibly (selected-window))
          (other-window 1)
          ,@body
          (rename-buffer ,bufferName 'make-unique)))

(defmacro do-in-buffer (bufferName &rest body)
  "If the buffer already exists, open it up in a window.  Otherwise, execute body in new buffer."
  `(if (get-buffer ,bufferName)
       (switch-to-buffer-other-window ,bufferName)
       (do-in-new-buffer ,bufferName ,@body)))
 
(defun do-func-in-new-buffer (bufferName func)
  "Splits current window, call it bufferName (or unique variant thereof), execute func in the buffer"
 (do-in-new-buffer bufferName (funcall func))) 
 
(defun do-func-in-buffer (bufferName func) 
  "If the buffer already exists, open it up in a window.  Otherwise, execute func in new buffer."
 (do-in-buffer bufferName (funcall func))) 
;;;;;;;;;;;;;
;;Pattern: closing visible then calling a helper on the same name
;;These need to go BELOW the helpers above, or else they won't be recognized

(defmacro close-visible-buffer-else-call-helper (bufferName helper &rest body)
  "Closes the visible window or, if closed, calls the helper function/macro with that body"
  `(close-visible-buffer-else-do ,bufferName
                               (,helper ,bufferName ,@body)))


(defmacro close-visible-window-else-call-helper (bufferName helper &rest body)
  "Closes the visible window or, if closed, calls the helper function/macro with that body"
  `(close-visible-window-else-do ,bufferName
                               (,helper ,bufferName ,@body)))

;;;;;;;;;;;;;

(provide 'lalopmak-buffer)
