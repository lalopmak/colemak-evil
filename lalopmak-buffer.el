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

(defmacro close-visible-buffer-or-do (bufferName &rest body)
  "If buffer by that name is visible, close it and kill the buffer.  Otherwise, execute body."
  `(if-visible-buffer ,bufferName
                      (progn (delete-windows-on ,bufferName)
                             (kill-buffer ,bufferName))
                      ,@body))

(defmacro close-visible-window-or-do (bufferName &rest body)
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
  `(if (not (get-buffer ,bufferName))
       (do-in-new-buffer ,bufferName ,@body)
     (switch-to-buffer-other-window ,bufferName)))
 
(defun do-func-in-new-buffer (bufferName func)
  "Splits current window, call it bufferName (or unique variant thereof), execute func in the buffer"
 (do-in-new-buffer bufferName (funcall func))) 
 
(defun do-func-in-buffer (bufferName func) 
  "If the buffer already exists, open it up in a window.  Otherwise, execute func in new buffer."
 (do-in-buffer bufferName (funcall func))) 


(provide 'lalopmak-buffer)
