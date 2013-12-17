;; hdevtools --- hdevtools integration with emacs
;;; Commentary:

;; This package currently only supports type information; for syntax
;; checking/compilation, see flycheck-hdevtools.

;;; Code:

(require 'cl-lib)

(cl-defstruct (hdevtools//type-info (:constructor hdevtools//make-type-info))
  start end type)

(defun hdevtools//get-type-info ()
  "Get all type information for identifiers containing point."

  (let* ((line-number (line-number-at-pos))
         (col-number (1+ (current-column)))
         (file-name (buffer-file-name))
         (hdevtools-buffer (get-buffer-create "*hdevtools*")))
    (with-current-buffer hdevtools-buffer
      (erase-buffer)
      (call-process "hdevtools" nil t nil "type" file-name
                    (number-to-string line-number)
                    (number-to-string col-number)))
    (hdevtools//parse-type-info (current-buffer) hdevtools-buffer)
    ))

(defun hdevtools//parse-type-info (haskell-buffer hdevtools-buffer)
  "Parse type information for HASKELL-BUFFER out of HDEVTOOLS-BUFFER.

Assumes the current buffer does actually have type information."
  (with-current-buffer hdevtools-buffer
    (goto-char (point-min))
    (let* ((parse-number (lambda () (forward-word) (thing-at-point 'number)))
           (line-start (funcall parse-number))
           (col-start (- (funcall parse-number) 1))
           (line-end (funcall parse-number))
           (col-end (- (funcall parse-number) 1))
           (start (hdevtools//line-col-to-pos haskell-buffer line-start
                                              col-start))
           (end (hdevtools//line-col-to-pos haskell-buffer line-end col-end)))
      (hdevtools//make-type-info
       :start start
       :end end
       :type (buffer-substring (+ 2 (point)) (1- (line-end-position)))))))

(defun hdevtools//line-col-to-pos (buffer line column)
  "Get the position in BUFFER of the given LINE and COLUMN."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char column)
      (point))))

(provide 'hdevtools)
;;; hdevtools.el ends here
