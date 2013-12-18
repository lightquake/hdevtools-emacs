;; hdevtools --- hdevtools integration with emacs

;; Package-Requires: ((cl-lib "0.3") (dash "2.3.0"))

;;; Commentary:

;; This package currently only supports type information; for syntax
;; checking/compilation, see flycheck-hdevtools.

;;; Code:

(require 'cl-lib)
(require 'dash)

;; A type info contains a start, an end, and a string representation of the
;; type.
(cl-defstruct (hdevtools/type-info (:constructor hdevtools/make-type-info))
  start end type)

(defvar hdevtools//type-infos nil
  "Type infos currently being examined.

Repeatedly invoking `hdevtools/show-type-info' cycles through all
type infos for point; this caches the list to avoid calling
hdevtools each time.  The cache should be cleared whenever the
file is changed.")

(defvar hdevtools//type-infos-index nil
  "The index of the currently-highlighted type info, if any.

This is initially 0 when `hdevtools/show-type-info' is invoked
and increments by 1 each time, wrapping around to 0. nil
indicates no type info is being displayed.")

(defvar-local hdevtools//type-info-overlay nil
  "Overlay for the current type info.")

(defun hdevtools/show-type-info ()
  "Show type info for the identifier at point.

If already showing type info, show type info for the next largest
expression."
  (interactive)
  ;; Initialize the type information state if necessary.
  (if (not hdevtools//type-info-overlay) (hdevtools//init-type-info))
  ;; If point is outside the smallest overlay, restart.
  (let ((smallest (car hdevtools//type-infos)))
    (if (and smallest
             (or (< (point) (hdevtools/type-info-start smallest))
                 (> (point) (hdevtools/type-info-end smallest))))
        (setq hdevtools//type-infos nil)))

  ;; Load the type infos into the cache if necessary, else go to the next one.
  (if (not hdevtools//type-infos)
      (setq hdevtools//type-infos (hdevtools/get-type-infos)
            hdevtools//type-infos-index 0)
    (setq hdevtools//type-infos-index
          (mod (1+ hdevtools//type-infos-index)
               (length hdevtools//type-infos))))

  (if (not hdevtools//type-infos)
      (user-error "Can't get type information")
    (let ((tinfo (nth hdevtools//type-infos-index hdevtools//type-infos)))
      (move-overlay hdevtools//type-info-overlay
                    (hdevtools/type-info-start tinfo)
                    (hdevtools/type-info-end tinfo))
      (message "%s" (hdevtools/type-info-type tinfo)))))

(defun hdevtools//init-type-info ()
  "Perform type information-related initialization."
  (setq hdevtools//type-info-overlay (make-overlay 0 0))
  (overlay-put hdevtools//type-info-overlay 'face 'region)
  (hdevtools//clear-type-info)
  (add-hook 'after-change-functions 'hdevtools//clear-type-info))

(defun hdevtools//clear-type-info (&optional beginning end length)
  "Clear out any existing type info.

BEGINNING, END, and LENGTH are not used."
  (move-overlay hdevtools//type-info-overlay 0 0)
  (setq hdevtools//type-infos nil)
  (setq hdevtools//type-infos-index nil))

(defun hdevtools/get-type-infos ()
  "Get a list of type infos for identifiers containing point."
  (let* ((line-number (line-number-at-pos))
         ;; emacs numbers columns from 0, Haskell numbers columns from 1.
         (col-number (1+ (current-column)))
         (file-name (buffer-file-name))
         (hdevtools-buffer (get-buffer-create "*hdevtools*")))
    (with-current-buffer hdevtools-buffer
      (erase-buffer)
      (call-process "hdevtools" nil t nil "type" file-name
                    (number-to-string line-number)
                    (number-to-string col-number)))
    (hdevtools//parse-type-info (current-buffer) hdevtools-buffer)))

(defun hdevtools//parse-type-info (haskell-buffer hdevtools-buffer)
  "Parse type information for HASKELL-BUFFER out of HDEVTOOLS-BUFFER.

Assumes the current buffer does actually have type information."
  (with-current-buffer hdevtools-buffer
    (goto-char (point-min))
    (-filter 'identity (cl-loop collect (hdevtools//parse-single-type-info
                                         haskell-buffer hdevtools-buffer)
                                until (eobp) do (forward-line 1)))))

(defun hdevtools//parse-single-type-info (haskell-buffer hdevtools-buffer)
  "Parse a single line of type info for HASKELL-BUFFER out of HDEVTOOLS-BUFFER.

Assumes point is at the start of the line.  If the line contains
no type information, returns nil."
  (if (not (looking-at "[0-9]")) nil
    (let* ((parse-number (lambda () (forward-word) (thing-at-point 'number)))
           (line-start (funcall parse-number))
           (col-start (1- (funcall parse-number)))
           (line-end (funcall parse-number))
           (col-end (1- (funcall parse-number)))
           (start (hdevtools//line-col-to-pos haskell-buffer line-start
                                              col-start))
           (end (hdevtools//line-col-to-pos haskell-buffer line-end col-end)))
      (hdevtools/make-type-info
       :start start
       :end end
       ;; The +2 and 1- here are to skip over quotes.
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
