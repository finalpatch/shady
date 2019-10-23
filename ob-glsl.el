(require 'ob)

(defvar shady-path
  (let ((exec-path (cons "." exec-path)))
    (executable-find "shady")))

(defvar org-babel-default-header-args:glsl
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a glsl source block.")

(defun org-babel-expand-body:glsl (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  body)

(defun org-babel-execute:glsl (body params)
  "Execute a block of glsl code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (or (assq :file params)
			    (error "You need to specify a :file parameter"))))
	 (in-file (org-babel-temp-file "glsl-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:glsl body params)))
    (org-babel-eval
     (concat shady-path
	     " -s " (org-babel-process-file-name in-file)
	     " -o " (org-babel-process-file-name out-file)) "")
    nil))

(defun org-babel-prep-session:shady (_session _params)
  "Return an error because glsl does not support sessions."
  (error "glsl does not support sessions"))

(provide 'ob-glsl)
