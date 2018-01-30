;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil .
      ;; Evaluate our working directory.
      ;; The directory which contains this file is the
      ;; working directory.
      ((eval . (setq working-dir
		     (file-name-directory
		      (let ((d (dir-locals-find-file ".")))
			(if (stringp d) d (car d))))))
       ;; Loads local-funs.el.
       (eval . (load-file
		(concat working-dir "local-funs.el"))))))
