(defun my-os-debug ()
  "Debug my-os by simply typing this command."
  (interactive)
  (let* ((wd working-dir) ;; Uses the variable defined in .dir-locals.el
	 (arch "x86_64")
	 (kernel (concat wd "build/kernel-" arch ".bin"))
	 (iso (concat wd "build/os-" arch ".iso")))
    ;; GDB defaults
    (setq gdb-many-windows t)
    (setq gdb-show-main t)
    ;; First we start qemu
    (start-process "qemu" nil
		   "qemu-system-x86_64" "-cdrom" iso "-s" "-S")
    ;; We then start gdb with the kernel image symbols, and we connect to qemu
    (gdb (concat "gdb " kernel " -ex \"target remote :1234\" -i=mi"))))
