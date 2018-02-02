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
    (start-process "qemu" "*qemu*"
		   "qemu-system-x86_64"
		   "-d" "int"
		   "-no-reboot"
		   "-cdrom" iso
		   "-s" "-S")
    ;; We then start gdb with the kernel image symbols, and we connect to qemu
    (gdb (concat "gdb " kernel " -ex \"target remote :1234\" -i=mi"))))

(defun my-os-backtrace ()
  "Dumps previous 200 lines before a fault on the specified address"
  (interactive)
  (let* ((wd working-dir) ;; Uses the variable defined in .dir-locals.el
	 (arch "x86_64")
	 (kernel (concat wd "build/kernel-" arch ".bin"))
	 (addr (read-from-minibuffer "Input fault address: "))
	 (command (concat "objdump -d " kernel " | grep -B200 \"" addr "\"")))
    ;;(message command)
    (start-process-shell-command "objdump" "*objdump*" command)
    (split-window)
    (set-window-buffer (next-window) "*objdump*")))
