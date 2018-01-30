my-os
=======

Operational system written in Rust.
Everything was coded on a Void Linux system under the x86\_64 architecture, so don't expect it to work on other kinds of systems.

For now, all the important things are following this guide: https://os.phil-opp.com/

To compile, you'll need the following dependencies:
- Rust (Nightly compiler preferable; install using `rustup`);
- `cargo`;
- `xargo` (installable using `cargo`);
- `nasm`, to build our kernel image for x86_64;
- `grub-mkrescue` and `xorriso`, to build an ISO image;
- `qemu`, to run the ISO;
- `gdb`, to debug while running on `qemu`;
- Optionally `Emacs`, which is my main editor.

If you're using Emacs, I've included some scripts, so you can use `M-x my-os-debug` to debug directly on Emacs with GDB/GUD. You probably don't need to, but you can edit the `.dir-locals.el` and `local-funs.el` files to adjust to your system.

The [original code](https://github.com/phil-opp/blog_os) has a dual license option, being MIT or Apache2, but I'm gonna keep things simple and add an MIT License to this repository. For now, there is little to no difference between what I am doing here and the code on the original author's repository, so you can either check what I did here or read everything on Philipp Oppermann's blog linked above, which I recommend much more than mine.
