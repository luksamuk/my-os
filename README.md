my-os
=======

**WARNING: This project can only be built under Rust nightly. Switch to the nightly toolchain by using `rustup default nightly`. Remember that you got to have the nightly toolchain installed as well.**

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
You can also call `my-os-backtrace` and input the faulty line on Emacs' minibuffer, so you can analize the output of the kernel's `objdump`, from the faulty line backwards, counting until 200 previous lines. This might come in handy when facing a fault that causes an immediate machine halt or reboot loop.

The [original code](https://github.com/phil-opp/blog_os) has a dual license option, being MIT or Apache2, but I'm gonna keep things simple and add an MIT License to this repository. For now, there is little to no difference between what I am doing here and the code on the original author's repository, so you can either check what I did here or read everything on Philipp Oppermann's blog linked above, which I recommend much more than mine.

Extra notes
-----------
1. I renamed `enable_nxe_bit` to `enable_nx_bit`, since the bit is called "the NX bit". I may be wrong, though, but I'll need to read more to figure it out.
2. On `memory::init`, I got confused at a certain part and managed to calculate the multiboot end manually, like it was being done at `rust_main`. But it's no big deal.
3. The [`linked_list_allocator` crate](https://github.com/phil-opp/linked-list-allocator) has a bug which requires the feature `ptr_internals` in newer versions of Rust. I'll send a pull request later, but I might also fork it and use as submodule to avoid getting stuff from crates.io.
4. I will most likely not do much anymore here, for now. The next step would be to add hardware interrupts so we can fetch stuff like keyboard events and etc, but Mr. Oppermann still hasn't written a post on the subject on the last... well, year. I still want to implement those myself, and I might need to take a look at Redox OS' code in order to do so, but for now, all I can do is a nice code cleanup of all unused imports, plus adding a _ in front of unused variables' names and whatnot. Therefore, this code should now compile without warnings.
