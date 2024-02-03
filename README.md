# pKVM-proxy (Ocaml) #

This project contains a library interfacing with pkvm-proxy, and test
executables built on top of it that exercise the pKVM EL2 (private) API.

## Make use of ##

The project is largely self-contained.

After cloning and changing directory to it, run `opam install --deps-only .` to
install dependencies. From that point on, `dune build` rebuilds everything.

## Building ##

Since cross-compiling OCaml is currently flaky, an aarch64 system is required to
build the final executables. An emulated virtual machine is good enough. To help
with development, the project also builds on x86-64, in order to get Merlin/LSP
running.

While developing (for instance, on the host), it is enough to rebuild with
`dune build`. When building tests for deployment, build the project with
`dune build --profile=release`. This ensures, among other things, that the
executables are statically compiled.

The build produces executables named `_build/default/src-bin/*.exe`. These are
the stand-alone tests, and can be used on a (real or virtual) aarch64 system
patched with pkvm-proxy.

## Development ##

Relevant directories:

- `src-bin` contains the test binaries. Pending detailed documentation, new
  tests should looks roughly like existing ones.

  The dune file contains commented out directives which change the aarch64
  assembler. Tweaking these makes the build succeed on x86-64, depending on how
  and where the cross-toolchain is installed (and if).

- `src` contains the glue library for interfacing with pkvm-proxy. Its interface
  is somewhat documented. It can be installed switch-wide by pinning the opam
  file, or just used as part of the project.

- `src-ppx-asm` contains the syntax extension which provides inline assembly
  (`{%asm| ... |}`). It can also be installed switch-wide, or used as part of
  the project.

A simple start would be extending an existing test.
