(lang dune 3.0)

(generate_opam_files)

(source (github rems-project/pkvm-proxy-ocaml))
(authors "dk505@cl.cam.ac.uk")
(maintainers "dk505@cl.cam.ac.uk")

(package
 (name pkvm-proxy)
 (synopsis "Library to issue pKVM hypercalls using pkvm-proxy")
 (description "Break VMs.")
 (depends
  (ocaml (>= 5.1.0))
  (fmt (>= 0.9.0))
  (logs (>= 0.7.0))))

(package
 (name ppx-asm)
 (synopsis "Inline ASM fragments")
 (description "Enables quoted assembly, like {%asm| ASM HERE |}")
 (depends
  (fmt (>= 0.9.0))
  (ppxlib (>= 0.31.0))))
