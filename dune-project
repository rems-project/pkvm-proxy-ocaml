(lang dune 3.0)

(generate_opam_files)

(source (github rems-project/pkvm-proxy-ocaml))
(maintainers "dk505@cl.cam.ac.uk")

(package
 (name pkvm-proxy)
 (synopsis "Issue pKVM hypercalls using pkvm-proxy")
 (description "Break VMs.")
 (depends
  (fmt (>= 0.9.0))
  (logs (>= 0.7.0))))

(package
 (name ppx-asm)
 (synopsis "Inline ASM fragments")
 (description "Enable quoted assembly, like {%asm| ASM HERE |}")
 (depends
  (fmt (>= 0.9.0))
  (ppxlib (>= 0.31.0))))
