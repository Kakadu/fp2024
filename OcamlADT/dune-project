(lang dune 3.7)

(generate_opam_files true)

(cram enable)

(license LGPL-3.0-or-later)

(authors "Rodion Suvorov Mikhail Gavrilenko")

(maintainers "Rodion Suvorov Mikhail Gavrilenko")

(bug_reports "https://github.com/suvorovrain/OcamlADT")

(homepage "https://github.com/suvorovrain/OcamlADT")

(package
 (name OcamlADT) ; FIXME and regenerate .opam file using 'dune build @install'
 (synopsis "An interpreter for Ocaml language with ADT")
 (description
  "An interpreter for Ocaml language with ADT.") (version 0.1)
 (depends
  dune
  angstrom
  (ppx_inline_test :with-test)
  ppx_expect
  ppx_deriving
  bisect_ppx
  (odoc :with-doc)
  (ocamlformat :build)
  base
  ; After adding dependencies to 'dune' files and the same dependecies here too
  ))
