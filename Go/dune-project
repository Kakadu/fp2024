(lang dune 3.7)

(generate_opam_files true)

(cram enable)

(license MIT)

(authors
 "Karim Shakirov <karimshak06@gmail.com>"
 "Alexei Dmitrievtsev <dmt.alex2005@gmail.com>")

(maintainers
 "Karim Shakirov <karimshak06@gmail.com>"
 "Alexei Dmitrievtsev <dmt.alex2005@gmail.com>")

(bug_reports "https://github.com/kar1mgh/go-interpreter")

(homepage "https://github.com/kar1mgh/go-interpreter")

(package
 (name Go)
 (synopsis "An interpreter for a subset of Go")
 (description
  "Our interpeter supports:\n  - int, bool and string data types\n  - for cycles\n  - if operator\n  - arrays\n  - functions (including recursive and closures)\n  - channels\n  - goroutines")
 (version 0.1)
 (depends
  dune
  (ppx_inline_test :with-test)
  ppx_expect
  angstrom
  qcheck
  ppx_deriving
  bisect_ppx
  (odoc :with-doc)
  (ocamlformat :build)
  base))
