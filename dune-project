(lang dune 3.0)

(name ocamlot)

(package
 (name ocamlot)
 (synopsis "OCaml Trading System")
 (description "A comprehensive trading system with order management, risk checks, FIX protocol, and market data")
 (depends
  ocaml
  dune
  base
  core
  (lwt (>= 5.0.0))
  (lwt_ppx (>= 2.0.0))
  (yojson (>= 2.0.0))
  (ppx_deriving (>= 5.0))
  (mtime (>= 1.4.0))
  logs
  fmt
  cmdliner
  alcotest
  alcotest-lwt
  ppx_deriving_yojson))

(generate_opam_files true)