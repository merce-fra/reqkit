# ppx inline test does not install properly with opam when using ocaml 5.2.0
# ocaml 5.1.0 is needed 
opam switch create ocaml.5.1.0

opam install -y dune menhir alcotest ppx_inline_test bisect_ppx ppx_expect
