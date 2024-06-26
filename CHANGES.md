v0.7 2024-04-04 Paris (France)
------------------------------------

* Upgrade the distribution with `fmt.0.8.7` (@dinosaure, #12)
* Upgrade tests to be compatible with OCaml 5.2 (@kit-ty-kate, #14)

v0.6 2021-07-26 Paris (France)
------------------------------------

* Fix a bug when we decode contents (@dinosaure, #7)
* Lint OPAM file (@dinosaure, #8)
* Add missing dune constraint (@kit-ty-kate, @dinosaure, #9)
* Add regression tests (@dinosaure, #10)

v0.5 2020-11-26 Paris (France)
------------------------------------

* **breaking changes** The encoder flush
  at any emission of `\r\n`. It can break
  some assumptions about the behavior of
  `Pecu.encode`.
* Add empty .ocamlformat file

v0.4 2020-03-13 Paris (France)
------------------------------------

* Clean the distribution
* Delete the useless binary
* Better documentation

v0.3 2019-07-1 Paris (France)
------------------------------------

* Add `Pecu.Inline`
* Add tests
* Better documentation

v0.2 2018-11-8 Paris (France)
------------------------------------

* Clean interface
* Dunify project
* Add tests (materials come from fuzzer)

v0.1 2018-08-31 Paris (France)
------------------------------------

* First release
