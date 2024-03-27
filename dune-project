(lang dune 3.14)
(version 0.3.4)
(name lastfm)
(source (github savonet/ocaml-lastfm))
(license LGPL-2.1)
(authors "Romain Beauxis <toots@rastageeks.org>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name lastfm)
 (synopsis "The lastfm library is an implementation of the API used by the last.fm to keep count of played songs")
 (depends
  xmlplaylist
  re
  (dune (>= 2.0)))
 (depopts
  ocamlnet))
