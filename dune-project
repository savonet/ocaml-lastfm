(lang dune 3.14)
(version 0.3.4)
(name lastfm)
(source (github savonet/ocaml-lastfm))
(license LGPL-2.1-or-later)
(authors "Romain Beauxis <toots@rastageeks.org>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name lastfm)
 (synopsis "The lastfm library is an implementation of the API used by the last.fm to keep count of played songs")
 (depends
  xmlplaylist
  (re (>= 1.11.0)))
 (depopts
  ocamlnet))
