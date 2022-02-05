(lang dune 2.9)
(using mdx 0.1)
(generate_opam_files true)
(source (github patricoferris/ocaml-cid))
(authors "patrick@sirref.org")
(maintainers "patrick@sirref.org")
(package
 (name cid)
 (synopsis "Content-addressed Identifiers")
 (depends
  multibase
  multicodec
  multihash-digestif
  cstruct))