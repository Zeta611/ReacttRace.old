(lang dune 3.7)

(name react_trace)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name react_trace)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(map_workspace_root false)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
