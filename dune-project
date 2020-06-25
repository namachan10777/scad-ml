(lang dune 2.6)
(name scad_ml)
(version 0.0.0)
(generate_opam_files true)

(source (github namachan10777/scad-ml))
(license BSL-1.0)
(authors "Masaki Nakano<namachan10777@gmail.com>")
(maintainers "Masaki Nakano<namachan10777@gmail.com>")
(package
  (name scad_ml)
  (synopsis "DSL for OpenSCAD")
  (description "DSL to generate OpenSCAD source codes")
  (depends
    (dune (> 2.0))))
