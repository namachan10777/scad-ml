(lang dune 2.9)

(name scad_ml)

(version 1.0)

(generate_opam_files true)

(source
 (github namachan10777/scad-ml))

(documentation
 https://namachan10777.github.io/scad-ml/scad_ml/Scad_ml/index.html)

(license BSL-1.0)

(authors
 "Masaki Nakano<namachan10777@gmail.com>"
 "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers
 "Masaki Nakano<namachan10777@gmail.com>"
 "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name scad_ml)
 (synopsis "OCaml DSL for 3D solid modelling in OpenSCAD")
 (description
  "Scad_ml is an OCaml front-end to the OpenSCAD CAD programming language.")
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (> 2.0))
  (ppx_inline_test :with-test)))
