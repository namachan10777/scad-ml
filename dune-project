(lang dune 2.9)

(cram enable)

(name scad_ml)

(generate_opam_files true)

(source
 (github namachan10777/scad-ml))

(documentation https://namachan10777.github.io/scad-ml)

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
  (cairo2
   (>= 0.6.2))))
