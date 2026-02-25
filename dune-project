(lang dune 3.9)

(implicit_transitive_deps false)

(generate_opam_files true)

(formatting
 (enabled_for ocaml reason))

(cram enable)

(name monorobot)

(using menhir 2.0)

(license "MIT")

(version "0.1")

(source
 (github ahrefs/monorobot))

(authors "Ahrefs Pte Ltd <github@ahrefs.com>")

(maintainers "Ahrefs Pte Ltd <github@ahrefs.com>")

(package
 (name monorobot)
 (synopsis "Notification bot for monorepos")
 (description
  "Notification bot to handle webhook events from monorepos and post notifications to Slack.")
 (depends
  (ocaml
   (>= 4.14.0))
  (atd
   (>= 2.14.0))
  (atdgen
   (>= 2.14.0))
  (atdgen-runtime
   (>= 2.14.0))
  (base64
   (>= 3.0.0))
  biniou
  (cmdliner
   (>= 1.1.0))
  (digestif
   (>= 1.2.0))
  (devkit
   (>= 1.20240429))
  (extlib
   (>= 1.7.8))
  (lwt
   (and (>= 5.7.0) (< 6.0.0)))
  (lwt_ppx
   (and (>= 5.7.0) (< 6.0.0)))
  (ptime
   (>= 1.2.0))
  (ocamldiff
   (>= 1.2))
  (sqlite3
   (>= 5.1.0))
  (sqlgg
   (>= 20231201))
  re
  uri
  (omd
   (< 2))
  yojson
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.26.2)))
  (curl
   (>= 0.10.0))
  (sexplib0
   (>= v0.16.0))
  (x509
   (>= 1.0.6))
  (mirage-crypto-rng
   (>= 2.0.2))
  (mirage-crypto-pk
   (>= 2.0.2))))
