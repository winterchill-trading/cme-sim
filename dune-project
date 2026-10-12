(lang dune 3.17)

(name cme-sim)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/cme-sim))

(package
 (name cme-sim)
 (synopsis
  "CME Globex futures simulator speaking iLink 3 and MDP 3.0 SBE over TCP")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  cohttp-async
  http
  async_websocket
  yojson
  fixed-decimal
  exchange-core
  exchange-core-engine
  equities-core
  order-book
  ppx_jane))
