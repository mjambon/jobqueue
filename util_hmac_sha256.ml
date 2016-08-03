let hash signing_key =
  let hash = Cryptokit.MAC.hmac_sha256 signing_key in
  fun s ->
    Cryptokit.hash_string hash s
