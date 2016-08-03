let hash s =
  Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
