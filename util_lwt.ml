open Lwt

(* Bind to 2 threads running concurrently *)
let bind2 a b func =
  a >>= fun a' ->
  b >>= fun b' ->
  func a' b'

(* Bind to 3 threads running concurrently *)
let bind3 a b c func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  func a' b' c'

(* Bind to 4 threads running concurrently *)
let bind4 a b c d func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  d >>= fun d' ->
  func a' b' c' d'

(* Bind to 5 threads running concurrently *)
let bind5 a b c d e func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  d >>= fun d' ->
  e >>= fun e' ->
  func a' b' c' d' e'

(* Combine the result of 2 threads running concurrently into a tuple *)
let join2 a b =
  bind2 a b (fun a b -> return (a, b))

(* Combine the result of 3 threads running concurrently into a tuple *)
let join3 a b c =
  bind3 a b c (fun a b c -> return (a, b, c))

(* Combine the result of 4 threads running concurrently into a tuple *)
let join4 a b c d =
  bind4 a b c d (fun a b c d -> return (a, b, c, d))

(* Combine the result of 5 threads running concurrently into a tuple *)
let join5 a b c d e =
  bind5 a b c d e (fun a b c d e -> return (a, b, c, d, e))
