(** A pair of values that can be serialized to a JSON array
    consistently for use as database keys.

   Both values have to be serializable to strings in plain UTF8,
   without \u escapes. \u is only used for non-printable characters,
   which probably may not be in keys anyhow. *)

module type Key =
sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

module type Serializable =
sig
  type t
  exception Error of string
  val error : string -> 'a
  val to_string : t -> string
  val of_string : string -> t
  val of_string_opt : string -> t option
  val wrap : string -> t
  val unwrap : t -> string
end

module type Abstract_tuple2 =
sig
  type t
  module Key1 : Key
  module Key2 : Key
  val construct : Key1.t -> Key2.t -> t
  val deconstruct : t -> (Key1.t * Key2.t)
end

module type Abstract_tuple3 =
sig
  type t
  module Key1 : Key
  module Key2 : Key
  module Key3 : Key
  val construct : Key1.t -> Key2.t -> Key3.t -> t
  val deconstruct : t -> (Key1.t * Key2.t * Key3.t)
end

module Make2 (T : Abstract_tuple2) : Serializable with type t = T.t =
struct
  type t = T.t

  exception Error of string

  let error s = raise (Error s)

  let to_string x =
    let a, b = T.deconstruct x in
    Yojson.Basic.to_string
      (`List [`String (T.Key1.to_string a); `String (T.Key2.to_string b)])

  let of_string s =
    match Yojson.Basic.from_string s with
    | `List [`String key_1; `String key_2] ->
        T.construct (T.Key1.of_string key_1) (T.Key2.of_string key_2)
    | _ -> raise (Error ("Key pair failed to parse: " ^ s))

  let of_string_opt s =
    try Some (of_string s)
    with _ -> None

  let wrap = of_string
  let unwrap = to_string
end

module Make3 (T : Abstract_tuple3) : Serializable with type t = T.t =
struct
  type t = T.t

  exception Error of string

  let error s = raise (Error s)

  let to_string x =
    let a, b, c = T.deconstruct x in
    Yojson.Basic.to_string
      (`List [
         `String (T.Key1.to_string a);
         `String (T.Key2.to_string b);
         `String (T.Key3.to_string c);
       ]
      )

  let of_string s =
    match Yojson.Basic.from_string s with
    | `List [`String key_1; `String key_2; `String key_3] ->
        T.construct
          (T.Key1.of_string key_1)
          (T.Key2.of_string key_2)
          (T.Key3.of_string key_3)
    | _ -> raise (Error ("Key triple failed to parse: " ^ s))

  let of_string_opt s =
    try Some (of_string s)
    with _ -> None

  let wrap = of_string
  let unwrap = to_string
end
