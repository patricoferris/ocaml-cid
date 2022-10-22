type version = [ `Cidv0 | `Cidv1 | `Cidv2 | `Cidv3 ]

type t
(** A content-addressed identifier. *)

val v :
  version:version ->
  base:Multibase.Encoding.t ->
  codec:Multicodec.t ->
  hash:Cstruct.t Multihash_digestif.t ->
  t
(** Build a CID, this performs no checks on any of the inputs *)

val version : t -> version
(** The CID version. *)

val base : t -> Multibase.Encoding.t
(** The multibase encoding of the CID. *)

val codec : t -> Multicodec.t
(** The multicodec type of the data *)

val hash : t -> Cstruct.t Multihash_digestif.t
(** The digest of the CID *)

val equal : t -> t -> bool
(** Tests the equality of two CIDs. *)

val of_string :
  string ->
  (t, [ `Msg of string | `Unsupported of Multibase.Encoding.t ]) result
(** [of_string s] takes an encoded string [s] that is the CID and 
    pulls out each of the parts that make it up. *)

val of_cstruct :
  base:Multibase.Encoding.t ->
  Cstruct.t ->
  (t, [ `Msg of string | `Unsupported of Multibase.Encoding.t ]) result
(** [of_cstruct ~base buf] builds a value representing a CID. The buffer 
    should not be encoded with the multibase encoding. *)

val to_string :
  t ->
  (string, [ `Msg of string | `Unsupported of Multibase.Encoding.t ]) result
(** [to_string t] converts the CID to a multibase encoded string. Errors happen
    if the base encoding is not supported. TODO: change to exception. *)

val to_cstruct : t -> Cstruct.t
(** [to_cstruct t] returns a buffer with the bytes corresponding to the unencoded 
    CID. *)

val pp_human : Format.formatter -> t -> unit
(** Pretty-prints a CID. *)
