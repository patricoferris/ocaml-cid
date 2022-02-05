module Md = Multihash_digestif

type version = [ `Cidv1 | `Cidv2 | `Cidv3 ] (* Compatible with Multicodec.ipld *)

let version_of_ipld = function
  | `Cidv1 | `Cidv2 | `Cidv3 as v -> v
  | _ -> invalid_arg "Expected a CID version"

type t = {
  version : version;
  base : Multibase.Encoding.t;
  codec : Multicodec.t;
  hash : Md.t;
}

let v ~version ~base ~codec ~hash = { version; base; codec; hash }

let version t = t.version
let base t = t.base
let codec t = t.codec
let hash t = t.hash

let equal a b =
  a.version = b.version &&
  a.base = b.base &&
  a.codec = b.codec &&
  Md.equal a.hash b.hash

let of_cstruct ~base buf =
  let l = Cstruct.length buf in
  let v, off = Uvarint.decode buf in
  let c, off' = Uvarint.decode Cstruct.(sub buf off (l - off)) in
  match Md.of_cstruct Cstruct.(sub buf (off + off') (l - off - off')) with
    | Ok hash ->
      let version = version_of_ipld (Option.get @@ Multicodec.ipld_of_code v) in
      let codec = Option.get @@ Multicodec.of_code c in
      { version; codec; base; hash }
    | Error (`Msg m) -> failwith m

let to_cstruct { version; base; codec; hash } =
  let enc = Multibase.Encoding.to_code base |> fun s -> String.get_uint8 s 0 in
  let ver = Multicodec.ipld_to_code (version :> Multicodec.ipld) in
  let cod = Multicodec.to_code codec in
  let has = Md.to_cstruct hash in
  let buf = Cstruct.create 3 in
  Cstruct.set_uint8 buf 0 enc;
  Cstruct.set_uint8 buf 1 ver;
  Cstruct.set_uint8 buf 2 cod;
  Cstruct.append buf has

let to_string t =
  let buf = to_cstruct t in
  let data = Cstruct.(to_string (sub buf 1 (length buf - 1))) in
  Multibase.encode t.base data

let of_string s = 
  match Multibase.decode s with
  | Ok (base, s) -> 
    of_cstruct ~base (Cstruct.of_string s)
  | Error (`Msg m) -> failwith m


let pp_human ppf { version; base; codec; hash } =
  Fmt.pf ppf "%s - %s - %s - %a" 
  (Multicodec.ipld_to_string (version :> Multicodec.ipld))
  (Multibase.Encoding.to_string base)
  (Multicodec.to_string codec)
  Md.pp hash


