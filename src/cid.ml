module Md = Multihash_digestif

type version = [ `Cidv0 | `Cidv1 | `Cidv2 | `Cidv3 ] (* Compatible with Multicodec.ipld *)

let version_string = function
  | `Cidv0 -> "Cidv0"
  | `Cidv1 | `Cidv2 | `Cidv3 as v -> Multicodec.ipld_to_string v

let version_of_ipld = function
  | `Cidv1 | `Cidv2 | `Cidv3 as v -> v
  |  _ -> invalid_arg "Expected a CID version"

type t = 
 {
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

let cidv0_of_string ~base buf =
  match Md.of_cstruct buf with
  | Ok hash -> { version = `Cidv0; base; codec = `Dag_pb; hash }
  | Error (`Msg m) -> failwith ("Failed parsing Cidv0: " ^ m)

let of_cstruct ~base buf =
  let l = Cstruct.length buf in
  if l = 34 && Cstruct.get_uint8 buf 0 = 0x12 && Cstruct.get_uint8 buf 1 = 0x20 
  then cidv0_of_string ~base buf
  else
  let v, off = Uvarint.decode buf in
  let c, off' = Uvarint.decode Cstruct.(sub buf off (l - off)) in
  match Md.of_cstruct Cstruct.(sub buf (off + off') (l - off - off')) with
    | Ok hash ->
      let version = version_of_ipld (Option.get @@ Multicodec.ipld_of_code v) in
      let codec = Option.get @@ Multicodec.of_code c in
      { version; codec; base; hash }
    | Error (`Msg m) -> failwith m

let ( <+> ) = Cstruct.append
  
let to_cstruct { version; base; codec; hash } = 
  match version with
  | `Cidv0 -> (
    match base with
    | `Base58btc ->
       let hash = Md.to_cstruct hash in
       let b = Multibase.Base58.encode (Cstruct.to_string hash) in
       Cstruct.of_string b
    | _ ->
      let hash = Md.to_cstruct hash in
      let b = Multibase.encode base (Cstruct.to_string hash) |> Result.get_ok in
      Cstruct.of_string b
  )
  | `Cidv1 | `Cidv2 | `Cidv3 as version ->
  let enc = Multibase.Encoding.to_code base |> fun s -> String.get_uint8 s 0 in
  let ver = Multicodec.ipld_to_code (version :> Multicodec.ipld) |> Uvarint.encode in
  let cod = Multicodec.to_code codec |> Uvarint.encode in
  let has = Md.to_cstruct hash in
  let buf = Cstruct.create 1 in
  Cstruct.set_uint8 buf 0 enc;
  buf <+> ver <+> cod <+> has

let to_string t =
  let buf = to_cstruct t in
  if t.version = `Cidv0 then Ok (Cstruct.to_string buf)
  else
  let data = Cstruct.(to_string (sub buf 1 (length buf - 1))) in
  Multibase.encode t.base data

let of_string s =
  if String.length s = 46 && s.[0] = 'Q' && s.[1] = 'm' 
  then of_cstruct ~base:`Base58btc (Cstruct.of_string @@ Multibase.Base58.decode s)
  else
  match Multibase.decode s with
  | Ok (base, s) ->
    of_cstruct ~base (Cstruct.of_string s)
  | Error (`Msg m) -> failwith m

let pp_human ppf { version; base; codec; hash } =
  Fmt.pf ppf "%s - %s - %s - %a" 
  (version_string version)
  (Multibase.Encoding.to_string base)
  (Multicodec.to_string codec)
  Md.pp hash


