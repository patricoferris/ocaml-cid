let without_tests =
  try
    ignore Sys.argv.(1);
    true
  with _ -> false

let () = if without_tests then Logs.set_reporter Logs.nop_reporter

module Schema = struct
  include Irmin.Schema.KV (Irmin.Contents.String)
  module Md = Multihash_digestif

  module Hash = struct
    type t = Cid.t

    let hash = `Sha2_256
    let cid = Cid.v ~version:`Cidv1 ~base:`Base32 ~codec:`Raw

    let get_64_little_endian str idx =
      if Sys.big_endian then Cstruct.BE.get_uint64 str idx
      else Cstruct.LE.get_uint64 str idx

    let short_hash c =
      Int64.to_int (get_64_little_endian (Md.write (Cid.hash c)) 0)

    let short_hash_substring bigstring ~off =
      Int64.to_int (Bigstringaf.get_int64_le bigstring off)

    let hash_size =
      Md.of_cstruct hash Cstruct.empty
      |> Result.get_ok |> Md.write |> Cstruct.length

    let of_string s =
      match Cid.of_string s with
      | Ok _ as v -> v
      | Error (`Msg _) as v -> v
      | Error (`Unsupported b) ->
          Error
            (`Msg ("Unsupported encoding " ^ Multibase.Encoding.to_string b))

    let pp ppf cid = Fmt.string ppf (Cid.to_string cid)

    let read v =
      let hash = Result.get_ok (Md.read_buff (Cstruct.of_string v)) in
      cid ~hash

    let write v = Md.write (Cid.hash v) |> Cstruct.to_string

    let t =
      let open Irmin in
      Type.map ~pp ~of_string Type.(string_of (`Fixed hash_size)) read write

    let convert (f : (string -> unit) -> unit) : (Cstruct.t -> unit) -> unit =
     fun s -> f (fun buf -> s (Cstruct.of_string buf))

    let hash (f : (string -> unit) -> unit) : t =
      let hash = Md.iter_cstruct hash (convert f) |> Result.get_ok in
      cid ~hash

    let to_raw_string = write
    let unsafe_of_raw_string = read
  end
end

module Store = Irmin_mem.Make (Schema)

let main () =
  let open Lwt.Syntax in
  let config = Irmin_mem.config () in
  let* repo = Store.Repo.v config in
  let* main = Store.main repo in
  let content = "foo" in
  let* () = Store.set_exn ~info:Store.Info.none main [ "a" ] content in
  Fmt.pr "Base encoded: %a\nHuman: %a\n"
    Irmin.Type.(pp Store.hash_t)
    (Store.Contents.hash content)
    Cid.pp_human
    (Store.Contents.hash content);
  Lwt.return_unit

let store = (module Store : Irmin_test.S)
let suite config = Irmin_test.Suite.create ~name:"MEM.CID" ~store ~config ()

let () =
  if not without_tests then
    Lwt_main.run
      (Irmin_test.Store.run "irmin-mem-cid" ~slow:false ~misc:[]
         ~sleep:Lwt_unix.sleep
         [ (`Quick, suite (Irmin_mem.config ())) ]);
  Lwt_main.run (main ())
