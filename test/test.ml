module Md = Multihash_digestif

let tests = [
  "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA",
  Cid.v
    ~version:`Cidv1
    ~base:`Base58btc
    ~codec:`Raw
    ~hash:(Md.make ~ident:`Sha2_256 ~length:(256 / 8) ~digest:(Cstruct.of_hex "6E6FF7950A36187A801613426E858DCE686CD7D7E3C0FC42EE0330072D245C95"))
]

let cid = Alcotest.testable Cid.pp_human Cid.equal
let msg = Alcotest.of_pp (fun ppf (`Msg m) -> Fmt.string ppf m)

let tests =
  let test (expected_enc, expected) () = 
    let actual = Cid.of_string expected_enc in
    Alcotest.(check cid) "same cid" actual expected;
    let actual_enc = Cid.to_string actual in
    Alcotest.(check (result string msg)) "same encoding" actual_enc (Ok expected_enc)
  in
    List.mapi (fun i v -> Alcotest.test_case ("cid " ^ string_of_int i) `Quick (test v)) tests

let () = 
  Alcotest.run "CID" [
    "enc-dec", tests
  ]