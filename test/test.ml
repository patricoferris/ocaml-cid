module Md = Multihash_digestif

let tests =
  [
    ( "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA",
      Cid.v ~version:`Cidv1 ~base:`Base58btc ~codec:`Raw
        ~hash:
          (Md.v `Sha2_256 (256 / 8)
             (Cstruct.of_hex
                "6E6FF7950A36187A801613426E858DCE686CD7D7E3C0FC42EE0330072D245C95"))
    );
    ( "bafyreihdb57fdysx5h35urvxz64ros7zvywshber7id6t6c6fek37jgyfe",
      Cid.v ~version:`Cidv1 ~base:`Base32 ~codec:`Dag_cbor
        ~hash:
          (Md.v `Sha2_256 (256 / 8)
             (Cstruct.of_hex
                "E30F7E51E257E9F7DA46B7CFB9174BF9AE2D238491FA07E9F85E2915BFA4D829"))
    );
    ( "bafkreibme22gw2h7y2h7tg2fhqotaqjucnbc24deqo72b6mkl2egezxhvy",
      Cid.v ~version:`Cidv1 ~base:`Base32 ~codec:`Raw
        ~hash:
          (Md.v `Sha2_256 (256 / 8)
             (Digestif.SHA256.digest_string "foo"
             |> Digestif.SHA256.to_hex |> Cstruct.of_hex)) );
    ( "QmXg9Pp2ytZ14xgmQjYEiHjVjMFXzCVVEcRTWJBmLgR39V",
      Cid.v ~version:`Cidv0 ~base:`Base58btc ~codec:`Dag_pb
        ~hash:
          (Md.v `Sha2_256 (256 / 8)
             (Cstruct.of_hex
                "8AB7A6C5E74737878AC73863CB76739D15D4666DE44E5756BF55A2F9E9AB5F44"))
    );
    ( "mAVUSICwmtGto/8aP+ZtFPB0wQTQTQi1wZIO/oPmKXohiZueu",
      Cid.v ~version:`Cidv1 ~base:`Base64 ~codec:`Raw
        ~hash:
          (Md.v `Sha2_256 (256 / 8)
             (Digestif.SHA256.digest_string "foo"
             |> Digestif.SHA256.to_hex |> Cstruct.of_hex)) );
  ]

let cid = Alcotest.testable Cid.pp_human Cid.equal

let pp_err ppf = function
  | `Msg m -> Fmt.string ppf m
  | `Unsupported _ -> Fmt.string ppf "Unsupported"

let err = Alcotest.of_pp pp_err

let tests =
  let test (expected_enc, expected) () =
    let actual = Cid.of_string expected_enc in
    Alcotest.(check (result cid err)) "same cid" actual (Ok expected);
    let actual_enc = Result.map Cid.to_string actual in
    Alcotest.(check (result string err))
      "same encoding" (Ok expected_enc) actual_enc
  in
  List.mapi
    (fun i v -> Alcotest.test_case ("cid " ^ string_of_int i) `Quick (test v))
    tests

let () = Alcotest.run "CID" [ ("enc-dec", tests) ]
