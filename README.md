# ocaml-cid

*Untested and not stable*

[Content-addressed identifiers](https://docs.ipfs.io/concepts/content-addressing/) in OCaml.

```ocaml
# let s = "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA";;
val s : string = "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA"
# let cid = Cid.of_string s;;
val cid : Cid.t = <abstr>
# Cid.pp_human Format.std_formatter cid;;
cidv1 - base58btc - raw - ident(sha2-256) length(32) digest(6e 6f f7 95 0a 36 18 7a  80 16 13 42 6e 85 8d ce
                                                            68 6c d7 d7 e3 c0 fc 42  ee 03 30 07 2d 24 5c 95
                                                            )
- : unit = ()
# Result.get_ok @@ Cid.to_string cid;;
- : string = "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA"
```
