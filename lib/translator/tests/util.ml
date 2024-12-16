


let%expect_test "requirements_without_decl" =
  let s = "Input x02 IS bool
          ID000: Globally, it is always the case that if \"x00000000\" holds, then \"x00000001\" holds for at least 25 time units"  in 
  let ast = Reqs.Parse.ast_from_string s in
  let parse_t = Reqs.Parse.ast_to_parse_t ast in
  try
    ignore(Src.Util.typical_reqs [parse_t] false);
  with Src.Util.Unknown_variable _ -> Format.printf "ko@.";
  [%expect {|ko|}]
    
let%expect_test "requirements_identical" =
  let s="Input x01 IS bool
  Input x02 IS bool
  Input x03 IS bool
  ID000: Globally, it is always the case that if \"x01\" holds, then \"x02\" holds for at least 25 time units
  ID001: Globally, it is always the case that if \"x01 || x02\" holds, then \"x03\" holds for at least 25 time units" in
  let ast = Reqs.Parse.ast_from_string s in
  let parse_t = Reqs.Parse.ast_to_parse_t ast in
  let filtered_req = Src.Util.typical_reqs [parse_t] false in
  let fmt = Format.get_std_formatter() in 
  List.iter( fun p -> Reqs.Parse.print fmt p) filtered_req;
  [%expect {|
    Input		x03 IS bool
    Input		x01 IS bool
    Input		x02 IS bool

    ID001 : Globally, it is always the case that if "(x01 || x02)"  holds  , then "x03"  holds for at least "25" time units |}]

