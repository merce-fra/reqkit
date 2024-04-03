let parse_from_string s = 
  let ast = Src.Parse.ast_from_string s in
  match ast with 
  | Ok _ -> true
  | Error _ -> false

let check r e = 
  if e = r then Format.printf "ok-" else Format.printf "ko-"

let%expect_test "parse_declaration_1" =
  let valid_declarations = 
    ["CONST x00895 IS 3";
     "CONST x00895 IS 3.0";
     "CONST x00895 IS TRUE";
     "CONST x00895 IS true";
     "CONST x00895 is true";
     "Input x00001 IS int";
     "Input x00001 IS bool";
     "Input x00001 IS real";
     "Output x00001 IS int";
     "Output x00001 IS bool";
     "Output x00001 IS real";
     "Internal x00001 IS int";
     "Internal x00001 IS bool";
     "Internal x00001 IS real";
    ] in 
    let result = List.rev (List.fold_left (fun acc s ->  (parse_from_string s)::acc) [] valid_declarations) in 
    let (expected_result : bool list )= List.init (List.length valid_declarations) (fun _ ->  true) in
    List.iter2 check result expected_result;
    [%expect {|
      ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-|}]

let%expect_test "parse_declaration_2" =
  let non_valid_declarations = 
    ["CONST x00895 IS bool";
      "CONST x00895 IS int";
      "CONST x00895 IS real";
      "CONST not_valid IS 3"
    ] in 
    let result = List.rev (List.fold_left (fun acc s ->  (parse_from_string s)::acc) [] non_valid_declarations) in 
    let (expected_result : bool list )= List.init (List.length non_valid_declarations) (fun _ ->  false) in
    List.iter2 check result expected_result;
    [%expect {|
      ok-ok-ok-ok-|}]


let%expect_test "parse_req_1" =
  let valid_requirements = 
    [ "ID000: Globally, it is always the case that if \"x0000\" holds, then \"x0001\" holds for at least 25 time units";
      "ID001: Globally, it is always the case that if \"x0000\" holds, then \"!x0001\" holds after at most 30 time units";
      "ID002: Globally, it is always the case that if \"x0002 && x0003 && x0004\" holds, then \"!x0000\" holds for at least \"30\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15+3\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15+x00001\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15/x00001\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15-x00001\" time units";
      "ID007: After \"x0003\" until \"x0005\", it is always the case that if \"!x0004\" holds then \"x0006\" holds for at least \"15*x00001\" time units";
      "ID009: After \"x0003\" until \"x0005\", it is always the case that if \"!x0006\" holds, then \"x0005\" holds after at most \"10\" time units";
      "ID009: After  \"x0006\"  until  \"!x0002 && !x0003\" , it is always the case that  \"x0006\"  holds";
      "ID011: Globally, it is always the case that if  \"x0015\"  holds, then  \"!x0016\"  holds as well";
      "ID016: After \"x0003\" until \"!x0002\", it is always the case that \"x0003\" holds";
      "ID001: Between \"x0000\" and \"!x0001\", it is always the case that once \"x0001\" becomes satisfied it holds for less than \"30\" time units";
      "ID004: Between \"x0005 && !x0006 &&!x0007\" and \"x0006\", it is always the case that once \"!x0006\" becomes satisfied it holds for at least \"100\" time units";
      "ID015: Globally, it is always the case that if \"x0014 && x0015\" holds, then \"x0011\" previously held";
      "ID039: Globally, it is never the case that \"x0023 && x0024\" holds";
      "ID056: Globally, it is always the case that if \"x0037\" holds then \"x0031\" holds as well.";
      "ID057: Globally, it is never the case that \"x0034 && x0037\" holds.";
      "ID057: Globally, it is never the case that \"x0034 && x0037\" holds.";
      "ID004: Globally, it is always the case that if \"x0004\" holds and is succeeded by \"x0005\" then \"x0006\" previously held";
      "ID004: Globally, it is always the case that once \"x0002\" becomes satisfied it holds for less than 1 time units";
      "ID014: After \"x0025\" until \"!x0025\", it is always the case that \"x0026\" holds at least every 4 time units";
      "ID16: Before \"x0025\", it is always the case that if \"x0018\" holds then \"x0007\" holds after at most 10 time units";
      "ID006: Globally, it is always the case that if \"x0001 && !x0006\" holds, then \"x0007\" holds after at most \"1\" time unit.";
      "ID_00010: Globally, it is always the case that if \"(x00043 == x00022)\" holds, then \"(x00049 == x00029)\n&& (x00047 == x00037)\n&& (x00038 == x00025)\n\" holds after at most \"x00054\" time units";
      "ID_00291: Globally, it is always the case that if \"!x00932 \" holds for at least \"500.0\" time units, then \"x00671 \" holds afterwards";
      "//SysRS_ONw_496_0: Globally, it is always the case that if \"(x00686==x00119)||(x00686==x00919&&x00530==x00620)||(x00686==x00919&&x00530==x00235)||(x00686==x00919&&x00530==x00198)||(x00686==x00919&&x00530==x00147)||(x00686==x00919&&x00530==x00342)||(x00686==x00919&&x00530==x00438)||(x00686==x00919&&x00530==x00778)||(x00686==x00918&&x00529==x00620)||(x00686==x00918&&x00529==x00235)||(x00686==x00918&&x00529==x00198)||(x00686==x00918&&x00529==x00147)||(x00686==x00918&&x00529==x00342)||(x00686==x00918&&x00529==x00438)||(x00686==x00918&&x00529==x00778)||(x00686==x00917&&x00528==x00620)||(x00686==x00917&&x00528==x00235)||(x00686==x00917&&x00528==x00198)||(x00686==x00917&&x00528==x00147)||(x00686==x00917&&x00528==x00342)||(x00686==x00917&&x00528==x00438)||(x00686==x00917&&x00528==x00778)||(x00686==x00916&&x00527==x00620)||(x00686==x00916&&x00527==x00235)||(x00686==x00916&&x00527==x00198)||(x00686==x00916&&x00527==x00147)||(x00686==x00916&&x00527==x00342)||(x00686==x00916&&x00527==x00438)||(x00686==x00916&&x00527==x00778)||(x00686==x01041)||(x00686==x01040)\" holds, then \"x00363 == x00119 \" holds after at most \"x01033\" time units"    
        ] in 
    let result = List.rev (List.fold_left (fun acc s ->  (parse_from_string s)::acc) [] valid_requirements) in 
    let (expected_result : bool list )= List.init (List.length valid_requirements) (fun _ ->  true) in
    List.iter2 check result expected_result;
    [%expect {| ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok-ok- |}]


let%expect_test "print_req_1" =
  let s = "Input x0000 is bool
          Input x0001 is real 
          ID000: Globally, it is always the case that if \"x0000\" holds, then \"x0001\" holds for at least 25 time units" in
  let ast = Src.Parse.ast_from_string s in
  let parse_t = Src.Parse.ast_to_parse_t ast in
  let fmt = Format.get_std_formatter () in
  Src.Parse.print fmt parse_t;
  [%expect {|
    Input		x0000 IS bool
    Input		x0001 IS real

    ID000 : Globally, it is always the case that if "x0000"  holds  , then "x0001"  holds for at least "25" time units |}]