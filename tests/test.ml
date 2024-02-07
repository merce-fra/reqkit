open! Base

let%test_unit "rev" =
  [%test_eq: int list] (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]

