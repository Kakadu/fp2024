open QCheck
open Base
open Astpprinter
open Ocamladt_lib.Parser
open Ocamladt_lib.Ast

let gen_char = 
  let open Gen in 
  map Char.of_int_exn (int_range (Char.to_int 'a') (Char.to_int 'z'))


let gen_constant =
  let open Gen in
  oneof [
    map (fun i -> Const_integer (i mod 100)) int;
    map (fun c -> Const_char c) gen_char;
  ]

let gen_ident = Gen.string_size ~gen:gen_char (Gen.int_range 1 5)

let rec gen_pattern size =
  let open Gen in
  if size <= 0 then oneof [
    return Pat_any;
    map (fun id -> Pat_var id) gen_ident;
    map (fun c -> Pat_constant c) gen_constant;
  ]
  else
    oneof [
      return Pat_any;
      map (fun id -> Pat_var id) gen_ident;
      map (fun c -> Pat_constant c) gen_constant;
      map2 (fun p1 p2 -> Pat_tuple (p1, p2, [])) (gen_pattern (size/2)) (gen_pattern (size/2));
      map2 (fun id p_opt -> Pat_construct (id, p_opt)) gen_ident (option (gen_pattern (size/2)));
    ]

let gen_rec_flag = Gen.oneofl [ Nonrecursive; Recursive ]

let rec gen_expression size =
  let open Gen in
  if size <= 0 then
    oneof [
      map (fun id -> Exp_ident id) gen_ident;
      map (fun c -> Exp_constant c) gen_constant;
    ]
  else
    oneof [
      map (fun id -> Exp_ident id) gen_ident;
      map (fun c -> Exp_constant c) gen_constant;
      (* map2 (fun e1 e2 -> Exp_tuple (e1, e2, [])) (gen_expression (size/2)) (gen_expression (size/2)); *)
      (* map (fun (c, cs) -> Exp_function (c, cs)) (gen_cases size); *)
      map3 (fun p ps e -> Exp_fun (p, List.take ps 3, e)) (gen_pattern (size/2)) (list_size (int_bound 3) (gen_pattern (size/2))) (gen_expression (size/2));
      map2 (fun e es -> Exp_apply (e, es)) (gen_expression (size/2)) (small_list (gen_expression (size/2)));
      (* map3 (fun e c cs -> Exp_match (e, c, cs)) (gen_expression (size/2)) (gen_case size) (small_list (gen_case (size/2))); *)
      (* map2 (fun e1 e2 -> Exp_if (e1, e2, None)) (gen_expression (size/2)) (gen_expression (size/2)); *)
      (* map3 (fun e1 e2 e3 -> Exp_if (e1, e2, Some e3)) (gen_expression (size/2)) (gen_expression (size/2)) (gen_expression (size/2)); *)
      map3 (fun rec_flag bindings e -> Exp_let (rec_flag, bindings, e)) gen_rec_flag (list_size (int_bound 3) (gen_value_binding (size/10))) (gen_expression (size/2));
      (* map2 (fun id e_opt -> Exp_construct (id, e_opt)) gen_ident (option (gen_expression (size/2))); *)
    ]

and gen_value_binding size =
  let open Gen in
  map2 (fun pat expr -> { pat; expr }) (gen_pattern (size/2)) (gen_expression (size/2))

and gen_case size =
  let open Gen in
  map2 (fun left right -> { left; right }) (gen_pattern (size/2)) (gen_expression (size/2))

and gen_cases size = 
  let open Gen in
  let case = gen_case (size / 2) in
  map2 (fun c cs -> (c, cs)) case (small_list case)

let gen_structure_item size =
  let open Gen in
  oneof [
    map (fun e -> Str_eval e) (gen_expression size);
    map2 (fun rec_flag bindings -> Str_value (rec_flag, bindings)) gen_rec_flag (small_list (gen_value_binding size));
  ]

let gen_program =
  let open Gen in
  list_size (int_bound 2) (gen_structure_item 1)

let arbitrary_program =
  QCheck.make
  ~print:(fun program -> Format.asprintf "%a" pprint_program program)
    (gen_program)

let success_with_message message =
  print_endline ("Success: " ^ message)

(* Running tests *)
let () =
  let open QCheck_runner in
  let test_round_trip = 
    Test.make 
      ~name:"round trip parsing and pretty printing" 
      arbitrary_program
      (fun program ->
        let program_str = Format.asprintf "%a" pprint_program program in
        match parse program_str with
         | Ok ast -> 
           let is_equal = equal_program ast program in
           if not is_equal then
            let error_message =
              Printf.sprintf "Error: Programs are not structurally identical.\nAST: %s\n Middle representation: %s\n Program: %s"
                (show_program ast) (program_str) (show_program program)
            in
            failwith error_message
            else
              true
          | Error err -> 
              (* If parsing failed, show the error *)
              let error_message = 
                Printf.sprintf "Parsing failed on program:\n %s\nError: %s\n"
                  program_str (match err with
                    | msg -> msg
                  )
              in
              failwith error_message
      )
  in 
  ignore (run_tests ~verbose:true [test_round_trip])
