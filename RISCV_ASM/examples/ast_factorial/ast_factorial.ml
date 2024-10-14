open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Angstrom
open Printf

let expected_ast : ast =
  [ LabelExpr "factorial"
  ; (* factorial: *)
    InstructionExpr (Mv (X15, X10))
  ; (* mv a5, a0 *)
    InstructionExpr (Li (X10, Immediate32 1))
  ; (* li a0, 1 *)
    InstructionExpr (Beq (X15, X0, LabelAddress12 ".L5"))
  ; (* beq a5, zero, .L5 *)
    LabelExpr ".L2"
  ; (* .L2: *)
    InstructionExpr (Mv (X14, X15))
  ; (* mv a4, a5 *)
    InstructionExpr (Addiw (X15, X15, Immediate12 (-1)))
  ; (* addiw a5, a5, -1 *)
    InstructionExpr (Mulw (X10, X14, X10))
  ; (* mulw a0, a4, a0 *)
    InstructionExpr (Bne (X15, X0, LabelAddress12 ".L2"))
  ; (* bne a5, zero, .L2 *)
    LabelExpr ".L5"
  ; (* .L5: *)
    InstructionExpr Ret (* ret *)
  ]
;;

let () = print_endline (show_ast expected_ast)

let read_file file_name =
  let input_channel = open_in file_name in
  let file_length = in_channel_length input_channel in
  let file_content = really_input_string input_channel file_length in
  close_in input_channel;
  file_content
;;

let parse_input_file filename =
  let input = read_file filename in
  match parse_string ~consume:All parse_ast input with
  | Ok ast -> ast
  | Error msg -> failwith (sprintf "Failed to parse file: %s" msg)
;;

let () =
  let parsed_ast = parse_input_file "../examples/ast_factorial/factorial.s" in
  print_endline (show_ast parsed_ast)
;;