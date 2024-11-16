(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Riscv_asm_interpreter_lib.Prettyprinter
open QCheck.Shrink
open QCheck.Iter

let rec pow base = function
  | 0 -> 1
  | 1 -> base
  | n ->
    let a = pow base (n / 2) in
    a * a * if n mod 2 = 0 then 1 else base
;;

let shrink_string str =
  string str
  |> filter (fun s -> String.length s < String.length str && not (String.contains s '\\'))
  |> map (fun s -> s)
;;

let shrink_int_to_k_bits x k = int x |> filter (fun x -> abs x < pow 2 k)

let shrink_address12 addr =
  match addr with
  | ImmediateAddress12 imm ->
    shrink_int_to_k_bits 12 imm |> map (fun new_imm -> ImmediateAddress12 new_imm)
  | LabelAddress12 label ->
    shrink_string label |> map (fun new_label -> LabelAddress12 new_label)
;;

let shrink_address20 addr =
  match addr with
  | ImmediateAddress20 imm ->
    shrink_int_to_k_bits 20 imm |> map (fun new_imm -> ImmediateAddress20 new_imm)
  | LabelAddress20 label ->
    shrink_string label |> map (fun new_label -> LabelAddress20 new_label)
;;

let shrink_address32 addr =
  match addr with
  | ImmediateAddress32 imm ->
    shrink_int_to_k_bits 32 imm |> map (fun new_imm -> ImmediateAddress32 new_imm)
  | LabelAddress32 label ->
    shrink_string label |> map (fun new_label -> LabelAddress32 new_label)
;;

let shrink_instruction instr =
  match instr with
  | Add _
  | Sub _
  | Xor _
  | Or _
  | And _
  | Sll _
  | Srl _
  | Sra _
  | Slt _
  | Sltu _
  | Mul _
  | Mulh _
  | Mulhsu _
  | Mulhu _
  | Div _
  | Divu _
  | Rem _
  | Remu _
  | Lwu _
  | Ld _
  | Sd _
  | Addiw _
  | Slliw _
  | Srliw _
  | Sraiw _
  | Addw _
  | Subw _
  | Sllw _
  | Srlw _
  | Sraw _
  | Mulw _
  | Divw _
  | Divuw _
  | Remw _
  | Remwu _
  | Mv _
  | Li _
  | Ret
  | Jr _
  | Call _
  | Ecall -> empty
  | Addi (r1, r2, addr)
  | Xori (r1, r2, addr)
  | Ori (r1, r2, addr)
  | Andi (r1, r2, addr)
  | Slli (r1, r2, addr)
  | Srli (r1, r2, addr)
  | Srai (r1, r2, addr)
  | Slti (r1, r2, addr)
  | Sltiu (r1, r2, addr)
  | Lb (r1, r2, addr)
  | Lh (r1, r2, addr)
  | Lw (r1, r2, addr)
  | Lbu (r1, r2, addr)
  | Lhu (r1, r2, addr)
  | Sb (r1, r2, addr)
  | Sh (r1, r2, addr)
  | Sw (r1, r2, addr)
  | Beq (r1, r2, addr)
  | Bne (r1, r2, addr)
  | Blt (r1, r2, addr)
  | Bge (r1, r2, addr)
  | Bltu (r1, r2, addr)
  | Bgeu (r1, r2, addr)
  | Jalr (r1, r2, addr) ->
    shrink_address12 addr
    |> map (fun new_addr ->
      match instr with
      | Addi _ -> Addi (r1, r2, new_addr)
      | Xori _ -> Xori (r1, r2, new_addr)
      | Ori _ -> Ori (r1, r2, new_addr)
      | Andi _ -> Andi (r1, r2, new_addr)
      | Slli _ -> Slli (r1, r2, new_addr)
      | Srli _ -> Srli (r1, r2, new_addr)
      | Srai _ -> Srai (r1, r2, new_addr)
      | Slti _ -> Slti (r1, r2, new_addr)
      | Sltiu _ -> Sltiu (r1, r2, new_addr)
      | Lb _ -> Lb (r1, r2, new_addr)
      | Lh _ -> Lh (r1, r2, new_addr)
      | Lw _ -> Lw (r1, r2, new_addr)
      | Lbu _ -> Lbu (r1, r2, new_addr)
      | Lhu _ -> Lhu (r1, r2, new_addr)
      | Sb _ -> Sb (r1, r2, new_addr)
      | Sh _ -> Sh (r1, r2, new_addr)
      | Sw _ -> Sw (r1, r2, new_addr)
      | Beq _ -> Beq (r1, r2, new_addr)
      | Bne _ -> Bne (r1, r2, new_addr)
      | Blt _ -> Blt (r1, r2, new_addr)
      | Bge _ -> Bge (r1, r2, new_addr)
      | Bltu _ -> Bltu (r1, r2, new_addr)
      | Bgeu _ -> Bgeu (r1, r2, new_addr)
      | Jalr _ -> Jalr (r1, r2, new_addr)
      | _ -> instr)
  | J addr -> shrink_address20 addr |> map (fun new_addr -> J new_addr)
  | Lui (reg, addr) | Auipc (reg, addr) | Jal (reg, addr) ->
    shrink_address20 addr
    |> map (fun new_addr ->
      match instr with
      | Lui _ -> Lui (reg, new_addr)
      | Auipc _ -> Auipc (reg, new_addr)
      | Jal _ -> Jal (reg, new_addr)
      | _ -> instr)
  | La (reg, addr) | Lla (reg, addr) ->
    shrink_address32 addr
    |> map (fun new_addr ->
      match instr with
      | La _ -> La (reg, new_addr)
      | Lla _ -> Lla (reg, new_addr)
      | _ -> instr)
;;

let shrink_directive dir =
  match dir with
  | CfiStartproc | CfiEndproc | Text | CfiRememberState | CfiRestoreState -> empty
  | File str | Option str | Ident str | String str ->
    shrink_string str
    |> map (fun new_str ->
      match dir with
      | File _ -> File new_str
      | Option _ -> Option new_str
      | Ident _ -> Ident new_str
      | String _ -> String new_str
      | _ -> dir)
  | Attribute (tag, value) ->
    let shrink_value = function
      | StrValue str -> shrink_string str |> map (fun new_str -> StrValue new_str)
      | IntValue x -> shrink_int_to_k_bits x 32 |> map (fun new_x -> IntValue new_x)
    in
    shrink_value value |> map (fun new_value -> Attribute (tag, new_value))
  | Align x -> shrink_int_to_k_bits x 32 |> map (fun new_x -> Align new_x)
  | Globl addr -> shrink_address12 addr |> map (fun new_addr -> Globl new_addr)
  | TypeDir (s, Type t) -> shrink_string s |> map (fun new_s -> TypeDir (new_s, Type t))
  | Size (addr, s) -> shrink_address12 addr |> map (fun new_addr -> Size (new_addr, s))
  | Section (name, str, Type t, x) ->
    let shrink_x =
      match x with
      | Some x -> shrink_int_to_k_bits x 32 |> map (fun new_x -> Some new_x)
      | None -> empty
    in
    shrink_string name
    >>= fun new_name ->
    shrink_string str
    >>= fun new_str ->
    shrink_x |> map (fun new_x -> Section (new_name, new_str, Type t, new_x))
  | CfiDefCfaOffset x ->
    shrink_int_to_k_bits x 32 |> map (fun new_x -> CfiDefCfaOffset new_x)
  | CfiOffset (x, y) ->
    shrink_int_to_k_bits x 32
    >>= fun new_x ->
    shrink_int_to_k_bits y 32 |> map (fun new_y -> CfiOffset (new_x, new_y))
  | CfiRestore x -> shrink_int_to_k_bits x 32 |> map (fun new_x -> CfiRestore new_x)
;;

let shrink_expr expr =
  match expr with
  | LabelExpr label -> shrink_string label |> map (fun new_label -> LabelExpr new_label)
  | InstructionExpr instr ->
    shrink_instruction instr |> map (fun new_instr -> InstructionExpr new_instr)
  | DirectiveExpr dir ->
    shrink_directive dir |> map (fun new_dir -> DirectiveExpr new_dir)
;;

let shrink_exprs ast =
  List.fold_right
    (fun expr acc ->
      shrink_expr expr
      >>= fun shrunk_expr ->
      acc
      |> map (fun ast' -> List.map (fun e -> if e == expr then shrunk_expr else e) ast'))
    ast
    (return ast)
;;

let shrink_ast ast =
  let shrink_exprs_result = shrink_exprs ast in
  let list_result = list ast in
  append shrink_exprs_result list_result
;;

let arbitrary_ast =
  let open QCheck.Gen in
  let ast_gen = list gen_expr in
  QCheck.make
    ast_gen
    ~print:
      (Format.asprintf
         "%a"
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") pp_expr))
    ~shrink:shrink_ast
;;

let run () =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_ast (fun ast ->
          Result.ok ast
          = Angstrom.parse_string ~consume:All parse_ast (Format.asprintf "%a" pp_ast ast)))
    ]
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed" ]
    print_endline
    "Testing automatic generator.";
  let _ : int = run () in
  ()
;;
