(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen
open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Riscv_asm_interpreter_lib.Prettyprinter

let label_name_gen = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
let label_gen = map (fun c -> String.make 1 c) label_name_gen

let register_gen =
  QCheck.Gen.(
    oneofl
      [ X0
      ; X1
      ; X2
      ; X3
      ; X4
      ; X5
      ; X6
      ; X7
      ; X8
      ; X9
      ; X10
      ; X11
      ; X12
      ; X13
      ; X14
      ; X15
      ; X16
      ; X17
      ; X18
      ; X19
      ; X20
      ; X21
      ; X22
      ; X23
      ; X24
      ; X25
      ; X26
      ; X27
      ; X28
      ; X29
      ; X30
      ; X31
      ; Zero
      ; Ra
      ; Sp
      ; Gp
      ; Tp
      ; T0
      ; T1
      ; T2
      ; S0
      ; Fp
      ; S1
      ; A0
      ; A1
      ; A2
      ; A3
      ; A4
      ; A5
      ; A6
      ; A7
      ; S2
      ; S3
      ; S4
      ; S5
      ; S6
      ; S7
      ; S8
      ; S9
      ; S10
      ; S11
      ; T3
      ; T4
      ; T5
      ; T6
      ])
;;

let address12_gen =
  QCheck.Gen.(map (fun imm -> ImmediateAddress12 imm) (int_range (-2048) 2047))
;;

let address20_gen =
  QCheck.Gen.(map (fun imm -> ImmediateAddress20 imm) (int_range (-524288) 524287))
;;

let address32_gen =
  QCheck.Gen.(
    map (fun imm -> ImmediateAddress32 imm) (int_range (-2147483648) 2147483647))
;;

let string_or_int_value_gen =
  QCheck.Gen.(
    oneof [ map (fun str -> StrValue str) label_gen; map (fun imm -> IntValue imm) int ])
;;

let type_dir_gen = map (fun str -> Type str) label_gen

let section_gen =
  QCheck.Gen.(
    map3
      (fun str1 str2 typ opt -> Section (str1, str2, typ, opt))
      label_gen
      label_gen
      type_dir_gen
    >>= fun f -> option int >>= fun opt -> return (f opt))
;;

let directive_gen =
  QCheck.Gen.(
    oneof
      [ map (fun str -> File str) label_gen
      ; map (fun str -> Option str) label_gen
      ; map2 (fun str value -> Attribute (str, value)) label_gen string_or_int_value_gen
      ; return Text
      ; map (fun imm -> Align imm) (int_range 0 32)
      ; map (fun imm -> Globl imm) address12_gen
      ; map2 (fun str typ -> TypeDir (str, typ)) label_gen type_dir_gen
      ; return CfiStartproc
      ; return CfiEndproc
      ; map2 (fun imm str -> Size (imm, str)) address12_gen label_gen
      ; section_gen
      ; map (fun str -> String str) label_gen
      ; map (fun imm -> CfiDefCfaOffset imm) int
      ; map2 (fun imm1 imm2 -> CfiOffset (imm1, imm2)) int int
      ; return CfiRememberState
      ; map (fun imm -> CfiRestore imm) int
      ; map (fun str -> Ident str) label_gen
      ; return CfiRestoreState
      ])
;;

let instruction_gen =
  QCheck.Gen.(
    oneof
      [ map3 (fun rd rs1 rs2 -> Add (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Sub (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Xor (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Or (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> And (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Sll (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Srl (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Sra (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3 (fun rd rs1 rs2 -> Slt (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3
          (fun rd rs1 rs2 -> Sltu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 imm -> Addi (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Xori (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Ori (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Andi (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Slli (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Srli (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Srai (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Slti (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Sltiu (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Beq (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Bne (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Blt (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Bge (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Bltu (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Bgeu (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map2 (fun rd imm -> Jal (rd, imm)) register_gen address20_gen
      ; map3
          (fun rd rs1 imm -> Jalr (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map (fun rs1 -> Jr rs1) register_gen
      ; map (fun imm -> J imm) address20_gen
      ; map2 (fun rd imm -> Lui (rd, imm)) register_gen address20_gen
      ; map2 (fun rd imm -> Auipc (rd, imm)) register_gen address20_gen
      ; return Ecall
      ; map (fun str -> Call str) label_gen
      ; map3 (fun rd rs1 rs2 -> Mul (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3
          (fun rd rs1 rs2 -> Mulh (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Mulhsu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Mulhu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3 (fun rd rs1 rs2 -> Div (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3
          (fun rd rs1 rs2 -> Divu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3 (fun rd rs1 rs2 -> Rem (rd, rs1, rs2)) register_gen register_gen register_gen
      ; map3
          (fun rd rs1 rs2 -> Remu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 imm -> Addiw (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Slliw (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Srliw (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 imm -> Sraiw (rd, rs1, imm))
          register_gen
          register_gen
          address12_gen
      ; map3
          (fun rd rs1 rs2 -> Addw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Subw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Sllw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Srlw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Sraw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Mulw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Divw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Divuw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Remw (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map3
          (fun rd rs1 rs2 -> Remwu (rd, rs1, rs2))
          register_gen
          register_gen
          register_gen
      ; map2 (fun rd rs1 -> Mv (rd, rs1)) register_gen register_gen
      ; map2 (fun rd imm -> Li (rd, imm)) register_gen address32_gen
      ; return Ret
      ])
;;

let expr_gen =
  QCheck.Gen.(
    oneof
      [ map (fun instr -> InstructionExpr instr) instruction_gen
      ; map (fun lbl -> LabelExpr lbl) label_gen
      ; map (fun dir -> DirectiveExpr dir) directive_gen
      ])
;;

let ast_gen_helper self = function
  | 0 -> return []
  | n -> frequency [ 1, map2 List.cons expr_gen (self (n / 2)); 1, return [] ]
;;

let ast_gen = QCheck.Gen.(sized @@ fix (fun self n -> ast_gen_helper self n))

let arbitrary_ast =
  QCheck.make
    ast_gen
    ~print:
      (Format.asprintf
         "%a"
         (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") pp_expr))
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
    "Testing manual generator.";
  let _ : int = run () in
  ()
;;
