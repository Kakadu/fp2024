open Riscv_asm_interpreter_lib.Ast
open Riscv_asm_interpreter_lib.Parser
open Angstrom
open Printf

let factorial_ast : ast =
  [ DirectiveExpr(File "factorial.c")
  ; (** .file "factorial.c" *)
    DirectiveExpr(Option "pic")
  ; (** .option pic *)
    DirectiveExpr(Attribute("arch", StrValue("rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0")))
  ; (** .attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0" *)  
    DirectiveExpr(Attribute("unaligned_access", IntValue(0)))
  ; (** .attribute unaligned_access, 0 *)
    DirectiveExpr(Attribute("stack_align", IntValue(16)))
  ; (** .attribute stack_align, 16 *)
    DirectiveExpr Text
  ; (** .text *)
    DirectiveExpr(Align 1)
  ; (** .align 1 *)
    DirectiveExpr(Globl(LabelAddress12 "factorial"))
  ; (** .globl factorial *)
    DirectiveExpr(TypeDir("factorial", Type("factorial")))
  ; (** .type factorial, @function *)
    LabelExpr "factorial"
  ; (* factorial: *)
    LabelExpr ".LFB23"
  ; (** .LFB23: *)
    DirectiveExpr CfiStartproc
  ; (** .cfi_startproc *)
    InstructionExpr(Mv(X15, X10))
  ; (** mv a5,a0 *)
    InstructionExpr(Li(X10, Immediate32 1))
  ; (** li a0,1 *)
    InstructionExpr(Beq(X15, X0, LabelAddress12(".L5")))
  ; (** beq a5,zero,.L5 *)
    LabelExpr ".L2"
  ; (** .L2: *)
    InstructionExpr(Mv(X14, X15))
  ; (** mv a4,a5 *)
    InstructionExpr(Addiw(X15, X15, Immediate12(-1)))
  ; (** addiw a5,a5,-1 *)
    InstructionExpr(Mulw(X10, X14, X10))
  ; (** mulw a0,a4,a0 *)
    InstructionExpr(Bne(X15, X0, LabelAddress12(".L2")))
  ; (** bne a5,zero,.L2 *)
    LabelExpr ".L5"
  ; (** .L5: *)
    InstructionExpr Ret
  ; (** ret *)
    DirectiveExpr CfiEndproc
  ; (** .cfi_endproc *)
    LabelExpr ".LFE23"
  ; (** .LFE23: *)
    DirectiveExpr(Size(LabelAddress12 "factorial", ".-factorial"))
  ; (** .size factorial, .-factorial *)
    DirectiveExpr(Section(".rodata.str1.8", "aMS", Type("progbits"), Some 1))
  ; (** .section .rodata.str1.8,"aMS",@progbits,1 *)
    DirectiveExpr(Align 3)
  ; (** .align 3 *)
    LabelExpr ".LC0"
  ; (** .LC0: *)
    DirectiveExpr(String "%d")
  ; (** .string "%d" *)
    DirectiveExpr(Align 3)
  ; (** .align 3 *)
    LabelExpr ".LC1"
  ; (** .LC1: *)
    DirectiveExpr(String "%d\n")
  ; (** .string "%d\n" *)
    DirectiveExpr(Section(".text.startup", "ax", Type("progbits"), None))
  ; (** .section .text.startup,"ax",@progbits *)
    DirectiveExpr(Align 1)
  ; (** align 1 *)
    DirectiveExpr(Globl(LabelAddress12 "main"))
  ; (** .globl main *)
    DirectiveExpr(TypeDir("main", Type "function"))
  ; (** .type main, @function *)
    LabelExpr "main"
  ; (** main: *)
    LabelExpr ".LFB24"
  ; (** .LFB24: *)
    DirectiveExpr CfiStartproc
  ; (** .cfi_startproc *)
    InstructionExpr(Addi(X2, X2, Immediate12(-32)))
  ; (** addi sp,sp,-32 *)
    DirectiveExpr(CfiDefCfaOffset 32)
  ; (** .cfi_def_cfa_offset 32 *)
    InstructionExpr(Sd(X8, X2, ImmediateAddress12(Immediate12 16)))
  ; (** sd s0, 16(sp) *)
    DirectiveExpr(CfiOffset(8, -16))
  ; (** .cfi_offset 8, -16 *)
    InstructionExpr(La(X8, LabelAddress32 "__stack_chk_guard"))
  ; (** la s0,__stack_chk_guard *)
    InstructionExpr(Ld(X15, X8, ImmediateAddress12(Immediate12 0)))
  ; (** ld a5, 0(s0) *)
    InstructionExpr(Sd(X15, X2, ImmediateAddress12(Immediate12 8)))
  ; (** sd a5, 8(sp) *)
    InstructionExpr(Li(X15, Immediate32 0))
  ; (** li a5, 0 *)
    InstructionExpr(Addi(X11, X2, Immediate12 4))
  ; (** addi a1,sp,4 *)
    InstructionExpr(Lla(X10, LabelAddress32 ".LC0"))
  ; (** lla a0,.LC0 *)
    InstructionExpr(Sd(X1, X2, ImmediateAddress12(Immediate12 24)))
  ; (** sd ra,24(sp) *)
    DirectiveExpr(CfiOffset(1, -8))
  ; (** .cfi_offset 1, -8 *)
    InstructionExpr(Call "__isoc99_scanf@plt")
  ; (** call __isoc99_scanf@plt *)
    InstructionExpr(Li(X15, Immediate32 1))
  ; (** li a5, 1 *)
    InstructionExpr(Bne(X10, X15, LabelAddress12 ".L16"))
  ; (** bne a0,a5,.L16 *)
    InstructionExpr(Lw(X15, X2, Immediate12 4))
  ; (** lw a5,4(sp) *)
    InstructionExpr(Mv(X12, X10))
  ; (** mv a2,a0 *)
    InstructionExpr(Beq(X15, X0, LabelAddress12 ".L14"))
  ; (** beq a5,zero,.L14 *)
    LabelExpr ".L13"
  ; (** .L13: *)
    InstructionExpr(Mv(X14, X15))
  ; (** mv a4,a5 *)
    InstructionExpr(Addiw(X15, X15, Immediate12(-1)))
  ; (** addiw a5,a5,-1 *)
    InstructionExpr(Mulw(X12, X14, X12))
  ; (** mulw a2,a4,a2 *)
    InstructionExpr(Bne(X15, X0, LabelAddress12 ".L13"))
  ; (** bne a5,zero,.L13 *)
    LabelExpr ".L14"
  ; (** .L14: *)
    InstructionExpr(Lla(X11, LabelAddress32 ".LC1"))
  ; (** lla a1,.LC1 *)
    InstructionExpr(Li(X0, Immediate32 2))
  ; (** li a0, 2 *)
    InstructionExpr(Call "__printf_chk@plt")
  ; (** call __printf_chk@plt *)
    InstructionExpr(Li(X10, Immediate32 0))
  ; (** li a0,0 *)
    LabelExpr ".L12"
  ; (** .L12: *)
    InstructionExpr(Ld(X14, X2, ImmediateAddress12(Immediate12 8)))
  ; (** ld a4, 8(sp) *)
    InstructionExpr(Ld(X15, X8, ImmediateAddress12(Immediate12 0)))
  ; (** ld a5,0(s0) *)
    InstructionExpr(Xor(X15, X14, X15))
  ; (** xor a5, a4, a5 *)
    InstructionExpr(Li(X14, Immediate32 0))
  ; (** li a4, 0 *)
    InstructionExpr(Bne(X15, X0, LabelAddress12 ".L22"))
  ; (** bne a5,zero,.L22 *)
    InstructionExpr(Ld(X1, X2, ImmediateAddress12(Immediate12 24)))
  ; (** ld ra, 24(sp) *)
    DirectiveExpr CfiRememberState
  ; (** .cfi_remember_state *)
    DirectiveExpr(CfiRestore 1)
  ; (** .cfi_restore 1 *)
    InstructionExpr(Ld(X8, X2, ImmediateAddress12(Immediate12 16)))
  ; (** ld s0, 16(sp) *)
    DirectiveExpr(CfiRestore 8)
  ; (** .cfi_restore 8 *)
    InstructionExpr(Addi(X2, X2, Immediate12 32))
  ; (** addi sp,sp,32 *)
    DirectiveExpr(CfiDefCfaOffset 0)
  ; (** .cfi_def_cfa_offset 0 *)
    InstructionExpr(Jr X1)
  ; (** jr ra *)
    LabelExpr ".L16"
  ; (** .L16: *)
    DirectiveExpr(CfiRestoreState)
  ; (** .cfi_restore_state *)
    InstructionExpr(Li(X10, Immediate32(-1)))
  ; (** li a0,-1 *)
    InstructionExpr(J(LabelAddress20".L12"))
  ; (** J .L12 *)
    LabelExpr ".L22"
  ; (** .L22: *)
    InstructionExpr(Call "__stack_chk_fail@plt")
  ; (** call __stack_chk_fail@plt *)
    DirectiveExpr(CfiEndproc)
  ; (** .cfi_endproc *)
    LabelExpr ".LFE24"
  ; (** .LFE24: *)
    DirectiveExpr(Size(LabelAddress12 "main", ".-main"))
  ; (** .size main, .-main *)
    DirectiveExpr(Ident "GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0")
  ; (** .ident "GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0" *)
    DirectiveExpr(Section(".note.GNU-stack", "", Type "progbits", None))
  ; (** .section .note.GNU-stack,"",@progbits *)
  ]
;;

let () = print_endline (show_ast factorial_ast)

let read_file file_name =
  let input_channel = open_in file_name in
  let file_length = in_channel_length input_channel in
  let file_content = really_input_string input_channel file_length in
  close_in input_channel;
  file_content
;;

let parse_input_file filename =
  let input = read_file filename in
  match parse_string ~consume:Prefix parse_ast input with
  | Ok ast -> ast
  | Error msg -> failwith (sprintf "Failed to parse file%s" msg)
;;

let () =
  let parsed_ast = parse_input_file "../examples/ast_factorial/factorial.s" in
  print_endline (show_ast parsed_ast)
;;
