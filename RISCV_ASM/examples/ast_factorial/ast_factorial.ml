open Riscv_asm_interpreter_lib.Ast

let factorial_ast : ast = [
  Label "factorial";                                  (* factorial: *)
  Instruction (Mv (X25, X0));                   (* mv a5, a0 *)
  Instruction (Li (X0, Immediate32 1));                      (* li a0, 1 *)
  Instruction (Beq (X25, X0, address ".L5"));         (* beq a5, zero, .L5 *)

  Label ".L2";                                        (* .L2: *)
  Instruction (Mv (X24, X25));                   (* mv a4, a5 *)
  Instruction (Addiw (X25, X25, Immediate12 -1));            (* addiw a5, a5, -1 *)
  Instruction (Mulw (X10, X24, X10));           (* mulw a0, a4, a0 *)
  Instruction (Bne (X25, X0, address ".L2"));         (* bne a5, zero, .L2 *)

  Label ".L5";                                        (* .L5: *)
  Instruction Ret                                  (* ret *)
]