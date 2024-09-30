open Riscv_asm_interpreter_lib.Ast

let () =
  let factorial_ast : ast =
    [ Label "factorial"
    ; (* factorial: *)
      Instruction (Mv (X15, X10))
    ; (* mv a5, a0 *)
      Instruction (Li (X10, 1))
    ; (* li a0, 1 *)
      Instruction (Beq (X15, X0, Label ".L5"))
    ; (* beq a5, zero, .L5 *)
      Label ".L2"
    ; (* .L2: *)
      Instruction (Mv (X14, X15))
    ; (* mv a4, a5 *)
      Instruction (Addiw (X15, X15, -1))
    ; (* addiw a5, a5, -1 *)
      Instruction (Mulw (X10, X14, X10))
    ; (* mulw a0, a4, a0 *)
      Instruction (Bne (X15, X0, Label ".L2"))
    ; (* bne a5, zero, .L2 *)
      Label ".L5"
    ; (* .L5: *)
      Instruction Ret (* ret *)
    ]
  in
  print_endline (show_ast factorial_ast)
;;
