module StringMap = Map.Make(String)

open Ast

type state = {
  registers : int StringMap.t;
  pc : int;
}

let handle_alternative_names = function
  | Zero -> X0
  | Ra -> X1
  | Sp -> X2
  | Gp -> X3
  | Tp -> X4
  | T0 -> X5
  | T1 -> X6
  | T2 -> X7
  | S0 | Fp -> X8
  | S1 -> X9
  | A0 -> X10
  | A1 -> X11
  | A2 -> X12
  | A3 -> X13
  | A4 -> X14
  | A5 -> X15
  | A6 -> X16
  | A7 -> X17
  | S2 -> X18
  | S3 -> X19
  | S4 -> X20
  | S5 -> X21
  | S6 -> X22
  | S7 -> X23
  | S8 -> X24
  | S9 -> X25
  | S10 -> X26
  | S11 -> X27
  | T3 -> X28
  | T4 -> X29
  | T5 -> X30
  | T6 -> X31
  | x -> x

let init_state () =
  let registers = List.fold_left
    (fun acc reg -> StringMap.add (show_register reg) 0 acc)
    StringMap.empty
    [
      X0; X1; X2; X3; X4; X5; X6; X7; X8; X9;
      X10; X11; X12; X13; X14; X15; X16; X17; X18; X19;
      X20; X21; X22; X23; X24; X25; X26; X27; X28; X29; X30; X31
    ]
  in
  { registers; pc = 0 }

let get_register_value state reg =
  StringMap.find_opt (show_register handle_alternative_names reg) state.registers |> Option.value ~default:0

let set_register_value state reg value =
  { state with registers = StringMap.add (show_register handle_alternative_names reg) value state.registers }

let set_pc state new_pc = { state with pc = new_pc }

let increment_pc state = { state with pc = state.pc + 1 }

let resolve_label program label =
  let rec aux idx = function
    | [] -> -1
    | LabelExpr lbl :: _ when lbl = label -> idx
    | _ :: tl -> aux (idx + 1) tl
  in
  aux 0 program

let rec interpret state program =
  match List.nth_opt program state.pc with
  | None -> state
  | Some (InstructionExpr instr) ->
      let new_state = execute_instruction state instr program in
      interpret (increment_pc new_state) program
  | Some (LabelExpr _) ->
      interpret (increment_pc state) program
  | Some (DirectiveExpr _) ->
      interpret (increment_pc state) program

and execute_instruction state instr program =
  match instr with
  | Add (rd, rs1, rs2) ->
      let val1 = get_register_value state rs1 in
      let val2 = get_register_value state rs2 in
      let result = val1 + val2 in
      set_register_value state rd result
  | Sub (rd, rs1, rs2) ->
      let val1 = get_register_value state rs1 in
      let val2 = get_register_value state rs2 in
      let result = val1 - val2 in
      set_register_value state rd result
  | Jalr (rd, rs1, imm) ->
      let val_rs1 = get_register_value state rs1 in
      let new_pc = val_rs1 + imm in
      let new_state = set_register_value state rd (state.pc + 1) in
      set_pc new_state new_pc
  | Jr rs1 ->
      let val_rs1 = get_register_value state rs1 in
      let new_state = set_register_value state X0 (state.pc + 1) in
      set_pc new_state val_rs1
  | J imm ->
      let label_idx = resolve_label program (string_of_int imm) in
      if label_idx = -1 then
        state
      else
        let new_state = set_register_value state X0 (state.pc + 1) in
        set_pc new_state label_idx
  | _ -> state

let _ =
  let initial_state = init_state () in
  let program = [
    InstructionExpr (Add (X1, X2, X3));
    LabelExpr "start";
    InstructionExpr (Sub (X4, X5, X6));
    InstructionExpr (Jalr (X1, X2, 4));
    InstructionExpr (Jr X3);
    InstructionExpr (J 8);
  ] in
  let final_state = interpret initial_state program in
  Printf.printf "Final State: %s\n" (show_state final_state)
