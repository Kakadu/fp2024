(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap = Map.Make (String)
module Int64Map = Map.Make (Int64)

type state =
  { registers : int64 StringMap.t
  ; memory : int64 Int64Map.t
  ; memory_writable : bool Int64Map.t (* false - not writable, true - writable *)
  ; pc : int64
  }

module type MONADERROR = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module ErrorMonad : MONADERROR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return x = Ok x

  let ( >>= ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let fail e = Error e

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

open ErrorMonad
open ErrorMonad.Syntax

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
;;

let resolve_label_including_directives program label =
  let rec aux idx = function
    | [] -> -1L
    | LabelExpr lbl :: _ when lbl = label -> idx
    | _ :: tl -> aux (Int64.add idx 4L) tl
  in
  aux 0L program
;;

let resolve_label_excluding_directives program label =
  let rec aux idx = function
    | [] -> -1L
    | LabelExpr lbl :: _ when lbl = label -> idx
    | InstructionExpr _ :: tl -> aux (Int64.add idx 4L) tl
    | DirectiveExpr (Word _) :: tl -> aux (Int64.add idx 4L) tl
    | DirectiveExpr (Space integer) :: tl -> aux (Int64.add idx (Int64.of_int integer)) tl
    | _ :: tl -> aux idx tl
  in
  aux 0L program
;;

type address_info =
  | Immediate of int
  | Label of int64

let get_address12_value program = function
  | ImmediateAddress12 value -> Immediate value
  | LabelAddress12 label -> Label (resolve_label_excluding_directives program label)
;;

let get_address20_value program = function
  | ImmediateAddress20 value -> Immediate value
  | LabelAddress20 label -> Label (resolve_label_excluding_directives program label)
;;

let get_address32_value program = function
  | ImmediateAddress32 value -> Immediate value
  | LabelAddress32 label -> Label (resolve_label_excluding_directives program label)
;;

let init_data program =
  let rec traverse_program program temporary_pc_counter memory memory_writable =
    match program with
    | [] -> memory, memory_writable
    | InstructionExpr _ :: rest ->
      let memory_writable =
        List.fold_left
          (fun acc offset ->
            Int64Map.add (Int64.add temporary_pc_counter offset) false acc)
            (* instructions unwritable *)
          memory_writable
          [ 0L; 1L; 2L; 3L ]
      in
      traverse_program rest (Int64.add temporary_pc_counter 4L) memory memory_writable
    | DirectiveExpr (Word integer) :: rest ->
      let memory = Int64Map.add temporary_pc_counter (Int64.of_int integer) memory in
      let memory_writable =
        Int64Map.add temporary_pc_counter true memory_writable (* first byte writable *)
      in
      let memory_writable =
        List.fold_left
          (fun acc offset ->
            Int64Map.add (Int64.add temporary_pc_counter offset) false acc)
            (* rest is unwritable *)
          memory_writable
          [ 1L; 2L; 3L ]
      in
      traverse_program rest (Int64.add temporary_pc_counter 4L) memory memory_writable
    | DirectiveExpr (Space integer) :: rest ->
      traverse_program
        rest
        (Int64.add temporary_pc_counter (Int64.of_int integer))
        memory
        memory_writable
    | _ :: rest ->
      (* skip over labels, other directives *)
      traverse_program rest temporary_pc_counter memory memory_writable
  in
  traverse_program program 0L Int64Map.empty Int64Map.empty
;;

let init_state program =
  let registers =
    List.fold_left
      (fun acc reg -> StringMap.add (show_register reg) 0L acc)
      StringMap.empty
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
      ]
  in
  let memory, memory_writable = init_data program in
  { registers; memory; memory_writable; pc = 0L }
;;

let get_register_value state reg =
  StringMap.find_opt (show_register (handle_alternative_names reg)) state.registers
  |> Option.value ~default:0L
;;

let set_register_value state reg value =
  { state with
    registers =
      StringMap.add (show_register (handle_alternative_names reg)) value state.registers
  }
;;

let nth_opt_int64 l n =
  if n < 0L
  then fail "Index cannot be negative"
  else (
    let rec nth_aux l n =
      match l with
      | [] -> return None
      | a :: l ->
        (match n = 0L with
         | true -> return (Some a)
         | false -> nth_aux l (Int64.sub n 4L))
    in
    nth_aux l n)
;;

let set_pc state new_pc = { state with pc = new_pc }
let increment_pc state = { state with pc = Int64.add state.pc 4L }

(* Get index from including directives and labels to excluding *)
let resolve_address_incl_to_excl program immediate64_value =
  let rec traverse_program index remaining_value = function
    | [] ->
      fail
        "End of program reached before resolving immediate address from including \
         directives to excluding"
    | InstructionExpr _ :: _ when remaining_value = 4L -> return (Int64.add index 4L)
    | DirectiveExpr (Word _) :: _ when remaining_value = 4L -> return (Int64.add index 4L)
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest ->
      traverse_program index (Int64.sub remaining_value 4L) rest
    | InstructionExpr _ :: rest ->
      traverse_program (Int64.add index 4L) (Int64.sub remaining_value 4L) rest
  in
  traverse_program 0L immediate64_value program
;;

(* Get index from excluding directives and labels to including *)
let resolve_address_excl_to_incl program immediate64_value =
  let rec traverse_program index remaining_value = function
    | [] ->
      fail
        "End of program reached before resolving immediate address from excluding \
         directives to including"
    | InstructionExpr _ :: _ when remaining_value = 4L -> return (Int64.add index 4L)
    | DirectiveExpr (Word _) :: _ when remaining_value = 4L -> return (Int64.add index 4L)
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest ->
      traverse_program (Int64.add index 4L) remaining_value rest
    | InstructionExpr _ :: rest ->
      traverse_program (Int64.add index 4L) (Int64.sub remaining_value 4L) rest
  in
  traverse_program 0L immediate64_value program
;;

let handle_branch_condition state program rs1 rs2 imm_value comparison_fn =
  let val_rs1 = get_register_value state rs1 in
  let val_rs2 =
    match rs2 with
    | Some rs -> get_register_value state rs
    | None -> 0L
  in
  let address_info = get_address12_value program imm_value in
  let* new_pc =
    match address_info with
    | Immediate imm_value when comparison_fn val_rs1 val_rs2 ->
      let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
      let* target_pc =
        resolve_address_excl_to_incl
          program
          (Int64.add (Int64.of_int imm_value) current_pc_excl)
      in
      return target_pc
    | Immediate _ -> return (Int64.add state.pc 4L)
    | Label excluding_directives_label_offset when comparison_fn val_rs1 val_rs2 ->
      let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
      let* target_pc =
        resolve_address_excl_to_incl
          program
          (Int64.add excluding_directives_label_offset current_pc_excl)
      in
      return target_pc
    | Label _ -> return (Int64.add state.pc 4L)
  in
  return (set_pc state new_pc)
;;

let execute_arithmetic_op state rd rs1 rs2 op sext =
  let val1 = get_register_value state rs1 in
  let val2 = get_register_value state rs2 in
  let result = op val1 val2 in
  let result_final =
    match sext with
    | true -> Int64.shift_right (Int64.shift_left (Int64.logand result 0xFFFFFFFFL) 32) 32
    | false -> result
  in
  return (set_register_value state rd result_final)
;;

let execute_shift_op state rd rs1 rs2 op =
  let val1 = get_register_value state rs1 in
  let val2 = get_register_value state rs2 in
  let result = op val1 (Int64.to_int val2) in
  return (set_register_value state rd result)
;;

let execute_comparison_op state rd rs1 rs2 compare_fn =
  let val1 = get_register_value state rs1 in
  let val2 = get_register_value state rs2 in
  let result =
    match compare_fn val1 val2 with
    | true -> 1L
    | false -> 0L
  in
  return (set_register_value state rd result)
;;

let execute_immediate_op state program rd rs1 imm op sext =
  let val1 = get_register_value state rs1 in
  let address_info = get_address12_value program imm in
  let imm_value =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | Label excluding_directives_label_offset -> excluding_directives_label_offset
  in
  let result = op val1 imm_value in
  let result_final =
    match sext with
    | true -> Int64.shift_right (Int64.shift_left (Int64.logand result 0xFFFFFFFFL) 32) 32
    | false -> result
  in
  return (set_register_value state rd result_final)
;;

let execute_shift_immediate_op state program rd rs1 imm op =
  let val1 = get_register_value state rs1 in
  let imm_value =
    match get_address12_value program imm with
    | Immediate imm_value -> imm_value
    | Label excluding_directives_label_offset ->
      Int64.to_int excluding_directives_label_offset
  in
  let result = op val1 imm_value in
  return (set_register_value state rd result)
;;

let load_memory state address = function
  | (1 | 2 | 4) as size ->
    (* Check if writable. If not, it is either an instruction, or partial data, which we do not support. *)
    (match Int64Map.find_opt address state.memory_writable with
     | Some false ->
       fail
         "Load failed: Address is not writable, either an instruction address or points \
          to the middle of the data"
     | _ ->
       (match Int64Map.find_opt address state.memory with
        | Some value ->
          let* result =
            match size with
            | 1 -> return (Int64.logand value 0xFFL)
            | 2 -> return (Int64.logand value 0xFFFFL)
            | 4 -> return value
            | _ -> fail "Unsupported load size"
          in
          return result
        | None -> fail "Load failed: Address not found in memory"))
  | _ -> fail "Unsupported load size"
;;

let store_memory state address value size =
  let* stored_value =
    match size with
    | 1 -> return (Int64.logand value 0xFFL)
    | 2 -> return (Int64.logand value 0xFFFFL)
    | 4 -> return value
    | _ -> fail "Unsupported store size"
  in
  (* Check if the address is writable *)
  match Int64Map.find_opt address state.memory_writable with
  | Some false ->
    fail
      "Store failed: Address is not writable, either an instruction address or points to \
       the middle of the data"
  | _ ->
    let memory = Int64Map.add address stored_value state.memory in
    let* memory_writable =
      match size with
      | 1 ->
        let memory_writable =
          state.memory_writable
          |> Int64Map.add address false
          |> Int64Map.add (Int64.add address 1L) true
          |> Int64Map.add (Int64.add address 2L) true
          |> Int64Map.add (Int64.add address 3L) true
        in
        return memory_writable
      | 2 ->
        let memory_writable =
          state.memory_writable
          |> Int64Map.add address false
          |> Int64Map.add (Int64.add address 1L) false
          |> Int64Map.add (Int64.add address 2L) true
          |> Int64Map.add (Int64.add address 3L) true
        in
        return memory_writable
      | 4 ->
        let memory_writable =
          state.memory_writable
          |> Int64Map.add address false
          |> Int64Map.add (Int64.add address 1L) false
          |> Int64Map.add (Int64.add address 2L) false
          |> Int64Map.add (Int64.add address 3L) false
        in
        return memory_writable
      | _ -> fail "Unsupported store size"
    in
    return { state with memory; memory_writable }
;;

let execute_load state program rd rs1 imm size signed =
  let base_address = get_register_value state rs1 in
  let* offset =
    match get_address12_value program imm with
    | Immediate imm_value -> return (Int64.of_int imm_value)
    | Label _ -> fail "Label offset not supported for load/store"
  in
  let address = Int64.add base_address offset in
  let* value = load_memory state address size in
  let result =
    if signed
    then (
      match size with
      | 1 -> Int64.shift_right (Int64.shift_left value 56) 56
      | 2 -> Int64.shift_right (Int64.shift_left value 48) 48
      | _ -> value)
    else value
  in
  return (set_register_value state rd result)
;;

let execute_store state program rs1 rs2 imm size =
  let base_address = get_register_value state rs1 in
  let* offset =
    match get_address12_value program imm with
    | Immediate imm_value -> return (Int64.of_int imm_value)
    | Label _ -> fail "Label offset not supported for load/store"
  in
  let address = Int64.add base_address offset in
  let value = get_register_value state rs2 in
  let* new_state = store_memory state address value size in
  return new_state
;;

let handle_syscall state =
  let syscall_number = get_register_value state A0 in
  let arg1 = get_register_value state A1 in
  let arg2 = get_register_value state A2 in
  let arg3 = get_register_value state A3 in
  match syscall_number with
  | 93L ->
    let exit_code = Int64.to_int arg1 in
    exit exit_code
  | 64L ->
    let fd = Int64.to_int arg1 in
    let buf = arg2 in
    let count = Int64.to_int arg3 in
    if fd = 1
    then (
      let rec read_memory addr count acc =
        if count <= 0
        then return (String.init (List.length acc) (fun i -> List.nth acc i))
        else
          let* byte = load_memory state addr 1 in
          read_memory (Int64.add addr 1L) (count - 1) (Char.chr (Int64.to_int byte) :: acc)
      in
      let* str = read_memory buf count [] in
      Printf.printf "%s" str;
      return state)
    else fail "Unsupported file descriptor for write syscall"
  | _ -> fail "Unsupported syscall"
;;

let rec interpret state program =
  let* instr_opt = nth_opt_int64 program state.pc in
  match instr_opt with
  | None -> return state
  | Some (InstructionExpr instr) ->
    let* new_state = execute_instruction state instr program in
    interpret (increment_pc new_state) program
  | Some (LabelExpr _) -> interpret (increment_pc state) program
  | Some (DirectiveExpr _) -> interpret (increment_pc state) program

and execute_instruction state instr program =
  match instr with
  | Add (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.add false
  | Sub (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.sub false
  | Xor (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.logxor false
  | Or (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.logor false
  | And (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.logand false
  | Sll (rd, rs1, rs2) -> execute_shift_op state rd rs1 rs2 Int64.shift_left
  | Srl (rd, rs1, rs2) -> execute_shift_op state rd rs1 rs2 Int64.shift_right_logical
  | Sra (rd, rs1, rs2) -> execute_shift_op state rd rs1 rs2 Int64.shift_right
  | Slt (rd, rs1, rs2) ->
    execute_comparison_op state rd rs1 rs2 (fun arg1 arg2 -> Int64.compare arg1 arg2 < 0)
  | Sltu (rd, rs1, rs2) ->
    execute_comparison_op state rd rs1 rs2 (fun arg1 arg2 ->
      Int64.unsigned_compare arg1 arg2 < 0)
  | Addi (rd, rs1, imm) -> execute_immediate_op state program rd rs1 imm Int64.add false
  | Xori (rd, rs1, imm) ->
    execute_immediate_op state program rd rs1 imm Int64.logxor false
  | Ori (rd, rs1, imm) -> execute_immediate_op state program rd rs1 imm Int64.logor false
  | Andi (rd, rs1, imm) ->
    execute_immediate_op state program rd rs1 imm Int64.logand false
  | Slli (rd, rs1, imm) ->
    execute_shift_immediate_op state program rd rs1 imm Int64.shift_left
  | Srli (rd, rs1, imm) ->
    execute_shift_immediate_op state program rd rs1 imm Int64.shift_right_logical
  | Srai (rd, rs1, imm) ->
    execute_shift_immediate_op state program rd rs1 imm Int64.shift_right
  | Slti (rd, rs1, imm) ->
    execute_immediate_op
      state
      program
      rd
      rs1
      imm
      (fun arg1 imm_value ->
        match Int64.compare arg1 imm_value with
        | x when x < 0 -> 1L
        | _ -> 0L)
      false
  | Sltiu (rd, rs1, imm) ->
    execute_immediate_op
      state
      program
      rd
      rs1
      imm
      (fun arg1 imm_value ->
        match Int64.unsigned_compare arg1 imm_value with
        | x when x < 0 -> 1L
        | _ -> 0L)
      false
  | Lb (rd, rs1, imm) -> execute_load state program rd rs1 imm 1 true
  | Lh (rd, rs1, imm) -> execute_load state program rd rs1 imm 2 true
  | Lw (rd, rs1, imm) -> execute_load state program rd rs1 imm 4 true
  | Lbu (rd, rs1, imm) -> execute_load state program rd rs1 imm 1 false
  | Lhu (rd, rs1, imm) -> execute_load state program rd rs1 imm 2 false
  | Sb (rs1, rs2, imm) -> execute_store state program rs1 rs2 imm 1
  | Sh (rs1, rs2, imm) -> execute_store state program rs1 rs2 imm 2
  | Sw (rs1, rs2, imm) -> execute_store state program rs1 rs2 imm 4
  | Beq (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 = arg2 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Beqz (rs1, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 = arg2 in
    handle_branch_condition state program rs1 None imm_value comparison_fn
  | Bne (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 <> arg2 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Bnez (rs1, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 <> arg2 in
    handle_branch_condition state program rs1 None imm_value comparison_fn
  | Blt (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 < arg2 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Bltz (rs1, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 < arg2 in
    handle_branch_condition state program rs1 None imm_value comparison_fn
  | Bgt (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 > arg2 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Bgtz (rs1, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 > arg2 in
    handle_branch_condition state program rs1 None imm_value comparison_fn
  | Bge (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = arg1 >= arg2 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Bltu (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = Int64.unsigned_compare arg1 arg2 < 0 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Bgeu (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = Int64.unsigned_compare arg1 arg2 >= 0 in
    handle_branch_condition state program rs1 (Some rs2) imm_value comparison_fn
  | Jal (rd, imm_value) ->
    let address_info = get_address20_value program imm_value in
    let* new_pc =
      match address_info with
      | Immediate imm_value ->
        let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
        resolve_address_excl_to_incl
          program
          (Int64.add (Int64.of_int imm_value) current_pc_excl)
      | Label excluding_directives_label_offset ->
        let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
        resolve_address_excl_to_incl
          program
          (Int64.add excluding_directives_label_offset current_pc_excl)
    in
    let new_state = set_register_value state rd (Int64.add state.pc 1L) in
    return (set_pc new_state new_pc)
  | Jalr (rd, rs1, imm) ->
    let val_rs1 = get_register_value state rs1 in
    let address_info = get_address12_value program imm in
    let* new_pc =
      match address_info with
      | Immediate imm_value ->
        resolve_address_excl_to_incl program (Int64.add val_rs1 (Int64.of_int imm_value))
      | Label excluding_directives_label_offset ->
        resolve_address_excl_to_incl
          program
          (Int64.add excluding_directives_label_offset val_rs1)
    in
    let new_state = set_register_value state rd (Int64.add state.pc 1L) in
    return (set_pc new_state new_pc)
  | Jr rs1 ->
    let val_rs1 = get_register_value state rs1 in
    let* new_pc = resolve_address_excl_to_incl program val_rs1 in
    let new_state = set_register_value state X0 (Int64.add state.pc 1L) in
    return (set_pc new_state new_pc)
  | J imm_value ->
    let address_info = get_address20_value program imm_value in
    let* new_pc =
      match address_info with
      | Immediate imm_value ->
        let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
        resolve_address_excl_to_incl
          program
          (Int64.add (Int64.of_int imm_value) current_pc_excl)
      | Label excluding_directives_label_offset ->
        let* current_pc_excl = resolve_address_incl_to_excl program state.pc in
        resolve_address_excl_to_incl
          program
          (Int64.add excluding_directives_label_offset current_pc_excl)
    in
    let new_state = set_register_value state X0 (Int64.add state.pc 1L) in
    return (set_pc new_state new_pc)
  | Lui (rd, imm) ->
    let imm_value =
      match imm with
      | ImmediateAddress20 value -> Int64.shift_left (Int64.of_int value) 12
      | LabelAddress20 label ->
        let label_offset = resolve_label_excluding_directives program label in
        Int64.shift_left label_offset 12
    in
    return (set_register_value state rd imm_value)
  | Auipc (rd, imm) ->
    let imm_value =
      match imm with
      | ImmediateAddress20 value -> Int64.shift_left (Int64.of_int value) 12
      | LabelAddress20 label ->
        let label_offset = resolve_label_excluding_directives program label in
        Int64.shift_left label_offset 12
    in
    let new_value = Int64.add state.pc imm_value in
    return (set_register_value state rd new_value)
  | Ecall -> handle_syscall state
  | Mul (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.mul false
  | Div (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.div false
  | Rem (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.rem false
  | Lwu (rd, rs1, imm) -> execute_load state program rd rs1 imm 4 false
  | Ld (rd, rs1, imm) -> execute_load state program rd rs1 imm 8 true
  | Sd (rs1, rs2, imm) -> execute_store state program rs1 rs2 imm 8
  | Mv (rd, rs) ->
    execute_immediate_op state program rd rs (ImmediateAddress12 0) Int64.add false
  | Li (rd, imm) ->
    let imm_value =
      match imm with
      | ImmediateAddress32 value -> Int64.of_int value
      | LabelAddress32 label -> resolve_label_excluding_directives program label
    in
    return (set_register_value state rd imm_value)
  | Addiw (rd, rs1, imm) -> execute_immediate_op state program rd rs1 imm Int64.add true
  | Mulw (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.mul true
  | Addw (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.add true
  | Subw (rd, rs1, rs2) -> execute_arithmetic_op state rd rs1 rs2 Int64.sub true
  | Ret ->
    let val_rs1 = get_register_value state X1 in
    let* new_pc = resolve_address_excl_to_incl program val_rs1 in
    let new_state = set_register_value state X0 (Int64.add state.pc 1L) in
    return (set_pc new_state new_pc)
  | _ -> return state
;;

let show_state state =
  let registers_str =
    StringMap.fold
      (fun reg value acc -> acc ^ Printf.sprintf "%s: %Ld\n" reg value)
      state.registers
      ""
  in
  let pc_str = Printf.sprintf "PC: %Ld" state.pc in
  registers_str ^ pc_str
;;
