(** Copyright 2024-2025, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap = Map.Make (String)
module Int64Map = Map.Make (Int64)

type state =
  { program : ast
  ; registers : int64 StringMap.t
  ; vregisters : int64 array StringMap.t
  ; max_vector_length : int
  ; vector_element_length : int
  ; vector_length : int
  ; memory_int : int64 Int64Map.t
  ; memory_str : string Int64Map.t
  ; memory_writable : bool Int64Map.t
  ; program_idx : int64
  }

module type COMBINED_MONAD = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val fail : string -> ('s, 'a) t
  val read : ('s, 's) t
  val write : 's -> ('s, unit) t
  val run : ('s, 'a) t -> 's -> ('s * 'a, string) result

  module Syntax : sig
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end
end

module CombinedMonad : COMBINED_MONAD = struct
  type ('s, 'a) t = 's -> ('s * 'a, string) result

  let return x s = Ok (s, x)

  let ( >>= ) m f s =
    match m s with
    | Ok (s', x) -> f x s'
    | Error e -> Error e
  ;;

  let fail e _ = Error e
  let read s = Ok (s, s)
  let write s _ = Ok (s, ())
  let run m s = m s

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

open CombinedMonad
open CombinedMonad.Syntax

let handle_alternative_register_names = function
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

let resolve_label_address label =
  let* state = read in
  let rec traverse_program idx = function
    | [] -> fail "Label cannot be resolved"
    | LabelExpr lbl :: _ when lbl = label -> return idx
    | InstructionExpr _ :: tl ->
      traverse_program
        (Int64.add idx 4L)
        tl (* each supported instruction is stored in 4 bytes *)
    | DirectiveExpr (Word _) :: tl ->
      traverse_program (Int64.add idx 4L) tl (* each word is stored in 4 bytes *)
    | DirectiveExpr (Space amount) :: tl ->
      traverse_program
        (Int64.add idx (Int64.of_int amount))
        tl (* space A takes A bytes *)
    | DirectiveExpr (StringDir str) :: tl ->
      traverse_program
        (Int64.add idx (Int64.of_int (String.length str)))
        tl (* string takes amount of bytes equal to its length *)
    | _ :: tl -> traverse_program idx tl
  in
  traverse_program 0L state.program
;;

(** Information about some address can be stored in 2 variants *)
type address_info =
  | Immediate of int (** Immediate value *)
  | LabelAddress of int64 (** Label (resolved in terms of addresses) *)

let get_address12_value = function
  | ImmediateAddress12 value -> return (Immediate value)
  | LabelAddress12 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

let get_address20_value = function
  | ImmediateAddress20 value -> return (Immediate value)
  | LabelAddress20 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

let get_address32_value = function
  | ImmediateAddress32 value -> return (Immediate value)
  | LabelAddress32 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

(* used to initialize string and int64 memory from directives, and make instuctions' addresses non-writable *)
let init_data program =
  let rec traverse_program program idx memory_int memory_str memory_writable =
    match program with
    | [] -> memory_int, memory_str, memory_writable
    | InstructionExpr _ :: rest ->
      (* make all 4 instruction bytes non-writable *)
      let memory_writable =
        List.fold_left
          (fun acc offset -> Int64Map.add (Int64.add idx offset) false acc)
          memory_writable
          [ 0L; 1L; 2L; 3L ]
      in
      traverse_program rest (Int64.add idx 4L) memory_int memory_str memory_writable
    | DirectiveExpr (Word integer) :: rest ->
      (* make all word bytes except first non-writable *)
      let memory_int = Int64Map.add idx (Int64.of_int integer) memory_int in
      let memory_writable = Int64Map.add idx true memory_writable in
      let memory_writable =
        List.fold_left
          (fun acc offset -> Int64Map.add (Int64.add idx offset) false acc)
          memory_writable
          [ 1L; 2L; 3L ]
      in
      traverse_program rest (Int64.add idx 4L) memory_int memory_str memory_writable
    | DirectiveExpr (Space amount) :: rest ->
      (* skip amount of bytes used by space *)
      traverse_program
        rest
        (Int64.add idx (Int64.of_int amount))
        memory_int
        memory_str
        memory_writable
    | DirectiveExpr (StringDir str) :: rest ->
      (* make all string bytes except first non-writable *)
      let memory_int = Int64Map.add idx 0L memory_int in
      let memory_str = Int64Map.add idx str memory_str in
      let str_length = String.length str in
      let memory_writable = Int64Map.add idx true memory_writable in
      let memory_writable =
        List.fold_left
          (fun acc offset -> Int64Map.add (Int64.add idx offset) false acc)
          memory_writable
          (List.init (str_length - 1) (fun i -> Int64.of_int (i + 1)))
      in
      traverse_program
        rest
        (Int64.add idx (Int64.of_int str_length))
        memory_int
        memory_str
        memory_writable
    | _ :: rest -> traverse_program rest idx memory_int memory_str memory_writable
  in
  traverse_program program 0L Int64Map.empty Int64Map.empty Int64Map.empty
;;

let init_registers =
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
;;

let init_vregisters vector_length =
  List.fold_left
    (fun acc reg ->
      StringMap.add (show_vector_register reg) (Array.make vector_length 0L) acc)
    StringMap.empty
    [ V0
    ; V1
    ; V2
    ; V3
    ; V4
    ; V5
    ; V6
    ; V7
    ; V8
    ; V9
    ; V10
    ; V11
    ; V12
    ; V13
    ; V14
    ; V15
    ; V16
    ; V17
    ; V18
    ; V19
    ; V20
    ; V21
    ; V22
    ; V23
    ; V24
    ; V25
    ; V26
    ; V27
    ; V28
    ; V29
    ; V30
    ; V31
    ]
;;

let init_state program =
  let registers = init_registers in
  let max_vector_length = 128 in
  let vector_element_length = 4 in
  let vector_length = 4 in
  let vregisters = init_vregisters vector_length in
  let memory_int, memory_str, memory_writable = init_data program in
  { program
  ; registers
  ; vregisters
  ; max_vector_length
  ; vector_element_length
  ; vector_length
  ; memory_int
  ; memory_str
  ; memory_writable
  ; program_idx = 0L
  }
;;

let get_register_value reg =
  let* state = read in
  return
    (StringMap.find_opt
       (show_register (handle_alternative_register_names reg))
       state.registers
     |> Option.value ~default:0L)
;;

let set_register_value reg value =
  let* state = read in
  let new_state =
    match reg with
    | X0 -> state
    | _ ->
      { state with
        registers =
          StringMap.add
            (show_register (handle_alternative_register_names reg))
            value
            state.registers
      }
  in
  write new_state
;;

let get_vregister_value vreg =
  let* state = read in
  return
    (StringMap.find_opt (show_vector_register vreg) state.vregisters
     |> Option.value ~default:(Array.make state.vector_length 0L))
;;

let set_vregister_value vreg value =
  let* state = read in
  let new_state =
    { state with
      vregisters = StringMap.add (show_vector_register vreg) value state.vregisters
    }
  in
  write new_state
;;

let set_program_idx new_idx =
  let* state = read in
  let new_state = { state with program_idx = new_idx } in
  write new_state
;;

let increment_program_idx () =
  let* state = read in
  let new_state = { state with program_idx = Int64.add state.program_idx 4L } in
  write new_state
;;

let translate_index_to_address immediate64_value =
  let* state = read in
  let rec traverse_program address remaining_value = function
    | [] ->
      (match remaining_value with
       | 0L -> return address
       | _ -> fail "End of program reached before resolving address from index")
    | InstructionExpr _ :: _ when remaining_value = 0L -> return address
    | DirectiveExpr (Word _) :: _ when remaining_value = 0L -> return address
    | DirectiveExpr (Space _) :: _ when remaining_value = 0L -> return address
    | DirectiveExpr (StringDir _) :: _ when remaining_value = 0L -> return address
    | DirectiveExpr (Word _) :: rest | InstructionExpr _ :: rest ->
      (* make one step and subtract needed amount of bytes (word/instruction size) from remaining value *)
      traverse_program (Int64.add address 4L) (Int64.sub remaining_value 4L) rest
    | DirectiveExpr (Space amount) :: rest ->
      (* make one step and subtract needed amount of bytes (space size) from remaining value *)
      traverse_program
        (Int64.add address 4L)
        (Int64.sub remaining_value (Int64.of_int amount))
        rest
    | DirectiveExpr (StringDir str) :: rest ->
      (* make one step and subtract needed amount of bytes (string size) from remaining value *)
      traverse_program
        (Int64.add address 4L)
        (Int64.sub remaining_value (Int64.of_int (String.length str)))
        rest
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest ->
      (* other directives and labels don't have own address, so we just make a step for indices *)
      traverse_program address (Int64.sub remaining_value 4L) rest
  in
  traverse_program 0L immediate64_value state.program
;;

let translate_address_to_index immediate64_value =
  let* state = read in
  let rec traverse_program index remaining_value = function
    | [] ->
      (match remaining_value with
       | 0L -> return index
       | _ -> fail "End of program reached before resolving index from address")
    | InstructionExpr _ :: _ when remaining_value = 0L -> return index
    | DirectiveExpr (Word _) :: _ when remaining_value = 0L -> return index
    | DirectiveExpr (Space _) :: _ when remaining_value = 0L -> return index
    | DirectiveExpr (StringDir _) :: _ when remaining_value = 0L -> return index
    | DirectiveExpr (Word _) :: rest | InstructionExpr _ :: rest ->
      (* make one step and subtract needed amount of bytes (word/instruction size) from remaining value *)
      traverse_program (Int64.add index 4L) (Int64.sub remaining_value 4L) rest
    | DirectiveExpr (Space amount) :: rest ->
      (* make one step and subtract needed amount of bytes (space size) from remaining value *)
      traverse_program
        (Int64.add index 4L)
        (Int64.sub remaining_value (Int64.of_int amount))
        rest
    | DirectiveExpr (StringDir str) :: rest ->
      (* make one step and subtract needed amount of bytes (string size) from remaining value *)
      traverse_program
        (Int64.add index 4L)
        (Int64.sub remaining_value (Int64.of_int (String.length str)))
        rest
    | DirectiveExpr _ :: rest | LabelExpr _ :: rest ->
      (* other directives and labels don't have own address, so we just make a step for indices *)
      traverse_program (Int64.add index 4L) remaining_value rest
  in
  traverse_program 0L immediate64_value state.program
;;

let handle_branch_condition rs1 rs2 address_value comparison_fn =
  let* state = read in
  let* val_rs1 = get_register_value rs1 in
  let* val_rs2 =
    match rs2 with
    | Some rs -> get_register_value rs
    | None -> return 0L
  in
  let* address_info = get_address12_value address_value in
  let* new_program_idx =
    match address_info with
    | Immediate imm_value when comparison_fn val_rs1 val_rs2 ->
      let* current_address = translate_index_to_address state.program_idx in
      let* target_index =
        translate_address_to_index (Int64.add (Int64.of_int imm_value) current_address)
      in
      return (Int64.sub target_index 4L)
    | Immediate _ -> return state.program_idx
    | LabelAddress label_address when comparison_fn val_rs1 val_rs2 ->
      let* target_index = translate_address_to_index label_address in
      return (Int64.sub target_index 4L)
    | LabelAddress _ -> return state.program_idx
  in
  set_program_idx new_program_idx
;;

let sext x = Int64.shift_right (Int64.shift_left (Int64.logand x 0xFFFFFFFFL) 32) 32

let zext x =
  Int64.shift_right_logical (Int64.shift_left (Int64.logand x 0xFFFFFFFFL) 32) 32
;;

let execute_arithmetic_op rd rs1 rs2 op to_sext =
  let* val1 = get_register_value rs1 in
  let* val2 = get_register_value rs2 in
  let result = op val1 val2 in
  let result_final = if to_sext then sext result else result in
  set_register_value rd result_final
;;

let execute_shift_op rd rs1 rs2 op =
  let* val1 = get_register_value rs1 in
  let* val2 = get_register_value rs2 in
  let val2_lower_5bits = Int64.logand val2 0x1FL in
  let result = op val1 (Int64.to_int val2_lower_5bits) in
  set_register_value rd result
;;
let execute_comparison_op rd rs1 rs2 compare_fn =
  let* val1 = get_register_value rs1 in
  let* val2 = get_register_value rs2 in
  let result = if compare_fn val1 val2 then 1L else 0L in
  set_register_value rd result
;;

let execute_immediate_op rd rs1 imm op to_sext =
  let* val1 = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let address_value =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let result = op val1 (sext address_value) in
  let result_final = if to_sext then sext result else result in
  set_register_value rd result_final
;;

let execute_shift_immediate_op rd rs1 imm op =
  let* val1 = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let shamt =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let shamt = Int64.to_int (Int64.logand shamt 0x3FL) in
  let result = op val1 shamt in
  set_register_value rd result
;;

let execute_shnadd rd rs1 rs2 n to_zext =
  let* val1 = get_register_value rs1 in
  let* val2 = get_register_value rs2 in
  let arg2 = if to_zext then zext val2 else val2 in
  let result = Int64.add val1 (Int64.shift_left arg2 n) in
  set_register_value rd result
;;

let load_memory_int address size =
  let* state = read in
  match size with
  | 1 | 2 | 4 ->
    (match Int64Map.find_opt address state.memory_writable with
     | Some false ->
       fail
         "Load failed: address is non-writable, either an instruction address or another \
          data points to the middle of the data"
     | _ ->
       (match Int64Map.find_opt address state.memory_int with
        | Some value ->
          let* result =
            match size with
            | 1 -> return (Int64.logand value 0xFFL)
            | 2 -> return (Int64.logand value 0xFFFFL)
            | 4 -> return value
            | _ -> fail "Unsupported load size"
          in
          return result
        | None -> fail "Load failed: address is not found in memory"))
  | _ -> fail "Unsupported load size"
;;

let store_memory_int address value size =
  let* state = read in
  let* stored_value =
    match size with
    | 1 -> return (Int64.logand value 0xFFL)
    | 2 -> return (Int64.logand value 0xFFFFL)
    | 4 -> return value
    | _ -> fail "Unsupported store size"
  in
  match Int64Map.find_opt address state.memory_writable with
  | Some false -> fail "Store failed: address is non-writable"
  | _ ->
    let memory_int = Int64Map.add address stored_value state.memory_int in
    let memory_str = Int64Map.add address "" state.memory_str in
    let* memory_writable =
      match size with
      (* first byte is always writable whereas all other bytes not *)
      | 1 ->
        let memory_writable = state.memory_writable |> Int64Map.add address true in
        return memory_writable
      | 2 ->
        let memory_writable =
          state.memory_writable
          |> Int64Map.add address true
          |> Int64Map.add (Int64.add address 1L) false
        in
        return memory_writable
      | 4 ->
        let memory_writable =
          state.memory_writable
          |> Int64Map.add address true
          |> Int64Map.add (Int64.add address 1L) false
          |> Int64Map.add (Int64.add address 2L) false
          |> Int64Map.add (Int64.add address 3L) false
        in
        return memory_writable
      | _ -> fail "Unsupported store size"
    in
    let new_state = { state with memory_int; memory_str; memory_writable } in
    write new_state
;;

let execute_load_int rd rs1 imm size is_signed =
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address (sext offset) in
  let* value = load_memory_int address size in
  let result = if is_signed then sext value else zext value in
  set_register_value rd result
;;

let execute_store_int rs1 rs2 imm size =
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address (sext offset) in
  let* value = get_register_value rs2 in
  store_memory_int address value size
;;

let execute_vle32v vd rs1 imm =
  let* state = read in
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address offset in
  let vector_length = state.vector_length in
  let rec load_values address element_idx acc =
    if element_idx < vector_length
    then
      let* value = load_memory_int address state.vector_element_length in
      let next_address = Int64.add address (Int64.of_int state.vector_element_length) in
      load_values next_address (element_idx + 1) (Array.append acc [| value |])
    else return acc
  in
  let* vector_values = load_values address 0 [||] in
  set_vregister_value vd vector_values
;;

let execute_vstore vs rs1 imm =
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> Int64.of_int imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address offset in
  let* vector_value = get_vregister_value vs in
  let rec store_values element_idx addr =
    let* state = read in
    if element_idx < state.vector_length
    then (
      let element = vector_value.(element_idx) in
      let* () = store_memory_int addr element state.vector_element_length in
      store_values (element_idx + 1) (Int64.add addr 4L))
    else return ()
  in
  store_values 0 address
;;

let execute_vector_arithmetic vd vs1 vs2 op =
  let* state = read in
  let* vec1 = get_vregister_value vs1 in
  let* vec2 = get_vregister_value vs2 in
  let result = Array.init state.vector_length (fun i -> op vec1.(i) vec2.(i)) in
  set_vregister_value vd result
;;

let execute_vector_imm vd vs1 rs2 op =
  let* state = read in
  let* vec = get_vregister_value vs1 in
  let* scalar = get_register_value rs2 in
  let result = Array.init state.vector_length (fun i -> op vec.(i) scalar) in
  set_vregister_value vd result
;;

let handle_syscall =
  let* state = read in
  let* syscall_number = get_register_value A7 in
  let* arg1 = get_register_value A0 in
  let* arg2 = get_register_value A1 in
  match syscall_number with
  | 93L ->
    (* perform exit *)
    let exit_code = Int64.to_int arg1 in
    exit exit_code
  | 64L ->
    (* perform write - take information from memory and put in fd (only stdout supported) *)
    let fd = Int64.to_int arg1 in
    let address = arg2 in
    (match fd with
     | 1 ->
       let str =
         try Int64Map.find address state.memory_str with
         | Not_found -> ""
       in
       if str = ""
       then
         let* int_value = load_memory_int address 4 in
         return (Printf.printf "%Ld\n" int_value)
       else return (Printf.printf "%s\n" str)
     | _ -> fail "Unsupported file descriptor for write syscall")
  | 63L ->
    (* perform read - read line and put input string into memory *)
    let fd = Int64.to_int arg1 in
    let buf = arg2 in
    (match fd with
     | 0 ->
       let str = read_line () in
       let memory_int = Int64Map.add buf 0L state.memory_int in
       let memory_str = Int64Map.add buf str state.memory_str in
       let memory_writable = Int64Map.add buf true state.memory_writable in
       let memory_writable =
         List.fold_left
           (fun acc offset -> Int64Map.add (Int64.add buf offset) false acc)
           memory_writable
           (List.init (String.length str - 1) (fun i -> Int64.of_int (i + 1)))
       in
       let new_state = { state with memory_int; memory_str; memory_writable } in
       write new_state
     | _ -> fail "Unsupported file descriptor for read syscall")
  | _ -> fail "Unsupported syscall"
;;

let execute_j imm_value =
  let* state = read in
  let* address_info = get_address20_value imm_value in
  let* new_pc =
    match address_info with
    | Immediate imm_value ->
      let* current_pc_excl = translate_index_to_address state.program_idx in
      let* resolved_pc =
        translate_address_to_index (Int64.add (Int64.of_int imm_value) current_pc_excl)
      in
      return resolved_pc
    | LabelAddress excluding_directives_label_offset ->
      let* resolved_pc = translate_address_to_index excluding_directives_label_offset in
      return resolved_pc
  in
  set_program_idx (Int64.sub new_pc 4L)
;;

let execute_instruction instr =
  let* state = read in
  match instr with
  | Add (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.add false
  | Sub (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.sub false
  | Xor (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.logxor false
  | Or (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.logor false
  | And (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.logand false
  | Sll (rd, rs1, rs2) -> execute_shift_op rd rs1 rs2 Int64.shift_left
  | Srl (rd, rs1, rs2) -> execute_shift_op rd rs1 rs2 Int64.shift_right_logical
  | Sra (rd, rs1, rs2) -> execute_shift_op rd rs1 rs2 Int64.shift_right
  | Slt (rd, rs1, rs2) ->
    execute_comparison_op rd rs1 rs2 (fun arg1 arg2 -> Int64.compare arg1 arg2 < 0)
  | Sltu (rd, rs1, rs2) ->
    execute_comparison_op rd rs1 rs2 (fun arg1 arg2 ->
      Int64.unsigned_compare arg1 arg2 < 0)
  | Addi (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.add false
  | Xori (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.logxor false
  | Ori (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.logor false
  | Andi (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.logand false
  | Slli (rd, rs1, imm) -> execute_shift_immediate_op rd rs1 imm Int64.shift_left
  | Srli (rd, rs1, imm) -> execute_shift_immediate_op rd rs1 imm Int64.shift_right_logical
  | Srai (rd, rs1, imm) -> execute_shift_immediate_op rd rs1 imm Int64.shift_right
  | Slti (rd, rs1, imm) ->
    execute_immediate_op
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
      rd
      rs1
      imm
      (fun arg1 imm_value ->
        match Int64.unsigned_compare arg1 imm_value with
        | x when x < 0 -> 1L
        | _ -> 0L)
      false
  | Lb (rd, rs1, imm) -> execute_load_int rd rs1 imm 1 true
  | Lh (rd, rs1, imm) -> execute_load_int rd rs1 imm 2 true
  | Lw (rd, rs1, imm) -> execute_load_int rd rs1 imm 4 true
  | Lbu (rd, rs1, imm) -> execute_load_int rd rs1 imm 1 false
  | Lhu (rd, rs1, imm) -> execute_load_int rd rs1 imm 2 false
  | Sb (rs1, rs2, imm) -> execute_store_int rs1 rs2 imm 1
  | Sh (rs1, rs2, imm) -> execute_store_int rs1 rs2 imm 2
  | Sw (rs1, rs2, imm) -> execute_store_int rs1 rs2 imm 4
  | Beq (rs1, rs2, imm_value) -> handle_branch_condition rs1 (Some rs2) imm_value ( = )
  | Beqz (rs1, imm_value) -> handle_branch_condition rs1 None imm_value ( = )
  | Bne (rs1, rs2, imm_value) -> handle_branch_condition rs1 (Some rs2) imm_value ( <> )
  | Bnez (rs1, imm_value) -> handle_branch_condition rs1 None imm_value ( <> )
  | Blt (rs1, rs2, imm_value) -> handle_branch_condition rs1 (Some rs2) imm_value ( < )
  | Bltz (rs1, imm_value) -> handle_branch_condition rs1 None imm_value ( < )
  | Bgt (rs1, rs2, imm_value) -> handle_branch_condition rs1 (Some rs2) imm_value ( > )
  | Bgtz (rs1, imm_value) -> handle_branch_condition rs1 None imm_value ( > )
  | Bge (rs1, rs2, imm_value) -> handle_branch_condition rs1 (Some rs2) imm_value ( >= )
  | Bltu (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = Int64.unsigned_compare arg1 arg2 < 0 in
    handle_branch_condition rs1 (Some rs2) imm_value comparison_fn
  | Bgeu (rs1, rs2, imm_value) ->
    let comparison_fn arg1 arg2 = Int64.unsigned_compare arg1 arg2 >= 0 in
    handle_branch_condition rs1 (Some rs2) imm_value comparison_fn
  | Jal (rd, imm_value) ->
    let* address_info = get_address20_value imm_value in
    let* new_pc =
      match address_info with
      | Immediate imm_value ->
        let* current_pc_excl = translate_index_to_address state.program_idx in
        translate_address_to_index (Int64.add (Int64.of_int imm_value) current_pc_excl)
      | LabelAddress excluding_directives_label_offset ->
        let* current_pc_excl = translate_index_to_address state.program_idx in
        translate_address_to_index
          (Int64.add excluding_directives_label_offset current_pc_excl)
    in
    let* () = set_register_value rd (Int64.add state.program_idx 4L) in
    set_program_idx (Int64.sub new_pc 4L)
  | Jalr (rd, rs1, imm) ->
    let* val_rs1 = get_register_value rs1 in
    let* address_info = get_address12_value imm in
    let* new_pc =
      match address_info with
      | Immediate imm_value ->
        translate_address_to_index (Int64.add val_rs1 (Int64.of_int imm_value))
      | LabelAddress excluding_directives_label_offset ->
        translate_address_to_index (Int64.add excluding_directives_label_offset val_rs1)
    in
    let* () = set_register_value rd (Int64.add state.program_idx 4L) in
    set_program_idx (Int64.sub new_pc 4L)
  | Jr rs1 ->
    let* val_rs1 = get_register_value rs1 in
    let* new_pc = translate_address_to_index val_rs1 in
    set_program_idx (Int64.sub new_pc 4L)
  | J imm_value -> execute_j imm_value
  | Lui (rd, imm) ->
    let* imm_value =
      match imm with
      | ImmediateAddress20 value -> return (Int64.shift_left (Int64.of_int value) 12)
      | LabelAddress20 label ->
        let* label_offset = resolve_label_address label in
        return (Int64.shift_left label_offset 12)
    in
    set_register_value rd imm_value
  | Auipc (rd, imm) ->
    let* imm_value =
      match imm with
      | ImmediateAddress20 value -> return (Int64.shift_left (Int64.of_int value) 12)
      | LabelAddress20 label ->
        let* label_offset = resolve_label_address label in
        return (Int64.shift_left label_offset 12)
    in
    let new_value = Int64.add state.program_idx imm_value in
    set_register_value rd new_value
  | Ecall -> handle_syscall
  | Mul (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.mul false
  | Div (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.div false
  | Rem (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.rem false
  | Mv (rd, rs) -> execute_immediate_op rd rs (ImmediateAddress12 0) Int64.add false
  | Li (rd, imm) ->
    let* imm_value =
      match imm with
      | ImmediateAddress32 value -> return (Int64.of_int value)
      | LabelAddress32 label -> resolve_label_address label
    in
    set_register_value rd imm_value
  | Addiw (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.add true
  | Mulw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.mul true
  | Addw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.add true
  | Subw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.sub true
  | Ret ->
    let* val_rs1 = get_register_value X1 in
    let* new_pc = translate_address_to_index val_rs1 in
    let* () = set_register_value X0 (Int64.add state.program_idx 1L) in
    set_program_idx new_pc
  | La (rd, imm) ->
    let* address_info = get_address32_value imm in
    let* new_address =
      match address_info with
      | Immediate imm_value ->
        let* current_pc_excl = translate_index_to_address state.program_idx in
        let* resolved_address =
          translate_address_to_index (Int64.add (Int64.of_int imm_value) current_pc_excl)
        in
        return resolved_address
      | LabelAddress excluding_directives_label_offset ->
        return excluding_directives_label_offset
    in
    set_register_value rd new_address
  | Adduw (rd, rs1, rs2) ->
    let* val1 = get_register_value rs1 in
    let* val2 = get_register_value rs2 in
    let result = Int64.add val1 val2 in
    let result_final = zext result in
    set_register_value rd result_final
  | Sh1add (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 1 false
  | Sh1adduw (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 1 true
  | Sh2add (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 2 false
  | Sh2adduw (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 2 true
  | Sh3add (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 3 false
  | Sh3adduw (rd, rs1, rs2) -> execute_shnadd rd rs1 rs2 3 true
  | Andn (rd, rs1, rs2) ->
    let* val1 = get_register_value rs1 in
    let* val2 = get_register_value rs2 in
    let result = Int64.logand val1 (Int64.lognot val2) in
    set_register_value rd result
  | Orn (rd, rs1, rs2) ->
    let* val1 = get_register_value rs1 in
    let* val2 = get_register_value rs2 in
    let result = Int64.logor val1 (Int64.lognot val2) in
    set_register_value rd result
  | Xnor (rd, rs1, rs2) ->
    let* val1 = get_register_value rs1 in
    let* val2 = get_register_value rs2 in
    let result = Int64.logxor val1 val2 in
    let result_final = Int64.lognot result in
    set_register_value rd result_final
  | Lwu (rd, rs1, imm) -> execute_load_int rd rs1 imm 4 false
  | Vle32v (vd, rs1, imm) -> execute_vle32v vd rs1 imm
  | Vse32v (vs, rs1, imm) -> execute_vstore vs rs1 imm
  | Vaddvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.add
  | Vaddvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.add
  | Vsubvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.sub
  | Vsubvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.sub
  | Vmulvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.mul
  | Vmulvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.mul
  | Vdivvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.div
  | Vdivvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.div
  | Vandvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logand
  | Vandvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.logand
  | Vorvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logor
  | Vorvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.logor
  | Vxorvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logxor
  | Vxorvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.logxor
  | Vminvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.min
  | Vminvx (vd, vs1, rs2) -> execute_vector_imm vd vs1 rs2 Int64.min
  | Vmseqvv (vd, vs1, vs2) ->
    execute_vector_arithmetic vd vs1 vs2 (fun x y -> if x = y then 1L else 0L)
  | Vmseqvx (vd, vs1, rs2) ->
    execute_vector_imm vd vs1 rs2 (fun x y -> if x = y then 1L else 0L)
  | _ -> return ()
;;

let nth_opt_int64 l n =
  (* we make steps by 4L because in our interpreter PC is built RISC-V-like and one expr is 4 bytes *)
  match n with
  | x when x < 0 -> fail "Index cannot be negative"
  | _ ->
    let rec nth_aux l n =
      match l with
      | [] -> return None
      | a :: l ->
        (match n with
         | 0L -> return (Some a)
         | _ -> nth_aux l (Int64.sub n 4L))
    in
    nth_aux l (Int64.of_int n)
;;

let traverse_program () =
  let rec prog_trav_helper () =
    let* state = read in
    let* expr_opt = nth_opt_int64 state.program (Int64.to_int state.program_idx) in
    match expr_opt with
    | None -> return state
    | Some (InstructionExpr instr) ->
      let* () = execute_instruction instr in
      let* () = increment_program_idx () in
      prog_trav_helper ()
    | Some (LabelExpr _) | Some (DirectiveExpr _) ->
      let* () = increment_program_idx () in
      prog_trav_helper ()
  in
  let* () = execute_j (LabelAddress20 "_start") in
  prog_trav_helper ()
;;

let interpret program =
  let initial_state = init_state program in
  run (traverse_program ()) initial_state
;;

let show_memory_int state =
  let memory_int_string =
    Int64Map.fold
      (fun address value acc -> acc ^ Printf.sprintf "%Ld: %Ld\n" address value)
      state.memory_int
      "Integer memory:\n"
  in
  memory_int_string
;;

let show_memory_str state =
  let memory_str_string =
    Int64Map.fold
      (fun address value acc -> acc ^ Printf.sprintf "%Ld: %s\n" address value)
      state.memory_str
      "String memory:\n"
  in
  memory_str_string
;;

let show_memory_writable state =
  let writable_str =
    Int64Map.fold
      (fun address writable acc -> acc ^ Printf.sprintf "%Ld: %b\n" address writable)
      state.memory_writable
      "Writable:\n"
  in
  writable_str
;;

let show_state state =
  let registers_order =
    [ "X0"
    ; "X1"
    ; "X2"
    ; "X3"
    ; "X4"
    ; "X5"
    ; "X6"
    ; "X7"
    ; "X8"
    ; "X9"
    ; "X10"
    ; "X11"
    ; "X12"
    ; "X13"
    ; "X14"
    ; "X15"
    ; "X16"
    ; "X17"
    ; "X18"
    ; "X19"
    ; "X20"
    ; "X21"
    ; "X22"
    ; "X23"
    ; "X24"
    ; "X25"
    ; "X26"
    ; "X27"
    ; "X28"
    ; "X29"
    ; "X30"
    ; "X31"
    ]
  in
  let registers_str =
    List.fold_left
      (fun acc reg ->
        let value = StringMap.find_opt reg state.registers |> Option.value ~default:0L in
        acc ^ Printf.sprintf "%s: %Ld\n" reg value)
      ""
      registers_order
  in
  let vector_registers_order =
    [ "V0"
    ; "V1"
    ; "V2"
    ; "V3"
    ; "V4"
    ; "V5"
    ; "V6"
    ; "V7"
    ; "V8"
    ; "V9"
    ; "V10"
    ; "V11"
    ; "V12"
    ; "V13"
    ; "V14"
    ; "V15"
    ; "V16"
    ; "V17"
    ; "V18"
    ; "V19"
    ; "V20"
    ; "V21"
    ; "V22"
    ; "V23"
    ; "V24"
    ; "V25"
    ; "V26"
    ; "V27"
    ; "V28"
    ; "V29"
    ; "V30"
    ; "V31"
    ]
  in
  let vector_registers_str =
    List.fold_left
      (fun acc vreg ->
        let values =
          StringMap.find_opt vreg state.vregisters |> Option.value ~default:[||]
        in
        let values_str =
          Array.fold_left (fun acc value -> acc ^ Printf.sprintf "%Ld " value) "" values
        in
        acc ^ Printf.sprintf "%s: [%s]\n" vreg values_str)
      ""
      vector_registers_order
  in
  let memory_int_string = show_memory_int state in
  let memory_str_string = show_memory_str state in
  let memory_writable_str = show_memory_writable state in
  let pc_str = Printf.sprintf "PC: %Ld" state.program_idx in
  let result = registers_str in
  let result = result ^ vector_registers_str in
  let result = result ^ memory_int_string in
  let result = result ^ memory_str_string in
  let result = result ^ memory_writable_str in
  let result = result ^ pc_str in
  result
;;

let%expect_test "test_factorial" =
  let program =
    [ LabelExpr "_start"
    ; InstructionExpr (Addi (X10, X0, ImmediateAddress12 5))
    ; InstructionExpr (Addi (X6, X0, ImmediateAddress12 1))
    ; LabelExpr "loop"
    ; InstructionExpr (Beqz (X10, LabelAddress12 "exit"))
    ; InstructionExpr (Mul (X6, X6, X10))
    ; InstructionExpr (Addi (X10, X10, ImmediateAddress12 (-1)))
    ; InstructionExpr (J (LabelAddress20 "loop"))
    ; LabelExpr "exit"
    ]
  in
  match interpret program with
  | Ok (_, final_state) ->
    let state_str = show_state final_state in
    print_string state_str;
    [%expect
      {|
      X0: 0
      X1: 0
      X2: 0
      X3: 0
      X4: 0
      X5: 0
      X6: 120
      X7: 0
      X8: 0
      X9: 0
      X10: 0
      X11: 0
      X12: 0
      X13: 0
      X14: 0
      X15: 0
      X16: 0
      X17: 0
      X18: 0
      X19: 0
      X20: 0
      X21: 0
      X22: 0
      X23: 0
      X24: 0
      X25: 0
      X26: 0
      X27: 0
      X28: 0
      X29: 0
      X30: 0
      X31: 0
      V0: [0 0 0 0 ]
      V1: [0 0 0 0 ]
      V2: [0 0 0 0 ]
      V3: [0 0 0 0 ]
      V4: [0 0 0 0 ]
      V5: [0 0 0 0 ]
      V6: [0 0 0 0 ]
      V7: [0 0 0 0 ]
      V8: [0 0 0 0 ]
      V9: [0 0 0 0 ]
      V10: [0 0 0 0 ]
      V11: [0 0 0 0 ]
      V12: [0 0 0 0 ]
      V13: [0 0 0 0 ]
      V14: [0 0 0 0 ]
      V15: [0 0 0 0 ]
      V16: [0 0 0 0 ]
      V17: [0 0 0 0 ]
      V18: [0 0 0 0 ]
      V19: [0 0 0 0 ]
      V20: [0 0 0 0 ]
      V21: [0 0 0 0 ]
      V22: [0 0 0 0 ]
      V23: [0 0 0 0 ]
      V24: [0 0 0 0 ]
      V25: [0 0 0 0 ]
      V26: [0 0 0 0 ]
      V27: [0 0 0 0 ]
      V28: [0 0 0 0 ]
      V29: [0 0 0 0 ]
      V30: [0 0 0 0 ]
      V31: [0 0 0 0 ]
      Integer memory:
      String memory:
      Writable:
      0: false
      1: false
      2: false
      3: false
      4: false
      5: false
      6: false
      7: false
      8: false
      9: false
      10: false
      11: false
      12: false
      13: false
      14: false
      15: false
      16: false
      17: false
      18: false
      19: false
      20: false
      21: false
      22: false
      23: false
      PC: 36
    |}]
  | Error e -> print_string ("Error: " ^ e)
;;

let%expect_test "test_vector_program_execution" =
  let program =
    [ LabelExpr "vector_data"
    ; DirectiveExpr (Word 1)
    ; DirectiveExpr (Word 2)
    ; DirectiveExpr (Word 3)
    ; DirectiveExpr (Word 4)
    ; DirectiveExpr (Word 5)
    ; LabelExpr "vector_a"
    ; DirectiveExpr (Space 16)
    ; LabelExpr "vector_b"
    ; DirectiveExpr (Space 16)
    ; LabelExpr "vector_sum"
    ; DirectiveExpr (Space 16)
    ; LabelExpr "_start"
    ; InstructionExpr (Addi (T0, X0, ImmediateAddress12 4))
    ; InstructionExpr (La (T2, LabelAddress32 "vector_data"))
    ; InstructionExpr (Vle32v (V0, T2, ImmediateAddress12 0))
    ; InstructionExpr (Addi (T3, T2, ImmediateAddress12 4))
    ; InstructionExpr (Vle32v (V1, T3, ImmediateAddress12 0))
    ; InstructionExpr (La (T4, LabelAddress32 "vector_a"))
    ; InstructionExpr (Vse32v (V0, T4, ImmediateAddress12 0))
    ; InstructionExpr (La (T5, LabelAddress32 "vector_b"))
    ; InstructionExpr (Vse32v (V1, T5, ImmediateAddress12 0))
    ; InstructionExpr (Vaddvv (V2, V0, V1))
    ; InstructionExpr (La (T4, LabelAddress32 "vector_sum"))
    ; InstructionExpr (Vse32v (V2, T4, ImmediateAddress12 0))
    ; InstructionExpr (Addi (X3, X0, ImmediateAddress12 10))
    ; InstructionExpr (Vaddvx (V3, V0, X3))
    ]
  in
  match interpret program with
  | Ok (_, final_state) ->
    let state_str = show_state final_state in
    print_string state_str;
    [%expect
      {|
      X0: 0
      X1: 0
      X2: 0
      X3: 10
      X4: 0
      X5: 4
      X6: 0
      X7: 0
      X8: 0
      X9: 0
      X10: 0
      X11: 0
      X12: 0
      X13: 0
      X14: 0
      X15: 0
      X16: 0
      X17: 0
      X18: 0
      X19: 0
      X20: 0
      X21: 0
      X22: 0
      X23: 0
      X24: 0
      X25: 0
      X26: 0
      X27: 0
      X28: 4
      X29: 52
      X30: 36
      X31: 0
      V0: [1 2 3 4 ]
      V1: [2 3 4 5 ]
      V2: [3 5 7 9 ]
      V3: [11 12 13 14 ]
      V4: [0 0 0 0 ]
      V5: [0 0 0 0 ]
      V6: [0 0 0 0 ]
      V7: [0 0 0 0 ]
      V8: [0 0 0 0 ]
      V9: [0 0 0 0 ]
      V10: [0 0 0 0 ]
      V11: [0 0 0 0 ]
      V12: [0 0 0 0 ]
      V13: [0 0 0 0 ]
      V14: [0 0 0 0 ]
      V15: [0 0 0 0 ]
      V16: [0 0 0 0 ]
      V17: [0 0 0 0 ]
      V18: [0 0 0 0 ]
      V19: [0 0 0 0 ]
      V20: [0 0 0 0 ]
      V21: [0 0 0 0 ]
      V22: [0 0 0 0 ]
      V23: [0 0 0 0 ]
      V24: [0 0 0 0 ]
      V25: [0 0 0 0 ]
      V26: [0 0 0 0 ]
      V27: [0 0 0 0 ]
      V28: [0 0 0 0 ]
      V29: [0 0 0 0 ]
      V30: [0 0 0 0 ]
      V31: [0 0 0 0 ]
      Integer memory:
      0: 1
      4: 2
      8: 3
      12: 4
      16: 5
      20: 1
      24: 2
      28: 3
      32: 4
      36: 2
      40: 3
      44: 4
      48: 5
      52: 3
      56: 5
      60: 7
      64: 9
      String memory:
      20:
      24:
      28:
      32:
      36:
      40:
      44:
      48:
      52:
      56:
      60:
      64:
      Writable:
      0: true
      1: false
      2: false
      3: false
      4: true
      5: false
      6: false
      7: false
      8: true
      9: false
      10: false
      11: false
      12: true
      13: false
      14: false
      15: false
      16: true
      17: false
      18: false
      19: false
      20: true
      21: false
      22: false
      23: false
      24: true
      25: false
      26: false
      27: false
      28: true
      29: false
      30: false
      31: false
      32: true
      33: false
      34: false
      35: false
      36: true
      37: false
      38: false
      39: false
      40: true
      41: false
      42: false
      43: false
      44: true
      45: false
      46: false
      47: false
      48: true
      49: false
      50: false
      51: false
      52: true
      53: false
      54: false
      55: false
      56: true
      57: false
      58: false
      59: false
      60: true
      61: false
      62: false
      63: false
      64: true
      65: false
      66: false
      67: false
      68: false
      69: false
      70: false
      71: false
      72: false
      73: false
      74: false
      75: false
      76: false
      77: false
      78: false
      79: false
      80: false
      81: false
      82: false
      83: false
      84: false
      85: false
      86: false
      87: false
      88: false
      89: false
      90: false
      91: false
      92: false
      93: false
      94: false
      95: false
      96: false
      97: false
      98: false
      99: false
      100: false
      101: false
      102: false
      103: false
      104: false
      105: false
      106: false
      107: false
      108: false
      109: false
      110: false
      111: false
      112: false
      113: false
      114: false
      115: false
      116: false
      117: false
      118: false
      119: false
      120: false
      121: false
      122: false
      123: false
      PC: 108
    |}]
  | Error e -> print_string ("Error: " ^ e)
;;
