(** Copyright 2024-2025, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap = Map.Make (String)
module Int64Map = Map.Make (Int64)

type state =
  { program : ast
  ; registers : int64 StringMap.t
  ; vregisters : int64 list StringMap.t
  ; max_vector_length : int
  ; vector_element_length : int
  ; vector_length : int
  ; memory : char Int64Map.t
  ; program_idx : int64
  }

module type CombinedMonadType = sig
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

module CombinedMonad : CombinedMonadType = struct
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
  | Immediate of int64 (** Immediate value *)
  | LabelAddress of int64 (** Label (resolved in terms of addresses) *)

let get_address12_value = function
  | ImmediateAddress12 value -> return (Immediate (Int64.of_int value))
  | LabelAddress12 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

let get_address20_value = function
  | ImmediateAddress20 value -> return (Immediate (Int64.of_int value))
  | LabelAddress20 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

let get_address32_value = function
  | ImmediateAddress32 value -> return (Immediate (Int64.of_int value))
  | LabelAddress32 label ->
    let* label_address = resolve_label_address label in
    return (LabelAddress label_address)
;;

let put_bytes_to_memory get_byte size addr memory =
  let rec put_bytes memory current_addr =
    let n = Int64.to_int (Int64.sub current_addr addr) in
    if n < size
    then (
      let byte = get_byte n in
      let new_memory = Int64Map.add current_addr byte memory in
      put_bytes new_memory (Int64.succ current_addr))
    else memory
  in
  put_bytes memory addr
;;

let put_word_to_memory word =
  let get_word_byte n =
    Char.chr (Int32.to_int (Int32.logand (Int32.shift_right word (n * 8)) 0xFFl))
  in
  put_bytes_to_memory get_word_byte 4
;;

let put_string_to_memory str =
  let get_string_byte = String.get str in
  put_bytes_to_memory get_string_byte (String.length str)
;;

(* used to initialize memory from directives *)
let init_data program =
  let rec traverse_program program addr memory =
    match program with
    | [] -> memory
    | DirectiveExpr (Word word) :: rest ->
      let memory = put_word_to_memory word addr memory in
      traverse_program rest (Int64.add addr 4L) memory
    | DirectiveExpr (Space amount) :: rest ->
      (* skip amount of bytes used by space *)
      traverse_program rest (Int64.add addr (Int64.of_int amount)) memory
    | DirectiveExpr (StringDir str) :: rest ->
      let memory = put_string_to_memory str addr memory in
      traverse_program rest (Int64.add addr (Int64.of_int (String.length str))) memory
    | _ :: rest -> traverse_program rest addr memory
  in
  traverse_program program 0L Int64Map.empty
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
      StringMap.add (show_vector_register reg) (List.init vector_length (fun _ -> 0L)) acc)
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
  let memory = init_data program in
  { program
  ; registers
  ; vregisters
  ; max_vector_length
  ; vector_element_length
  ; vector_length
  ; memory
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
     |> Option.value ~default:(List.init state.vector_length (fun _ -> 0L)))
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

(* index is expr index in AST (= expr list) *)
(* address is a kind of expression's physical address (but all instructions are considered 4-byte, even compressed (for convenience)) *)

(* translates expr index in AST to its "physical address" *)
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

(* translates expr "physical address" to its index in ast *)
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

let sext x = Int64.shift_right (Int64.shift_left (Int64.logand x 0xFFFFFFFFL) 32) 32

let zext x =
  Int64.shift_right_logical (Int64.shift_left (Int64.logand x 0xFFFFFFFFL) 32) 32
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
        translate_address_to_index (Int64.add imm_value (sext current_address))
      in
      return (Int64.sub target_index 4L)
    | LabelAddress label_address when comparison_fn val_rs1 val_rs2 ->
      let* target_index = translate_address_to_index label_address in
      return (Int64.sub target_index 4L)
    | _ -> return state.program_idx
  in
  set_program_idx new_program_idx
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
    | Immediate imm_value -> imm_value
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
    | Immediate imm_value -> imm_value
    | LabelAddress label_address -> label_address
  in
  let shamt = Int64.to_int (Int64.logand shamt 0x3FL) in
  let result = op val1 shamt in
  set_register_value rd result
;;

let execute_shnadd rd rs1 rs2 n to_zext =
  let* val1 = get_register_value rs1 in
  let* val2 = get_register_value rs2 in
  let arg1 = if to_zext then zext val1 else val1 in
  let result = Int64.add val2 (Int64.shift_left arg1 n) in
  set_register_value rd result
;;

let load_byte_from_memory address =
  let* state = read in
  let byte = Int64Map.find_opt address state.memory in
  match byte with
  | None -> return '\000'
  | Some byte -> return byte
;;

let load_from_memory address size =
  let rec read_bytes acc offset =
    if offset < size
    then (
      let byte_addr = Int64.add address (Int64.of_int offset) in
      let* byte = load_byte_from_memory byte_addr in
      let byte_value = Int64.of_int (Char.code byte) in
      let shifted_byte = Int64.shift_left byte_value (offset * 8) in
      read_bytes (Int64.logor acc shifted_byte) (offset + 1))
    else return acc
  in
  match size with
  | 1 | 2 | 4 | 8 -> read_bytes 0L 0
  | _ -> fail "Unsupported load size"
;;

let store_in_memory address value size =
  let get_int64_byte n =
    Char.chr (Int64.to_int (Int64.logand (Int64.shift_right value (n * 8)) 0xFFL))
  in
  let* state = read in
  let new_memory = put_bytes_to_memory get_int64_byte size address state.memory in
  let new_state = { state with memory = new_memory } in
  write new_state
;;

let execute_load_int rd rs1 imm size is_signed =
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address (sext offset) in
  let* value = load_from_memory address size in
  let result = if is_signed then sext value else zext value in
  set_register_value rd result
;;

let execute_store_int rs1 rs2 imm size =
  let* base_address = get_register_value rs2 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address (sext offset) in
  let* value = get_register_value rs1 in
  store_in_memory address value size
;;

let execute_vle32v vd rs1 imm =
  let* state = read in
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address offset in
  let vector_length = state.vector_length in
  let rec load_values address element_idx acc =
    if element_idx < vector_length
    then
      let* value = load_from_memory address state.vector_element_length in
      let next_address = Int64.add address (Int64.of_int state.vector_element_length) in
      load_values next_address (element_idx + 1) (value :: acc)
    else return (List.rev acc)
  in
  let* vector_values = load_values address 0 [] in
  set_vregister_value vd vector_values
;;

let execute_vse32v vs rs1 imm =
  let* base_address = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let offset =
    match address_info with
    | Immediate imm_value -> imm_value
    | LabelAddress label_address -> label_address
  in
  let address = Int64.add base_address offset in
  let* vector_value = get_vregister_value vs in
  let rec store_values element_idx addr =
    let* state = read in
    if element_idx < state.vector_length
    then (
      let element = List.nth vector_value element_idx in
      let* () = store_in_memory addr element state.vector_element_length in
      store_values (element_idx + 1) (Int64.add addr 4L))
    else return ()
  in
  store_values 0 address
;;

let execute_vector_arithmetic vd vs1 vs2 op =
  let* vec1 = get_vregister_value vs1 in
  let* vec2 = get_vregister_value vs2 in
  let result = List.map2 op vec1 vec2 in
  set_vregister_value vd result
;;

let execute_vector_scalar vd vs1 rs2 op =
  let* vec = get_vregister_value vs1 in
  let* scalar = get_register_value rs2 in
  let result = List.map (fun x -> op x scalar) vec in
  set_vregister_value vd result
;;

let load_string_from_memory address size =
  let rec load_chars acc offset =
    if offset < size
    then (
      let char_addr = Int64.add address (Int64.of_int offset) in
      let* char = load_byte_from_memory char_addr in
      match char with
      | '\000' -> return acc
      | _ -> load_chars (acc ^ String.make 1 char) (offset + 1))
    else return acc
  in
  load_chars "" 0
;;

let handle_syscall =
  let* state = read in
  let* syscall_number = get_register_value A7 in
  let* arg1 = get_register_value A0 in
  let* arg2 = get_register_value A1 in
  let* arg3 = get_register_value A2 in
  match syscall_number with
  | 93L ->
    (* perform exit *)
    let exit_code = Int64.to_int arg1 in
    exit exit_code
  | 64L ->
    (* perform write - take information from memory and put in fd (only stdout supported) *)
    let fd = Int64.to_int arg1 in
    let address = arg2 in
    let size = arg3 in
    (match fd with
     | 1 ->
       let* output = load_string_from_memory address (Int64.to_int size) in
       return (Printf.printf "%s\n" output)
     | _ -> fail "Unsupported file descriptor for write syscall")
  | 63L ->
    (* perform read - read line and put input string into memory *)
    let fd = Int64.to_int arg1 in
    let buf = arg2 in
    (match fd with
     | 0 ->
       let str = read_line () in
       let new_memory = put_string_to_memory str buf state.memory in
       let new_state = { state with memory = new_memory } in
       let* () = write new_state in
       set_register_value A0 (Int64.of_int (String.length str))
     | _ -> fail "Unsupported file descriptor for read syscall")
  | _ -> fail "Unsupported syscall"
;;

let execute_j imm_value =
  let* state = read in
  let* address_info = get_address20_value imm_value in
  let* new_program_idx =
    match address_info with
    | Immediate imm_value ->
      let* current_address = translate_index_to_address state.program_idx in
      let* target_index =
        translate_address_to_index (Int64.add (sext imm_value) current_address)
      in
      return target_index
    | LabelAddress label_address ->
      let* target_index = translate_address_to_index label_address in
      return target_index
  in
  set_program_idx (Int64.sub new_program_idx 4L)
;;

let execute_jalr rd rs1 imm =
  let* state = read in
  let* val_rs1 = get_register_value rs1 in
  let* address_info = get_address12_value imm in
  let* target_idx =
    match address_info with
    | Immediate imm_value -> translate_address_to_index (Int64.add val_rs1 imm_value)
    | LabelAddress label_address -> translate_address_to_index label_address
  in
  let* () = set_register_value rd (Int64.add state.program_idx 4L) in
  (* last bit of result is set 0 according to specification *)
  let new_program_idx = Int64.logand (Int64.sub target_idx 4L) (Int64.neg 1L) in
  set_program_idx new_program_idx
;;

let get_upper_immediate = function
  | ImmediateAddress20 value -> return (Int64.shift_left (Int64.of_int value) 12)
  | LabelAddress20 label ->
    let* label_address = resolve_label_address label in
    return (sext (Int64.shift_left label_address 12))
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
    let* target_idx =
      match address_info with
      | Immediate imm_value ->
        let* current_address = translate_index_to_address state.program_idx in
        translate_address_to_index (Int64.add imm_value (sext current_address))
      | LabelAddress label_address -> translate_address_to_index label_address
    in
    let* () = set_register_value rd (Int64.add state.program_idx 4L) in
    set_program_idx (Int64.sub target_idx 4L)
  | Jalr (rd, rs1, imm) -> execute_jalr rd rs1 imm
  | Jr rs1 ->
    let* val_rs1 = get_register_value rs1 in
    let* target_idx = translate_address_to_index val_rs1 in
    set_program_idx (Int64.sub target_idx 4L)
  | J imm_value -> execute_j imm_value
  | Lui (rd, imm) ->
    let* imm_value = get_upper_immediate imm in
    set_register_value rd imm_value
  | Auipc (rd, imm) ->
    let* imm_value = get_upper_immediate imm in
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
    set_register_value rd (sext imm_value)
  | Addiw (rd, rs1, imm) -> execute_immediate_op rd rs1 imm Int64.add true
  | Mulw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.mul true
  | Addw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.add true
  | Subw (rd, rs1, rs2) -> execute_arithmetic_op rd rs1 rs2 Int64.sub true
  | Ret -> execute_jalr X0 X1 (ImmediateAddress12 0)
  | La (rd, imm) ->
    let* address_info = get_address32_value imm in
    let* new_address =
      match address_info with
      | Immediate imm_value ->
        let* current_address = translate_index_to_address state.program_idx in
        let* resolved_address =
          translate_address_to_index (Int64.add imm_value current_address)
        in
        return resolved_address
      | LabelAddress label_address -> return label_address
    in
    set_register_value rd new_address
  | Adduw (rd, rs1, rs2) ->
    let* val1 = get_register_value rs1 in
    let* val2 = get_register_value rs2 in
    let result = Int64.add val1 val2 in
    let result = zext result in
    set_register_value rd result
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
  | Vse32v (vs, rs1, imm) -> execute_vse32v vs rs1 imm
  | Vaddvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.add
  | Vaddvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.add
  | Vsubvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.sub
  | Vsubvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.sub
  | Vmulvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.mul
  | Vmulvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.mul
  | Vdivvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.div
  | Vdivvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.div
  | Vandvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logand
  | Vandvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.logand
  | Vorvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logor
  | Vorvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.logor
  | Vxorvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.logxor
  | Vxorvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.logxor
  | Vminvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.min
  | Vminvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.min
  | Vmaxvv (vd, vs1, vs2) -> execute_vector_arithmetic vd vs1 vs2 Int64.max
  | Vmaxvx (vd, vs1, rs2) -> execute_vector_scalar vd vs1 rs2 Int64.max
  | Vmseqvv (vd, vs1, vs2) ->
    execute_vector_arithmetic vd vs1 vs2 (fun x y -> if x = y then 1L else 0L)
  | Vmseqvx (vd, vs1, rs2) ->
    execute_vector_scalar vd vs1 rs2 (fun x y -> if x = y then 1L else 0L)
  | Vsetvli _ -> return ()
  | Vredsumvs (vd, vs1, vs2) ->
    let* vec1 = get_vregister_value vs1 in
    let* vec2 = get_vregister_value vs2 in
    let* vecd = get_vregister_value vd in
    let vec1_first = List.hd vec1 in
    let vec2_sum = List.fold_left Int64.add 0L vec2 in
    let result_first = Int64.add vec2_sum vec1_first in
    let result_rest = List.tl vecd in
    let result = result_first :: result_rest in
    set_vregister_value vd result
  | _ -> fail "Unsupported instruction"
;;

let nth_opt_int64 l n =
  (* similar to List.nth_opt function, but we make steps of 4L *)
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

let show_memory state =
  let memory_string =
    Int64Map.fold
      (fun address value acc ->
        acc ^ Printf.sprintf "%Ld: %d\n" address (int_of_char value))
      state.memory
      "Memory:\n"
  in
  memory_string
;;

let show_state state =
  let registers_str =
    let rec loop acc i =
      if i > 31
      then acc
      else (
        let reg = "X" ^ string_of_int i in
        let value = StringMap.find_opt reg state.registers |> Option.value ~default:0L in
        loop (acc ^ Printf.sprintf "%s: %Ld\n" reg value) (i + 1))
    in
    loop "" 0
  in
  let vector_registers_str =
    let rec loop acc i =
      if i > 31
      then acc
      else (
        let vreg = "V" ^ string_of_int i in
        let values =
          StringMap.find_opt vreg state.vregisters |> Option.value ~default:[]
        in
        let values_str =
          List.fold_left (fun acc value -> acc ^ Printf.sprintf "%Ld " value) "" values
        in
        loop (acc ^ Printf.sprintf "%s: [%s]\n" vreg values_str) (i + 1))
    in
    loop "" 0
  in
  let memory_str = show_memory state in
  let program_idx_str = Printf.sprintf "Program index: %Ld" state.program_idx in
  let result = registers_str in
  let result = result ^ vector_registers_str in
  let result = result ^ memory_str in
  let result = result ^ program_idx_str in
  result
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

let%expect_test "test_arithmetic" =
  let program =
    [ LabelExpr "_start"
    ; InstructionExpr (Li (A0, ImmediateAddress32 0b1010))
    ; InstructionExpr (Li (A1, ImmediateAddress32 0b1100))
    ; InstructionExpr (Sll (A2, A0, A1))
    ; InstructionExpr (Srl (A3, A2, A1))
    ; InstructionExpr (Sra (A4, A2, A1))
    ; InstructionExpr (Slli (A5, A0, ImmediateAddress12 2))
    ; InstructionExpr (Srli (A6, A5, ImmediateAddress12 2))
    ; InstructionExpr (Srai (A7, A5, ImmediateAddress12 2))
    ; InstructionExpr (Sub (S2, A1, A0))
    ; InstructionExpr (Xor (S3, A0, A1))
    ; InstructionExpr (Or (S4, A0, A1))
    ; InstructionExpr (And (S5, A0, A1))
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
      X6: 0
      X7: 0
      X8: 0
      X9: 0
      X10: 10
      X11: 12
      X12: 40960
      X13: 10
      X14: 10
      X15: 40
      X16: 10
      X17: 10
      X18: 2
      X19: 6
      X20: 14
      X21: 8
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
      Memory:
      Program index: 52
    |}]
  | Error e -> print_string ("Error: " ^ e)
;;

let%expect_test "test_jalr" =
  let program =
    [ LabelExpr "_start"
    ; InstructionExpr (Li (Ra, ImmediateAddress32 4))
    ; InstructionExpr (Jalr (Sp, X1, ImmediateAddress12 8))
    ; InstructionExpr (Li (Gp, ImmediateAddress32 10))
    ; LabelExpr "target"
    ; InstructionExpr (Li (Tp, ImmediateAddress32 20))
    ]
  in
  match interpret program with
  | Ok (_, final_state) ->
    let state_str = show_state final_state in
    print_string state_str;
    [%expect
      {|
        X0: 0
        X1: 4
        X2: 12
        X3: 0
        X4: 20
        X5: 0
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
        Memory:
        Program index: 24
      |}]
  | Error e -> print_string ("Error: " ^ e)
;;

let%expect_test "test_jal" =
  let program =
    [ LabelExpr "_start"
    ; InstructionExpr (Jal (T0, LabelAddress20 "target"))
    ; InstructionExpr (Li (T1, ImmediateAddress32 10))
    ; LabelExpr "target"
    ; InstructionExpr (Li (T2, ImmediateAddress32 20))
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
        X5: 8
        X6: 0
        X7: 20
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
        Memory:
        Program index: 20
      |}]
  | Error e -> print_string ("Error: " ^ e)
;;

let%expect_test "test_j_immediate" =
  let program =
    [ DirectiveExpr (Word 1l)
    ; DirectiveExpr (StringDir "abcd")
    ; DirectiveExpr (Space 4)
    ; LabelExpr "_start"
    ; InstructionExpr (J (ImmediateAddress20 8))
    ; InstructionExpr (Li (S0, ImmediateAddress32 10))
    ; InstructionExpr (Li (S1, ImmediateAddress32 20))
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
        X6: 0
        X7: 0
        X8: 0
        X9: 20
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
        Memory:
        0: 1
        1: 0
        2: 0
        3: 0
        4: 97
        5: 98
        6: 99
        7: 100
        Program index: 28
      |}]
  | Error e -> print_string ("Error: " ^ e)
;;
