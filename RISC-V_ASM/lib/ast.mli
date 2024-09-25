(** integer registers *)
type int_register = 
    | X0 (** zero - zero constant *)
    | X1 (** ra - return address *)
    | X2 (** sp - stack pointer *)
    | X3 (** gp - global pointer *)
    | X4 (** tp - thread pointer *)
    | X5 (** t0 - temporary *)
    | X6 (** t1 - temporary *)
    | X7 (** t2 - temporary *)
    | X8 (** s0/fp - saved/frame pointer *)
    | X9 (** s1 - saved register *)
    | X10 (** a0 - function argument or return value *)
    | X11 (** a1 - function argument or return value *)
    | X12 (** a2 - function argument *)
    | X13 (** a3 - function arg0ter *)
    | X24 (** s8 - saved register *)
    | X25 (** s9 - saved register *)
    | X26 (** s10 - saved register *)
    | X27 (** s11 - saved register *)
    | X28 (** t3 - temporary *)
    | X29 (** t4 - temporary *)
    | X30 (** t5 - temporary *)
    | X31 (** t6 - temporary *)

(** floating point registers *)
type float_register = 
    | F0 (** ft0 - temporary *)
    | F1 (** ft1 - temporary *)
    | F2 (** ft2 - temporary *)
    | F3 (** ft3 - temporary *)
    | F4 (** ft4 - temporary *)
    | F5 (** ft5 - temporary *)
    | F6 (** ft6 - temporary *)
    | F7 (** ft7 - temporary *)
    | F8 (** fs0 - saved register *)
    | F9 (** fs1 - saved register *)
    | F10 (** fa0 - function argument or return value *)
    | F11 (** fa1 - function argument or return value *)
    | F12 (** fa2 - function argument *)
    | F13 (** fa3 - function argument *)
    | F14 (** fa4 - function argument *)
    | F15 (** fa5 - function argument *)
    | F16 (** fa6 - function argument *)
    | F17 (** fa7 - function argument *)
    | F18 (** fs2 - saved register *)
    | F19 (** fs3 - saved register *)
    | F20 (** fs4 - saved register *)
    | F21 (** fs5 - saved register *)
    | F22 (** fs6 - saved register *)
    | F23 (** fs7 - saved register *)
    | F24 (** fs8 - saved register *)
    | F25 (** fs9 - saved register *)
    | F26 (** fs10 - saved register *)
    | F27 (** fs11 - saved register *)
    | F28 (** ft8 - temporary *)
    | F29 (** ft9 - temporary *)
    | F30 (** ft10 - temporary *)
    | F31 (** ft11 - temporary *)


type immediate

type int_instruction =
    | Add of int_register * int_register
    | Sub of int_register * int_register
    | Xor of int_register * int_register
    | Or of  int_register * int_register
    | And of int_register * int_register
    | Sll of int_register * int_register

type label

type directive = 
    | Section
    | Global
