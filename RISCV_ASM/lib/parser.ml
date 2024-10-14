open Angstrom
open Ast
let is_not_colon = function
    | ':' -> false
    | _ -> true
let is_digit = function
    | '1'..'9' -> true
    | _ -> false
let register = choice [
    string "x0" *> return X0;
    string "x1" *> return X1;
    string "x2" *> return X2;
    string "x3" *> return X3;
    string "x4" *> return X4;
    string "x5" *> return X5;
    string "x6" *> return X6;
    string "x7" *> return X7;
    string "x8" *> return X8;
    string "x9" *> return X9;
    string "x10" *> return X10;
    string "x11" *> return X11;
    string "x12" *> return X12;
    string "x13" *> return X13;
    string "x14" *> return X14;
    string "x15" *> return X15;
    string "x16" *> return X16;
    string "x17" *> return X17;
    string "x18" *> return X18;
    string "x19" *> return X19;
    string "x20" *> return X20;
    string "x21" *> return X21;
    string "x22" *> return X22;
    string "x23" *> return X23;
    string "x24" *> return X24;
    string "x25" *> return X25;
    string "x26" *> return X26;
    string "x27" *> return X27;
    string "x28" *> return X28;
    string "x29" *> return X29;
    string "x30" *> return X30;
    string "x31" *> return X31;
    string "zero" *> return X0;
    string "ra" *> return X1;
    string "sp" *> return X2;
    string "gp" *> return X3;
    string "tp" *> return X4;
    string "t0" *> return X5;
    string "t1" *> return X6;
    string "s0" *> return X7;
    string "fp" *> return X7;
    string "s1" *> return X8;
    string "a0" *> return X9;
    string "a1" *> return X10;
    string "a2" *> return X11;
    string "a3" *> return X12;
    string "a4" *> return X13;
    string "a5" *> return X14;
    string "a6" *> return X15;
    string "a7" *> return X16;
    string "s2" *> return X17;
    string "s3" *> return X18;
    string "s4" *> return X19;
    string "s5" *> return X20;
    string "s6" *> return X21;
    string "s7" *> return X22;
    string "s8" *> return X23;
    string "s9" *> return X24;
    string "s10" *> return X25;
    string "s11" *> return X26;
    string "t3" *> return X27;
    string "t4" *> return X28;
    string "t5" *> return X29;
    string "t6" *> return X30;
    string "t7" *> return X31;
]
let immediate12 = lift(fun n: int -> return (Immediate12(n))) (take_while1 is_digit)
let instruction = choice [
    string "add" *> lift3 (fun r1 r2 r3 -> return (Add(r1, r2, r3))) register register register;
    string "mv" *> lift2 (fun r1 r2 -> return (Mv(r1, r2))) register register;
    string "beq" *> lift3 (fun r1 r2 adr -> return (Beq(r1, r2))) register register int_of_string;
]
let label = lift (fun str -> return (Label(str))) (take_while1 is_not_colon)
let expr = choice
            [instruction;
            label]
let ast = many expr

