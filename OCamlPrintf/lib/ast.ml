open QCheck.Gen

let coef = 10 (* For the generator's speed. *)
let min_range = int_range 0 10 (* For the generator's speed. *)
let gen_string gen = string_size min_range ~gen
let gen_list gen = list_size min_range gen

type 'a list_ = ('a list[@gen gen_list gen_a])
[@@deriving show { with_path = false }, qcheck]

let gen_char =
  (* Exception quotation marks and backslash. *)
  oneof [ return '!'; char_range '#' '&'; char_range '(' '['; char_range ']' '~' ]
;;

let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "function"
  | "match"
  | "rec"
  | "then"
  | "true"
  | "with"
  | "Some"
  | "None" -> true
  | _ -> false
;;

let gen_ident =
  let gen_var =
    map2
      (fun start_ident rest_ident ->
        match Base.Char.to_string start_ident ^ rest_ident with
        | "_" -> "id"
        | id -> id)
      (oneof [ char_range 'a' 'z'; return '_' ])
      (gen_string
         (oneof
            [ char_range '0' '9'
            ; char_range 'A' 'Z'
            ; char_range 'a' 'z'
            ; return '_'
            ; return '\''
            ]))
  in
  gen_var >>= fun name -> if is_keyword name then gen_var else return name
;;

type ident = (string[@gen gen_ident]) [@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show { with_path = false }, qcheck]

type constant =
  | Const_integer of (int[@gen nat])
  | Const_char of (char[@gen gen_char])
  | Const_string of (string[@gen gen_string gen_char])
[@@deriving show { with_path = false }, qcheck]

let gen_type_name =
  map3
    (fun first_char second_char string ->
      Printf.sprintf "%c%c%s" first_char second_char string)
    (oneof [ char_range 'a' 'z' ])
    (oneof [ char_range '0' '9'; char_range 'A' 'Z'; char_range 'a' 'z'; return '_' ])
    (gen_string
       (oneof
          [ char_range '0' '9'
          ; char_range 'A' 'Z'
          ; char_range 'a' 'z'
          ; return '_'
          ; return '\''
          ]))
;;

type core_type =
  | Type_any
  | Type_unit
  | Type_char
  | Type_int
  | Type_string
  | Type_bool
  | Type_option of (core_type[@gen gen_core_type_sized (n / coef)])
  | Type_name of (ident[@gen gen_type_name])
  | Type_list of (core_type[@gen gen_core_type_sized (n / coef)])
  | Type_tuple of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)]) list_
  | Type_arrow of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
[@@deriving show { with_path = false }, qcheck]

let gen_construct gen n tuple construct =
  oneof
    [ (let rec gen_list n =
         if n = 0
         then return ("[]", None)
         else (
           let element = gen 0 in
           let tail = gen_list (n / coef) in
           map2 (fun e t -> "::", Some (tuple (e, construct t, []))) element tail)
       in
       gen_list n)
    ; return ("true", None)
    ; return ("false", None)
    ; map (fun i -> "Some", Some i) (gen (n / coef))
    ; return ("None", None)
    ; return ("()", None)
    ]
;;

type pattern =
  | Pat_any
  | Pat_var of ident
  | Pat_constant of constant
  | Pat_tuple of
      (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern[@gen gen_pattern_sized (n / coef)]) list_
  | Pat_construct of
      ((ident * pattern option)
      [@gen
        gen_construct
          gen_pattern_sized
          n
          (fun (first_pat, second_pat, pat_list) ->
            Pat_tuple (first_pat, second_pat, pat_list))
          (fun (id, pat_option) -> Pat_construct (id, pat_option))])
  | Pat_constraint of (pattern[@gen gen_pattern_sized (n / coef)]) * core_type
[@@deriving show { with_path = false }, qcheck]

type 'exp value_binding =
  { pat : pattern
  ; exp : 'exp
  }
[@@deriving show { with_path = false }, qcheck]

type 'exp case =
  { left : pattern
  ; right : 'exp
  }
[@@deriving show { with_path = false }, qcheck]

module Expression = struct
  let gen_value_binding gen n fix_exp_fun =
    oneof
      [ map2 (fun id exp -> { pat = Pat_var id; exp }) gen_ident (gen (n / coef))
      ; map3
          (fun id type' exp -> { pat = Pat_constraint (Pat_var id, type'); exp })
          gen_ident
          gen_core_type
          (gen (n / coef))
      ; map2 (fun pat exp -> { pat; exp = fix_exp_fun exp }) gen_pattern (gen (n / coef))
      ]
  ;;

  let gen_exp_apply gen n exp_ident exp_apply =
    oneof
      [ map2 (fun exp first_exp -> exp, first_exp) (gen 0) (gen (n / coef))
      ; map (fun exp -> exp_ident "~-", exp) (gen (n / coef))
      ; map3
          (fun opr opn1 opn2 -> opr, exp_apply (opn1, opn2))
          (oneofl
             [ exp_ident "*"
             ; exp_ident "/"
             ; exp_ident "+"
             ; exp_ident "-"
             ; exp_ident ">="
             ; exp_ident "<="
             ; exp_ident "<>"
             ; exp_ident "="
             ; exp_ident ">"
             ; exp_ident "<"
             ; exp_ident "&&"
             ; exp_ident "||"
             ])
          (gen (n / coef))
          (gen (n / coef))
      ]
  ;;

  type value_binding_exp =
    (t value_binding
    [@gen
      gen_value_binding
        gen_sized
        n
        (let rec fix_exp_fun = function
           | Exp_fun (_, _, exp) -> fix_exp_fun exp
           | Exp_function ({ left = _; right = exp }, _) -> fix_exp_fun exp
           | Exp_constraint (exp, type') -> Exp_constraint (fix_exp_fun exp, type')
           | exp -> exp
         in
         fix_exp_fun)])

  and case_exp =
    (t case
    [@gen map2 (fun left right -> { left; right }) gen_pattern (gen_sized (n / coef))])

  and t =
    | Exp_ident of ident
    | Exp_constant of constant
    | Exp_let of
        rec_flag
        * value_binding_exp
        * value_binding_exp list_
        * (t[@gen gen_sized (n / coef)])
    | Exp_fun of pattern * pattern list_ * (t[@gen gen_sized (n / coef)])
    | Exp_apply of
        ((t * t)
        [@gen
          gen_exp_apply
            gen_sized
            n
            (fun id -> Exp_ident id)
            (fun (opn1, opn2) -> Exp_apply (opn1, opn2))])
    | Exp_function of case_exp * case_exp list_
    | Exp_match of (t[@gen gen_sized (n / coef)]) * case_exp * case_exp list_
    | Exp_tuple of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) list_
    | Exp_construct of
        ((ident * t option)
        [@gen
          gen_construct
            gen_sized
            n
            (fun (first_exp, second_exp, exp_list) ->
              Exp_tuple (first_exp, second_exp, exp_list))
            (fun (id, exp_option) -> Exp_construct (id, exp_option))])
    | Exp_ifthenelse of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) option
    | Exp_sequence of (t[@gen gen_sized (n / coef)]) * (t[@gen gen_sized (n / coef)])
    | Exp_constraint of (t[@gen gen_sized (n / coef)]) * core_type
  [@@deriving show { with_path = false }, qcheck]
end

type structure_item =
  | Struct_eval of Expression.t
  | Struct_value of
      rec_flag * Expression.value_binding_exp * Expression.value_binding_exp list_
[@@deriving show { with_path = false }, qcheck]

type structure = structure_item list_ [@@deriving show { with_path = false }, qcheck]
