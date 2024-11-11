open Ast
open Format

let print_bin_op indent fmt = function
  | Binary_equal -> fprintf fmt "%s| Binary Equal\n" (String.make indent '-')
  | Binary_unequal -> fprintf fmt "%s| Binary Unequal\n" (String.make indent '-')
  | Binary_less -> fprintf fmt "%s| Binary Less\n" (String.make indent '-')
  | Binary_less_or_equal ->
    fprintf fmt "%s| Binary Less Or Equal\n" (String.make indent '-')
  | Binary_greater -> fprintf fmt "%s| Binary Greater\n" (String.make indent '-')
  | Binary_greater_or_equal ->
    fprintf fmt "%s| Binary Greater Or Equal\n" (String.make indent '-')
  | Binary_add -> fprintf fmt "%s| Binary Add\n" (String.make indent '-')
  | Binary_subtract -> fprintf fmt "%s| Binary Subtract\n" (String.make indent '-')
  | Binary_multiply -> fprintf fmt "%s| Binary Multiply\n" (String.make indent '-')
  | Logical_or -> fprintf fmt "%s| Logical Or\n" (String.make indent '-')
  | Logical_and -> fprintf fmt "%s| Logical And\n" (String.make indent '-')
  | Binary_divide -> fprintf fmt "%s| Binary Divide\n" (String.make indent '-')
  | Binary_or_bitwise -> fprintf fmt "%s| Binary Or Bitwise\n" (String.make indent '-')
  | Binary_xor_bitwise -> fprintf fmt "%s| Binary Xor Bitwise\n" (String.make indent '-')
  | Binary_and_bitwise -> fprintf fmt "%s| Binary And Bitwise\n" (String.make indent '-')
;;

let rec print_pattern indent fmt = function
  | Wild -> fprintf fmt "%s| Wild\n" (String.make indent '-')
  | PCons (head, tail) ->
    fprintf fmt "%s| PCons:\n" (String.make indent '-');
    fprintf fmt "%sHead:\n" (String.make (indent + 2) '-');
    print_pattern (indent + 4) fmt head;
    fprintf fmt "%sTail:\n" (String.make (indent + 2) '-');
    print_pattern (indent + 4) fmt tail
  | PTuple tuple ->
    fprintf fmt "%s| PTuple:\n" (String.make indent '-');
    List.iter (print_pattern (indent + 2) fmt) tuple
  | PConst literal ->
    fprintf fmt "%s| PConst:\n" (String.make indent '-');
    (match literal with
     | Int_lt i -> fprintf fmt "%sInt: %d\n" (String.make (indent + 2) '-') i
     | Bool_lt b -> fprintf fmt "%sBool: %b\n" (String.make (indent + 2) '-') b
     | String_lt s -> fprintf fmt "%sString: %S\n" (String.make (indent + 2) '-') s
     | Unit_lt -> fprintf fmt "%sUnit\n" (String.make (indent + 2) '-'))
  | PVar (Ident name) -> fprintf fmt "%s| PVar(%s)\n" (String.make indent '-') name
  | Variant variants ->
    fprintf fmt "%s| Variant:\n" (String.make indent '-');
    List.iter
      (fun (Ident v) -> fprintf fmt "%s- %s\n" (String.make (indent + 2) '-') v)
      variants
;;

let print_unary_op indent fmt = function
  | Unary_minus -> fprintf fmt "%s| Unary minus\n" (String.make indent '-')
  | Unary_not -> fprintf fmt "%s| Unary negative\n" (String.make indent '-')
;;

let rec print_expr indent fmt expr =
  match expr with
  | Const (Int_lt i) -> fprintf fmt "%s| Const(Int: %d)\n" (String.make indent '-') i
  | Const (Bool_lt b) -> fprintf fmt "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) ->
    fprintf fmt "%s| Const(String: %S)\n" (String.make indent '-') s
  | Const Unit_lt -> fprintf fmt "%s| Const(Unit)\n" (String.make indent '-')
  | List_expr (expr1, expr2) ->
    fprintf fmt "%s| List expr:\n" (String.make indent '-');
    print_expr (indent + 2) fmt expr1;
    print_expr (indent + 2) fmt expr2
  | Tuple t -> List.iter (print_expr indent fmt) t
  | Match (value, patterns) ->
    fprintf fmt "%s| Match:\n" (String.make indent '-');
    print_expr (indent + 2) fmt value;
    List.iter
      (fun (pat, expr) ->
        fprintf fmt "%s| Pattern:\n" (String.make (indent + 2) '-');
        print_pattern (indent + 4) fmt pat;
        fprintf fmt "%s| Inner expr:\n" (String.make (indent + 2) '-');
        print_expr (indent + 4) fmt expr)
      patterns
  | Variable (Ident name) ->
    fprintf fmt "%s| Variable(%s)\n" (String.make indent '-') name
  | Unary_expr (op, expr) ->
    fprintf fmt "%s| Unary expr(\n" (String.make indent '-');
    print_unary_op indent fmt op;
    print_expr (indent + 2) fmt expr
  | Bin_expr (op, left, right) ->
    fprintf fmt "%s| Binary expr(\n" (String.make indent '-');
    print_bin_op indent fmt op;
    print_expr (indent + 2) fmt left;
    print_expr (indent + 2) fmt right
  | If_then_else (cond, then_body, else_body) ->
    fprintf fmt "%s| If Then Else(\n" (String.make indent '-');
    fprintf fmt "%sCONDITION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt cond;
    fprintf fmt "%sTHEN BRANCH\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt then_body;
    fprintf fmt "%sELSE BRANCH\n" (String.make (indent + 2) ' ');
    (match else_body with
     | Some body -> print_expr (indent + 2) fmt body
     | None -> fprintf fmt "%s| No else body\n" (String.make (indent + 2) '-'))
  | Function_def (args, body) ->
    fprintf fmt "%s| Func:\n" (String.make indent '-');
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 4) fmt) args;
    fprintf fmt "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 4) fmt body
  | Function_call (func, arg) ->
    fprintf fmt "%s| Function Call:\n" (String.make indent '-');
    fprintf fmt "%sFUNCTION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt func;
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt arg
  | LetIn (rec_flag, name, args, body, in_expr) ->
    fprintf
      fmt
      "%s | LetIn %s %s =\n"
      (String.make indent '-')
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "Rec")
      (match name with
       | Some (Ident n) -> n
       | None -> "Anonymous");
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 2) fmt) args;
    fprintf fmt "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt body;
    fprintf fmt "%sINNER EXPRESSION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt in_expr
;;

let print_statement indent fmt = function
  | Let (rec_flag, Ident name, args, body) ->
    fprintf
      fmt
      "%s | Let %s %s =\n"
      (String.make indent '-')
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "Rec")
      name;
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 2) fmt) args;
    fprintf fmt "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt body
  | ActivePattern (patterns, expr) ->
    fprintf fmt "%s| ActivePattern:\n" (String.make indent '-');
    List.iter
      (fun (Ident param) -> fprintf fmt "%s- %s\n" (String.make (indent + 2) '-') param)
      patterns;
    print_expr (indent + 2) fmt expr
;;

let print_construction fmt = function
  | Expr e -> print_expr 0 fmt e
  | Statement s -> print_statement 0 fmt s
;;

let print_p_res fmt = function
  | Some expr -> print_construction fmt expr
  | None -> fprintf fmt "Error occured"
;;
