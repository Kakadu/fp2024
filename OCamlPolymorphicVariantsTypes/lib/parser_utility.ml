(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils

(** State of parser at one point in time.
    It is characterized by the following parameters:
    - [input]: [char list], it's the text that needs to be processed;
    - [line]: [int], it's number of line in text where the parser is located;
    - [inline]: [int], it's number of position in [line] where the parser is located. *)
type parser_state =
  { input : char list
  ; line : int
  ; inline : int
  }
[@@deriving show { with_path = false }]

(** Result of parsing process

    [!] parse_result type parametrised by ['a]
    which is type of value received upon successful completion of process

    It can be of three variants:
    - [ParseSuccess]: successful completion
      with not named parameters [{result: 'a; state: parser_state}];
    - [ParseError]: completion with a critical error
      with not named parameters [{message: string; state: parser_state}];
    - [ParseFail]: completion without error and without parameters. *)
type 'a parse_result =
  | ParseSuccess of 'a * parser_state
  | ParseError of string * parser_state
  | ParseFail
[@@deriving show { with_path = false }]

(** Parser is function that receives certain [state] and returns the result of parsing *)
type 'a parser = parser_state -> 'a parse_result [@@deriving show { with_path = false }]

(** Parser which always return [ParserFail] result *)
let pfail : _ parser = fun _ -> ParseFail

(** Parser which always return [ParseError] result with [message] *)
let perror message : _ parser = fun state -> ParseError (message, state)

(** Parser which always return [ParseSuccess] result with [value] *)
let preturn value : _ parser = fun state -> ParseSuccess (value, state)

(** Parser combinator that allows to apply function to argument

    Use case:
    [preturn (fun arg -> do_something(arg)) <*> some_parser] *)
let ( <*> ) : 'a 'b. ('a -> 'b) parser -> 'a parser -> 'b parser =
  fun pfun parg state ->
  match pfun state with
  | ParseSuccess (f, st) ->
    (match parg st with
     | (ParseFail | ParseError _) as err -> err
     | ParseSuccess (arg, st) -> preturn (f arg) st)
  | (ParseFail | ParseError _) as err -> err
;;

(** Parser combinator that allows to build parser from result of another parser *)
let ( >>= ) : 'a 'b. 'a parser -> ('a -> 'b parser) -> 'b parser =
  fun p f state ->
  match p state with
  | (ParseFail | ParseError _) as err -> err
  | ParseSuccess (res, st) -> f res st
;;

(** Parser combinator that allows to ignore left result *)
let ( *> ) : 'a 'b. 'a parser -> 'b parser -> 'b parser = fun p1 p2 -> p1 >>= fun _ -> p2

(** Parser combinator that allows to ignore right result *)
let ( <* ) : 'a 'b. 'a parser -> 'b parser -> 'a parser =
  fun p1 p2 -> p1 >>= fun res -> p2 >>= fun _ -> preturn res
;;

(** Parser combinator that matches [*] in regular expressions (Kleene's star)
    and сounting the number of successfully parsed elements *)
let rec lmany : 'a parser -> (int * 'a list) parser =
  fun p state ->
  match p state with
  | ParseFail -> preturn (0, []) state
  | ParseError (msg, st) -> perror msg st
  | ParseSuccess (res, st) -> (lmany p >>= fun (l, tl) -> preturn (l + 1, res :: tl)) st
;;

(** Parser combinator that matches [*] in regular expressions (Kleene's star) *)
let many : 'a parser -> 'a list parser =
  fun p state -> (lmany p >>= fun (_, res) -> preturn res) state
;;

(** Parser combinator that matches [+] in regular expressions
    and сounting the number of successfully parsed elements *)
let lmany1 : 'a parser -> (int * 'a list) parser =
  fun p -> p >>= fun x -> lmany p >>= fun (l, xs) -> preturn (l + 1, x :: xs)
;;

(** Parser combinator that matches [+] in regular expressions *)
let many1 : 'a parser -> 'a list parser =
  fun p -> lmany1 p >>= fun (_, res) -> preturn res
;;

let rec repeat : 'a parser -> int -> 'a list parser =
  fun p count state ->
  if count < 0
  then perror "Negative count for repeat parser" state
  else if count = 0
  then preturn [] state
  else (p >>= fun x -> repeat p (count - 1) >>= fun xs -> preturn (x :: xs)) state
;;

(** Parser combinator that allows to get result as one of two parser *)
let ( <|> ) : 'a parser -> 'a parser -> 'a parser =
  fun p1 p2 state ->
  match p1 state with
  | ParseFail -> p2 state
  | (ParseError _ | ParseSuccess _) as final -> final
;;

(** Parser combinator that allows to get result as one of two parser *)
let ( >>| ) : 'a 'b. 'a parser -> ('a -> 'b) -> 'b parser =
  fun p f state ->
  match p state with
  | (ParseFail | ParseError _) as err -> err
  | ParseSuccess (value, s) -> preturn (f value) s
;;

(** Parser combinator that allows to get result as one of [N] parsers *)
let rec one_of : 'a parser list -> 'a parser =
  fun plist state ->
  match plist with
  | p :: tl -> (p <|> one_of tl) state
  | [] -> ParseFail
;;

let reverse : 'a parser -> 'a parser -> 'a parser -> 'a parser =
  fun p pf ps state ->
  match p state with
  | ParseFail -> pf state
  | ParseSuccess (_, ns) -> ps ns
  | _ as err -> err
;;

let debug_parser : 'a parser -> string -> ('a -> string) -> 'a parser =
  fun p m pp state ->
  print_string (Format.sprintf "\ndebug: %s. Result is " m);
  match p state with
  | ParseFail ->
    print_endline "failed";
    ParseFail
  | ParseError (msg, st) ->
    print_endline (Format.sprintf "error[line=%d pos=%d]: %s" st.line st.inline msg);
    ParseError (msg, st)
  | ParseSuccess (v, st) ->
    print_endline (Format.sprintf "success: %s" (pp v));
    ParseSuccess (v, st)
;;

let handle_error : 'a parser -> (string -> 'a parser) -> 'a parser =
  fun p f state ->
  match p state with
  | (ParseFail | ParseSuccess _) as res -> res
  | ParseError (msg, _) -> f msg state
;;

(** Char predicate is function for defining characters *)
type char_predicate = char -> bool [@@deriving show { with_path = false }]

(** Char predicate to defining witespaces characters *)
let is_whitespace : char_predicate = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

(** Char predicate to defining digints characters *)
let is_digit : char_predicate = function
  | '0' .. '9' -> true
  | _ -> false
;;

(** Char predicate to defining ASCII-letter characters *)
let is_ascci_letter : char_predicate = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

(** State chager is function that chage state on reading one symbols *)
type state_changer = char -> parser_state -> parser_state
[@@deriving show { with_path = false }]

(** Default state changer *)
let default_schanger : state_changer =
  fun sym state ->
  match sym with
  | '\n' -> { state with inline = 0; line = state.line + 1 }
  | '\r' -> state
  | _ -> { state with inline = state.inline + 1 }
;;

(** Special parser function which read one character and processes it.
    Processing sybfunctions as arguments:
    - [condition]: [char_predicate],
      it's predicate for defining characters to be processed;
    - [converter]: [char -> 'a], it's function to processed character;
    - [schanger]: [state_changer option],
      it's option container to state chager function([state_changer]).
      If [schanger] is [None] then will used [default_schanger] function;
    - [state]: [parser_state], it's state of parser at beginning of reading character.

    [!] This parser returns also [ParseSuccess] or [ParseFail]*)
let rec satisfy
  :  char_predicate
  -> (char -> 'a)
  -> state_changer option
  -> parser_state
  -> 'a parse_result
  =
  fun condition converter schanger state ->
  match schanger with
  | Some changer ->
    (match state.input with
     | sym :: tl when condition sym ->
       preturn (converter sym) { (changer sym state) with input = tl }
     | _ -> ParseFail)
  | None -> satisfy condition converter (Some default_schanger) state
;;

(** Default satisfy, it's satisfy function with default [state_changer].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let dsatisfy : char_predicate -> (char -> 'a) -> parser_state -> 'a parse_result =
  fun condition converter state -> satisfy condition converter None state
;;

(** Satisfy to assert sympol *)
let asatisfy : (char -> bool) -> parser_state -> unit parse_result =
  fun cond state ->
  match state.input with
  | sym :: _ when cond sym -> preturn () state
  | _ -> pfail state
;;

(** Check that [expected] character is found further in parsing text.

    [!] This chacker returns also [ParseSuccess] or [ParseFail] *)
let symbol expected = dsatisfy (( = ) expected) Fun.id

(** Check that [expected] character sequence is found further in parsing text.

    [!] This chacker returns also [ParseSuccess] or [ParseFail] *)
let sequence expected =
  let rec helper expected storage state =
    match expected with
    | sym :: tl ->
      (match symbol sym state with
       | ParseSuccess (value, new_state) ->
         helper tl (List.append storage [ value ]) new_state
       | _ -> ParseFail)
    | _ -> preturn storage state
  in
  match expected with
  | [] -> pfail
  | _ -> helper expected []
;;

(** Check that [expected] string is found further in parsing text.

    [!] This checker returns also [ParseSuccess] or [ParseFail] *)
let ssequence expected = sequence (char_list_of_string expected)

(** Special parser to skip whitespaces

    Returns: [unit parse_result] wich always [ParseSuccess] with updated state *)
let skip_ws state =
  let rec helper st =
    match dsatisfy is_whitespace is_whitespace st with
    | ParseSuccess (is_ws, new_state) -> if is_ws then helper new_state else new_state
    | _ -> st
  in
  preturn () (helper state)
;;

(** Parser of digit character which convert it to integer in range from [0] to [9].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let digit state = satisfy is_digit int_of_digit_char None state

(** Parser for checking occurrence of char sequence in input data *)
let asequence expected =
  sequence expected
  >>= fun cl s ->
  preturn () { s with input = List.append cl s.input; inline = s.inline - List.length cl }
;;

(** Parser for checking occurrence of substring in input data *)
let assequence expected = asequence (char_list_of_string expected)

(** Initialize started parser state *)
let init_parser_state (input_string : string) =
  { input = char_list_of_string input_string; line = 1; inline = 0 }
;;

(** Run parser [f] for input string *)
let parse f inputs = f (init_parser_state inputs)

(** Convert custom parse result to string view *)
let string_of_parse_result converter = function
  | ParseFail -> "Parse process failed"
  | ParseError (msg, state) ->
    Printf.sprintf "ParseError(line=%d pos=%d): %s" state.line state.inline msg
  | ParseSuccess (r, _) -> converter r
;;
