#use "topfind";;
#require "unix";;
#require "str";;

type mode = Master | PR
type config =
  { mutable repo : string
  ; mutable user : string
  ; mutable user_branch : string
  ; mutable mode: mode
  ; mutable verbose : bool
  }

let config = { user = "Kakadu"; repo = ""; user_branch = "master"; mode = PR; verbose = false }
let eprintfn fmt = Format.kasprintf (Printf.eprintf "%s\n%!") fmt
let log fmt = Format.kasprintf (fun s -> if config.verbose then Printf.eprintf "%s\n%!" s) fmt

let () =
  Arg.parse
    [ "-repo", Arg.String (fun s -> config.repo <- s), " Github repository"
    ; "-user", Arg.String (fun s -> config.user <- s), " Github user"
    ; "-master", Arg.Unit (fun () -> config.mode <- Master), " "
    ; "-v", Arg.Unit (fun () -> config.verbose <- true), " verbose logging"
    ]
    (fun s -> config.user_branch <- s)
    "TODO: help";
  if config.user = "" then (eprintfn "User is empty"; exit 1);
  if config.repo = "" then (eprintfn "Repo is empty"; exit 1);
;;

let red = "\027[0;31m"
let no_color = "\027[0m"

let get_output fmt =
  Format.kasprintf
    (fun cmd ->
      log "Running: %s%s%s" red cmd no_color;
      let ch = Unix.open_process_in cmd in
      let s = In_channel.input_all ch in
      let () = In_channel.close ch in
      s)
    fmt
;;

let commandf fmt = Format.kasprintf Sys.command fmt

open Printf

(* let () = print_endline "hello world" *)

let () =
  let s = get_output "git remote | grep upstream" in
  log "%S" s;
  if not (List.mem "upstream" @@ Str.split (Str.regexp "\n") s)
  then (
    let _ =
      commandf
        "git remote add upstream https://github.com/%s/%s.git"
        config.user
        config.repo
    in
    match commandf "git fetch upstream master" with
    | 0 -> Format.eprintf "Upstream added\n%!"
    | err ->
        eprintfn "Adding upstream finished with error code %d" err;
        exit 1)
;;


let calculate_common_subdir files =
  let module SS = Set.Make (String) in
  let get_top_dir name =
    let prefix_len =
      match String.index name '/' with
      | exception Not_found -> String.length name
      | n -> n
    in
    String.sub name 0 prefix_len
  in
  let set = List.fold_right (fun x acc -> SS.add (get_top_dir x) acc) files SS.empty in
  SS.to_seq set |> List.of_seq
;;

let pp_str_list ppf xs = Format.fprintf ppf "[ %s ]" (String.concat ", " xs)

let merge_base =
  match config.mode with
  | Master -> "HEAD~1"
  | PR ->
    let s = get_output "git merge-base upstream/master %s" (* user_branch *) "HEAD" in
    match Str.split (Str.regexp "\n") s |> List.filter (( <> ) "") with
    | [ h ] -> h
    | xs ->
      eprintfn "After merge-base got a list of length %d" (List.length xs);
      eprintfn "[ %s ]" (String.concat "; " xs);
      exit 1
;;
let the_HEAD =
  match config.mode with
  | Master -> "HEAD"
  | PR -> config.user_branch

let () = log "merge_base: %S" merge_base

(*  *)
let () =
  let s =
    get_output
      "git diff-tree %s..%s  | rev | cut -f 1 | rev"
      merge_base
      the_HEAD
  in
  log "%S " s;
  let changed_files =
    String.split_on_char '\n' s
    |> List.filter (function
         | "" -> false
         | s
           when Sys.file_exists s
                && (not (Sys.is_directory s))
                && Filename.dirname s = Filename.current_dir_name -> false
         | _ -> true)
  in
  log "%s " (String.concat ", " changed_files);
  match changed_files with
  | [] ->
    eprintfn "No changed files.\n%!";
    print_endline "latest=Lambda";
    exit 0
  | xs ->
    (match calculate_common_subdir xs with
     | [] -> assert false
     | [ ".github" ] ->
       Printf.printf "latest=%s\n%!" "Lambda";
       exit 0
     | [ h ] ->
       Printf.printf "latest=%s\n%!" h;
       exit 0
     | ds ->
       eprintfn "Too many directories has been changed: %a" pp_str_list ds;
       exit 1)
;;
