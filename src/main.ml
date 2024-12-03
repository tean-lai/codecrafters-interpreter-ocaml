type token = LEFT_PAREN | RIGHT_PAREN
(* | LEFT_BRACE
   | RIGHT_BRACE
   | COMMA
   | DOT
   | MINUS
   | PLUS
   | SEMICOLON
   | STAR *)

let scan s =
  let rec helper i acc =
    if i = String.length s then List.rev acc
    else
      match s.[i] with
      | '(' -> helper (i + 1) (LEFT_PAREN :: acc)
      | ')' -> helper (i + 1) (RIGHT_PAREN :: acc)
      | _ -> failwith "shouldn't happen"
  in
  helper 0 []

let rec pprint_tokens = function
  | [] -> print_endline "EOF  null"
  | LEFT_PAREN :: t ->
      print_endline "LEFT_PAREN ( null";
      pprint_tokens t
  | RIGHT_PAREN :: t ->
      print_endline "RIGHT_PAREN ) null";
      pprint_tokens t
(* | _ -> failwith "todo" *)

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  (* Printf.eprintf "Logs from your program will appear here!\n"; *)
  if String.length file_contents > 0 then
    (* Implement & use your scanner here *)
    scan file_contents |> pprint_tokens
  else
    (* Uncomment this block to pass the first stage *)
    print_endline "EOF  null";
  (* Placeholder, remove this line when implementing the scanner *)
  ()
