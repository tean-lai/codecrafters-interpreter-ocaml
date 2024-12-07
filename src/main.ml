type token =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | STAR
  | EQUAL
  | EQUAL_EQUAL

let had_error = ref false

let report line where message =
  Printf.eprintf "[line %d] Error%s: %s\n" line where message;
  had_error := true

let error line message = report line "" message

let scan s =
  let rec helper seq acc line =
    match seq () with
    | Seq.Nil -> List.rev acc
    | Seq.Cons (h, t) -> (
        match h with
        | '(' -> helper t (LEFT_PAREN :: acc) line
        | ')' -> helper t (RIGHT_PAREN :: acc) line
        | '{' -> helper t (LEFT_BRACE :: acc) line
        | '}' -> helper t (RIGHT_BRACE :: acc) line
        | '.' -> helper t (DOT :: acc) line
        | ',' -> helper t (COMMA :: acc) line
        | '*' -> helper t (STAR :: acc) line
        | '+' -> helper t (PLUS :: acc) line
        | '-' -> helper t (MINUS :: acc) line
        | ';' -> helper t (SEMICOLON :: acc) line
        | '=' ->
            let equal, next =
              match t () with
              | Seq.Nil -> (EQUAL, t)
              | Seq.Cons (th, tt) ->
                  if th = '=' then (EQUAL_EQUAL, tt) else (EQUAL, t)
            in
            helper next (equal :: acc) line
        | _ ->
            error line ("Unexpected character: " ^ String.make 1 h);
            helper t acc line)
  in
  helper (String.to_seq s) [] 1

(* let eprint_endline s =
   Printf.eprintf s;
   Printf.eprintf "\n" *)

let rec pprint_tokens = function
  | [] -> print_endline "EOF  null"
  | LEFT_PAREN :: t ->
      print_endline "LEFT_PAREN ( null";
      pprint_tokens t
  | RIGHT_PAREN :: t ->
      print_endline "RIGHT_PAREN ) null";
      pprint_tokens t
  | LEFT_BRACE :: t ->
      print_endline "LEFT_BRACE { null";
      pprint_tokens t
  | RIGHT_BRACE :: t ->
      print_endline "RIGHT_BRACE } null";
      pprint_tokens t
  | COMMA :: t ->
      print_endline "COMMA , null";
      pprint_tokens t
  | DOT :: t ->
      print_endline "DOT . null";
      pprint_tokens t
  | STAR :: t ->
      print_endline "STAR * null";
      pprint_tokens t
  | PLUS :: t ->
      print_endline "PLUS + null";
      pprint_tokens t
  | MINUS :: t ->
      print_endline "MINUS - null";
      pprint_tokens t
  | SEMICOLON :: t ->
      print_endline "SEMICOLON ; null";
      pprint_tokens t
  | EQUAL :: t ->
      print_endline "EQUAL = null";
      pprint_tokens t
  | EQUAL_EQUAL :: t ->
      print_endline "EQUAL_EQUAL == null";
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

  scan file_contents |> pprint_tokens;
  if !had_error then exit 65;
  ()
