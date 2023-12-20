let argparse (options:string list) =
  let argv:string list = Array.to_list(Sys.argv) in
  let argv = match argv with [] -> failwith "error" | _::xs -> xs in 
  let files = ref [] in
  let opts = ref [] in
  let rec parse = function
    | [] ->
      if List.length !files = 0 then failwith "error";
      (!opts,!files)
    | x::xs when List.mem x options ->
      opts := x::!opts;
      parse xs
    | x::xs ->
      files := x::!files;
      parse xs
  in
  parse argv
let () =
  let opts,files = try
    argparse ["-json"]
  with _ ->
    Format.printf "Usage: ./_cc [-json] [filename]\n";
    exit (-1)
  in
  files |> List.iter (fun fname ->
    let inchan = open_in fname in
    let filebuf = Lexing.from_channel inchan in
    ignore (Uscoc.Parser.translation_unit Uscoc.Lexer.token filebuf);
    if List.mem "-json" opts then
      print_endline
        (Uscoc.Json.programi
            (List.mapi (fun i x -> (i, x)) (List.rev !Uscoc.Env.program)))
    else
      print_endline
        (Uscoc.Ast.show_programi
            (List.rev (List.mapi (fun i x -> (i, x)) (List.rev !Uscoc.Env.program))))
  )
