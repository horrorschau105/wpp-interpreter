open Syntax
module Map = Map.Make(String);;


let rec prettyT typ =
  match typ with
  | TNat -> "nat"
  | TTuple l -> let concated = String.concat "," (List.map prettyT l)
    in Printf.sprintf "Tuple(%s)" concated
  | TSum l -> let concated = String.concat " | " (prettyVT l)
    in Printf.sprintf "Sum(%s)" concated
  | TAbs abs -> abs
  | TPtr t -> Printf.sprintf "Ptr(%s)" (prettyT t)

and prettyVT l = List.map (fun typedvar -> let var, typ = typedvar in var ^ ": " ^ (prettyT typ)) l
and prettyE expr = 
  match expr with
  | ENat n -> string_of_int n
  | EVar x -> x
  | EPlus(e1, e2) -> Printf.sprintf "(%s + %s)" (prettyE e1) (prettyE e2)
  | EMult(e1, e2) -> Printf.sprintf "(%s * %s)" (prettyE e1) (prettyE e2)
  | EMinus(e) -> Printf.sprintf "-%s" (prettyE e) 
  | ETuple(l) -> let concated = String.concat "," (List.map prettyE l)
    in Printf.sprintf "tuple(%s)" concated
  | ETupIndex(e1, e2) -> Printf.sprintf "%s[%s]" (prettyE e1) (prettyE e2) 
  | EInj(v, e) -> Printf.sprintf "%s of %s" v (prettyE e) 
  | ECase(e, ls) -> Printf.sprintf "case (%s) of {%s}" (prettyE e) ( String.concat "|" (
      List.map ( fun lab_var_exp -> let lab, v, e = lab_var_exp in 
      Printf.sprintf "%s . %s -> %s" lab (prettyE (EVar v)) (prettyE e)) ls))
  | EPoint e -> Printf.sprintf "*%s" (prettyE e)
  | EPtr v -> Printf.sprintf "ptr(%s)" v 
    
and prettyC command =
  match command with
  | CSkip -> "SKIP"
  | CIfThenElse(e, c1, c2) -> Printf.sprintf "IF %s IS ZERO THEN %s ELSE %s" (prettyE e) (prettyC c1) (prettyC c2)
  | CWhile(e, c) -> Printf.sprintf "WHILE %s DO %s" (prettyE e) (prettyC c)
  | CAssign(x, e) -> Printf.sprintf "%s = %s" x (prettyE e)
  | CTypedNewVar(x, e, t, c) -> Printf.sprintf "LET %s : %s BE %s IN %s" x (prettyT t) (prettyE e) (prettyC c)
  | CNewVar(x, e, c) -> Printf.sprintf "LET %s BE %s IN %s" x (prettyE e) (prettyC c)
  | CCompose(c1, c2) -> Printf.sprintf "%s; %s" (prettyC c1) (prettyC c2)
  | CFuncCall(x, f, es) -> Printf.sprintf "%s = %s (%s)" x f (String.concat "," (List.map prettyE es))
  | CTypedNewPointer(x, e, t, c) -> Printf.sprintf "LET %s* : %s BE NEW %s IN %s" x (prettyT t) (prettyE e) (prettyC c)
  | CNewPointer(x, e, c) -> Printf.sprintf "LET %s BE NEW %s IN %s" x (prettyE e) (prettyC c)
  | CPointerAssign(x, e) -> Printf.sprintf "*%s = %s" x (prettyE e)
  
and prettyP program = 
  match program with
  | PNewVar (x, e, p) -> Printf.sprintf "GLOBAL %s IS %s IN %s" x (prettyE e) (prettyP p)
  | PTypedNewVar (x, e, t, p) -> Printf.sprintf "GLOBAL %s : %s IS %s IN %s" x (prettyT t) (prettyE e) (prettyP p)
  | PNewPointer (x, e, p) -> Printf.sprintf "GLOBAL %s IS NEW %s IN %s" x (prettyE e) (prettyP p)
  | PTypedNewPointer (x, e, t, p) -> Printf.sprintf "GLOBAL %s : %s* IS NEW %s IN %s" x (prettyT t) (prettyE e) (prettyP p)
  | PNewFunc (x, xs, ls, c, e, p) -> 
    let concatedVT = String.concat "," (prettyVT xs) in 
    let concatedVE = String.concat "," (List.map prettyL ls) in 
    Printf.sprintf "FUNC %s (%s) = LET %s in %s; return %s %s" x concatedVT concatedVE (prettyC c) (prettyE e) (prettyP p)
  | PNewType (x, t, p) -> Printf.sprintf "NEWTYPE %s = %s IN %s" x (prettyT t) (prettyP p)
  | PCommand comm -> Printf.sprintf "PROGRAM %s" (prettyC comm)
  
and prettyL local =
  match local with
  | LVar(x, e) -> Printf.sprintf "%s = %s" x (prettyE e)
  | LTypedVar(x, e, t) -> Printf.sprintf "%s %s = %s" (prettyT t) x (prettyE e)
  | LPointer(x, e) -> Printf.sprintf "*%s = %s" x (prettyE e)
  | LTypedPointer(x, e, t) -> Printf.sprintf "%s* %s = %s" (prettyT t) x (prettyE e)

let prettyFuncs funcs =
  print_string "Declared functions:\n";
  Map.iter (fun k v -> 
    let TFunc(l, ret) = v in 
      let concated = String.concat " ->" (List.map prettyT l) in
      Printf.printf "%s : %s -> %s\n" k concated (prettyT ret)
    ) funcs
  
let prettyTypes types = 
  print_string "Declared abstract types:\t";
  Map.iter (fun k v -> print_string (k ^ " : " ^ (prettyT v) ^ ", ")) types;
  print_string "\n"

let prettyGamma gamma = 
  print_string "Gamma:\t";
  Map.iter (fun k v -> print_string (k ^ ":" ^ (prettyT v) ^ ", ")) gamma ;
  print_string "\n"
   
let prettyMem memory =
  print_string "Values in memory:\t";
  Map.iter (fun k v -> print_string (k ^ "=" ^ (prettyE v) ^ ", ")) memory ;
  print_string "\n"

let prettyHeap heap =
  print_string "Values in heap:  \t";
  Map.iter (fun k v -> print_string (k ^ "=" ^ (prettyE v) ^ ", ")) heap ;
  print_string "\n"

let show token = 
  match token with
  | Parser.VARS -> "VARS"
  | Parser.LPAR -> "("
  | Parser.RPAR -> ")"  
  | Parser.ID _ -> "ID"  
  | Parser.NUM _ -> "NUM"
  | Parser.IFZ -> "IFZ"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.WHILE -> "WHILE"
  | Parser.DO -> "DO"
  | Parser.SKIP -> "SKIP"
  | Parser.SCOLON -> ";"
  | Parser.INITIALIZE -> ":="
  | Parser.ASSIGN -> "="
  | Parser.COMMA -> ","
  | Parser.PLUS -> "+"
  | Parser.MINUS -> "-"
  | Parser.PROD -> "*"
  | Parser.BLOCKBEG -> "{"
  | Parser.BLOCKEND -> "}"
  | Parser.IN -> "IN"
  | Parser.TUPLE -> "TUPLE"
  | Parser.LINDEXER -> "["
  | Parser.RINDEXER -> "]"
  | Parser.RETURN -> "RETURN"
  | Parser.FUNC -> "FUNC"
  | Parser.TNAT -> "INT"
  | Parser.TSUM -> "SUM"
  | Parser.TTUPLE -> "TUPLE"
  | Parser.PIPE -> "|"
  | Parser.OF -> "->"
  | Parser.DOT -> "."
  | Parser.CASE -> "CASE"
  | Parser.TYPE -> "TYPE"
  | Parser.NEW -> "NEW"
  | Parser.PTR -> "PTR"
  | _ -> "other"

let rec prettyTokens lexbuf =
  let token = Lexer.read lexbuf in
  match token with
  | Parser.EOF -> Printf.printf "\n"; ()
  | _ -> Printf.printf "%s " (show token); prettyTokens lexbuf
