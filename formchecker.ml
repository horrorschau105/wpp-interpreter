open Syntax
module Map = Map.Make(String);;

exception ProgMismatch of Syntax.program
exception FormMismatch of Syntax.command
exception ExprMismatch of Syntax.expr
exception TypeMismatch of typ * typ
exception VarNotDeclared of var
exception VarAlreadyDeclared of var
exception FunctionAlreadyDeclared of var
exception FunctionNotDeclared of var
exception IndexerNotConstant of Syntax.expr
exception IndexOfOutBound of Syntax.expr
exception NotTuple of Syntax.expr
exception IncorrectNumberOfArguments
exception CaseExpressionsNotOfUniformType
exception InjectionIntoWrongType of Syntax.expr
exception CaseOnWrongType of Syntax.expr
exception TypeNotDeclared of Syntax.typ
exception NotAPointer of Syntax.expr


let raiseIfIn x gamma =
  if Map.mem x gamma
  then raise (VarAlreadyDeclared x)

let raiseIfNotIn x gamma = 
  if not (Map.mem x gamma)
  then raise (VarNotDeclared x)

let rec feedGamma gamma l =
  match l with
  | [] -> gamma
  | (v, t) :: tl -> 
    raiseIfIn v gamma;
    feedGamma (Map.add v t gamma) tl

let rec checkType types t =
  match t with
  | TNat -> ()
  | TTuple l -> List.iter (checkType types) l
  | TSum l ->
    feedGamma Map.empty l; 
    List.iter (checkType types) (Utils.takeSeconds l)
  | TAbs s-> 
    if Map.mem s types
    then ()
    else raise (TypeNotDeclared t)
  | TPtr t' -> checkType types t'

let rec inferE types gamma expr =
  match expr with 
  | ENat n -> TNat
  | EVar x -> raiseIfNotIn x gamma; Map.find x gamma
  | EPlus(e1, e2) -> 
    check types gamma e1 TNat;
    check types gamma e2 TNat;
    TNat
  | EMult(e1, e2) -> 
    check types gamma e1 TNat;
    check types gamma e2 TNat;
    TNat
  | EMinus(e) ->
    check types gamma e TNat;
    TNat
  | ETuple e -> 
    begin
      match e with 
      | [] -> TTuple []
      | hd::tl -> 
      begin
        let hd_type = inferE types gamma hd in 
        match inferE types gamma (ETuple tl) with 
        | TTuple (tail) -> TTuple(hd_type :: tail)
        | _ -> raise (ExprMismatch (ETuple tl))
      end
    end    
  | ETupIndex(e1, e2) -> 
  begin
    match e2 with
    | ENat n -> (
      match inferE types gamma e1 with
      | TTuple l -> (
        if (List.length l) <= n || n < 0
        then raise (IndexOfOutBound (ETupIndex (e1,e2)))
        else (List.nth l n)
      )
      | _ -> raise (NotTuple e1)
    )
    | _ -> raise (IndexerNotConstant (ETupIndex (e1,e2)))
  end
  | EInj(v, e) -> let t = inferE types gamma e in TSum [(v, t)]
  | ECase(e, l) -> 
  begin
    match removeAbstracts types (inferE types gamma e) with
    | TSum subTypes ->
    begin
      let case_labels, case_vars, case_exprs = Utils.unzip3 l in
      let labelMap = feedGamma Map.empty (Utils.takeFstSnd l) in 
      let expected_labels, var_types = Utils.unzip subTypes in
      List.iter (fun v -> raiseIfNotIn v labelMap) expected_labels;       
      let expectedMap = feedGamma Map.empty subTypes in
      List.iter (fun v -> raiseIfNotIn v expectedMap) case_labels;
      List.iter (fun v -> raiseIfIn v gamma) case_vars;
      let resultTypes = List.map (
        fun l_v_e -> let label, var, expr = l_v_e in 
        let vartype = Map.find label expectedMap in
        inferE types (Map.add var vartype gamma) expr
      ) l in 
      let most_generals = (* looking for type 'containing' all of returning types*)
        List.find_all (fun candidate -> List.for_all (compare types candidate) resultTypes) resultTypes in
      if List.length most_generals = 0
      then raise CaseExpressionsNotOfUniformType
      else List.nth most_generals 0
    end
    | _ -> raise (CaseOnWrongType (ECase(e,l)))
  end
  | EPoint e ->
  begin
    match inferE types gamma e with
    | TPtr t' -> t'
    | _ -> raise (NotAPointer e)
  end
  | EPtr v -> TPtr (inferE types gamma (EVar v))
  
and removeAbstracts types t =
  match t with 
  | TAbs t' -> 
  if Map.mem t' types then removeAbstracts types (Map.find t' types)
  else raise (TypeNotDeclared t)
  | _ -> t

and check types gamma expr expected =
  let infered = inferE types gamma expr in 
  if not (compare types expected infered)
  then raise (TypeMismatch (expected, infered))
  
and compare types expected infered = (* return true if expected >= infered*)
  if not (expected = infered) 
  then match expected, infered with
  | TSum exp, TSum inf -> 
  begin 
    (* 
      In this case infered type can be 'smaller' than expected
      It means infered type doesn't contain all possible injection, but only just applied
    *)
    try
    let subTypes = feedGamma Map.empty exp in
    List.iter (fun x -> raiseIfNotIn x subTypes) (Utils.takeFirsts inf);
    List.for_all (
      fun label_type -> 
      let label, typ = label_type in
      let subTyp = Map.find label subTypes in
      compare types subTyp typ) inf
    with VarNotDeclared v -> false
  end
  | TTuple exp, TTuple inf ->
  begin
    try List.for_all2 (fun ex infer -> compare types ex infer) exp inf
    with VarNotDeclared v -> false
  end
  | TPtr x, TPtr y -> compare types x y
  | TAbs x, y -> 
    if Map.mem x types then compare types (Map.find x types) y
    else false
  | y, TAbs x -> 
    if Map.mem x types then compare types (Map.find x types) y
    else false   
  | _, _ -> false
  else true

let rec wellformedC types funcs gamma command = 
  Pretty.prettyGamma gamma; 
  match command with 
  | CSkip -> ()
  | CIfThenElse(e, c1, c2) -> 
    check types gamma e TNat;
    wellformedC types funcs gamma c1;
    wellformedC types funcs gamma c2;
  | CWhile(e, c) ->
    check types gamma e TNat;
    wellformedC types funcs gamma c;
  | CAssign(x, e) -> 
    check types gamma e (inferE types gamma (EVar x))
  | CPointerAssign(x, e) -> 
  begin
    match inferE types gamma (EVar x) with
    | TPtr t' -> check types gamma e t'
    | _ -> raise (NotAPointer e)
  end
  | CNewVar(x, e, c) ->
    raiseIfIn x gamma;  
    let extended_gamma = Map.add x (inferE types gamma e) gamma in
    wellformedC types funcs extended_gamma c
  | CTypedNewVar(x, e, t, c) ->
    raiseIfIn x gamma;
    let infered = inferE types gamma e in
    if not (compare types t infered) then raise (TypeMismatch (t, infered))
    else let extended_gamma = Map.add x t gamma in
    wellformedC types funcs extended_gamma c
  | CNewPointer(x, e, c) ->
    raiseIfIn x gamma;  
    let extended_gamma = Map.add x (TPtr (inferE types gamma e)) gamma in
    wellformedC types funcs extended_gamma c
  | CTypedNewPointer(x, e, t, c) ->
    raiseIfIn x gamma;
    let infered = TPtr(inferE types gamma e) in
    if not (compare types t infered) then raise (TypeMismatch (t, infered))
    else let extended_gamma = Map.add x t gamma in
    wellformedC types funcs extended_gamma c
  | CCompose(c1, c2) ->
    wellformedC types funcs gamma c1;
    wellformedC types funcs gamma c2;
  | CFuncCall(x, f, es) ->
    raiseIfNotIn f funcs;    
    let TFunc(in_types, out_type) = Map.find f funcs in
      check types gamma (EVar x) out_type;
      if not (List.length es = List.length in_types)
      then raise IncorrectNumberOfArguments
      else List.iter2 (check types gamma) es in_types
 | c -> raise (FormMismatch c)

let rec wellformedC' types funcs gamma command = 
  Pretty.prettyFuncs funcs;
  Pretty.prettyTypes types;
  wellformedC types funcs gamma command

let rec wellformedP types funcs vars program = 
  match program with
  | PNewVar (x, expr, p) -> 
    raiseIfIn x vars;
    wellformedP types funcs (Map.add x (inferE types vars expr) vars) p
  | PTypedNewVar(x, expr, t, p) ->
    raiseIfIn x vars;    
    checkType types t;
    let infered = inferE types vars expr in
    if compare types t infered 
    then wellformedP types funcs (Map.add x t vars) p
    else raise (TypeMismatch (t, infered))
  | PNewPointer (x, expr, p) -> 
    raiseIfIn x vars;
    wellformedP types funcs (Map.add x (TPtr (inferE types vars expr)) vars) p
  | PTypedNewPointer(x, expr, t, p) ->
    raiseIfIn x vars;    
    checkType types t;
    let infered = TPtr(inferE types vars expr) in
    if compare types t infered 
    then wellformedP types funcs (Map.add x t vars) p
    else raise (TypeMismatch (t, infered))
  | PCommand(c) -> wellformedC' types funcs vars c
  | PNewFunc(x, xs, ls, c, e, p) -> 
    raiseIfIn x funcs;    
    let localGamma = feedGammaWithLocals types (feedGamma Map.empty xs) ls in
    let funcType = TFunc(Utils.takeSeconds xs, inferE types localGamma e) in  
    let freshFuncs = Map.add x funcType funcs in
    wellformedC types freshFuncs localGamma c;      
    wellformedP types freshFuncs vars p
  | PNewType(x, t, p) ->
    raiseIfIn x types;
    let updatedTypes = Map.add x t types in
    checkType updatedTypes t;
    wellformedP updatedTypes funcs vars p
  | p -> raise (ProgMismatch p)

and feedGammaWithLocals types gamma locals =
  match locals with
  | [] -> gamma
  | LVar(x, e) :: tl -> 
    raiseIfIn x gamma;
    let infered = inferE types gamma e in
    feedGammaWithLocals types (Map.add x infered gamma) tl
  | LTypedVar(x, e, t) :: tl ->
    raiseIfIn x gamma;
    let infered = inferE types gamma e in
    if compare types t infered
    then feedGammaWithLocals types (Map.add x t gamma) tl
    else raise(TypeMismatch (t, infered))
  | LPointer(x, e) :: tl -> 
    raiseIfIn x gamma;
    let infered = inferE types gamma e in
    feedGammaWithLocals types (Map.add x (TPtr infered) gamma) tl
  | LTypedPointer(x, e, t) :: tl ->
    raiseIfIn x gamma;
    let infered = inferE types gamma e in
    if compare types t infered
    then feedGammaWithLocals types (Map.add x (TPtr t) gamma) tl
    else raise(TypeMismatch (t, infered))

let showException e = 
  match e with
  | ProgMismatch p -> "ProgMismatch: " ^ Pretty.prettyP p
  | FormMismatch c -> "CommandMismatch: " ^ Pretty.prettyC c
  | ExprMismatch e -> "ExprMismatch: " ^ Pretty.prettyE e
  | TypeMismatch (t, t') -> "TypeMismatch: " ^ Pretty.prettyT t ^ " != " ^ Pretty.prettyT t'
  | VarNotDeclared v -> "NotDeclared: " ^ Pretty.prettyE (EVar v)
  | VarAlreadyDeclared v -> "AlreadyDeclared: " ^ Pretty.prettyE (EVar v)
  | IndexerNotConstant e -> "IndexerNotConstant: " ^ Pretty.prettyE e 
  | IndexOfOutBound e -> "IndexOfOutBound: " ^ Pretty.prettyE e
  | NotTuple e -> "NotTuple: " ^ Pretty.prettyE e
  | FunctionAlreadyDeclared e -> "FunctionAlreadyDeclared: " ^ e
  | FunctionNotDeclared e -> "FunctionNotDeclared: " ^ e
  | IncorrectNumberOfArguments -> "IncorrectNumberOfArguments"  
  | InjectionIntoWrongType e -> "InjectionIntoWrongType" ^ Pretty.prettyE e
  | CaseOnWrongType e -> "CaseOnWrongType" ^ Pretty.prettyE e
  | CaseExpressionsNotOfUniformType -> "CaseExpressionsNotOfUniformType"
  | TypeNotDeclared t -> "TypeNotDeclared: " ^ Pretty.prettyT t
  | NotAPointer e -> "NotAPointer: " ^ Pretty.prettyE e
  | _ -> raise e

let wellformed program = 
  try 
    wellformedP Map.empty Map.empty Map.empty program; 
    Printf.printf "%s\n" "Program is well formed!";
  with e -> 
    begin 
      Printf.printf "%s\n" (showException e);
      failwith "Typechecking failed"
    end