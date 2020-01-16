open Syntax
module Map = Map.Make(String);;

exception Fail of string

let rec subst e x e' =
  match e' with 
  | ENat n -> ENat n
  | EVar y -> if x = y then e else EVar y
  | EPlus(e1, e2) -> EPlus(subst e x e1, subst e x e2)
  | EMult(e1, e2) -> EMult(subst e x e1, subst e x e2)
  | EMinus(e1) -> EMinus(subst e x e1)
  | ETuple l -> ETuple (List.map (subst e x) l)
  | ETupIndex(e1, e2) -> ETupIndex(subst e x e1, subst e x e2)
  | EInj(l, e1) -> EInj(l, subst e x e1)
  | ECase(e1, l) -> let case_exprs = List.map (fun lve -> let label, v, e2 = lve in (label, v, subst e x e2)) l in
    ECase(subst e x e1, case_exprs)
  | EPoint e1 -> EPoint (subst e x e1)
  | EPtr v -> EPtr v

let rec is_val expr =
  match expr with
  | ENat _ -> true
  | ETuple l -> (
    match l with
    | [] -> true
    | hd::tl -> 
      if is_val hd
      then
        is_val (ETuple tl)
      else
        false
    )
  | EInj(_, e) -> is_val e
  | EPtr _ ->  true
  | _ -> false
  
let rec reduceE heap memory expr = 
  match expr with 
  | ENat n -> (heap, memory, ENat n)
  | EVar v -> (heap, memory, Map.find v memory)
  (* begin 
    match Map.find v memory with
    | EVar key -> (heap, memory, Map.find key heap) (* special corner-case*)
    | expr -> (heap, memory, expr)
  end *)
  | EPlus (ENat n1, ENat n2) -> (heap, memory, ENat (n1 + n2))
  | EPlus (ENat n1, e) -> 
    let heap', mem, reduced = reduceE heap memory e
    in (heap', mem,  EPlus (ENat n1, reduced))
  | EPlus (e1, e2) -> 
    let heap', mem, reduced = reduceE heap memory e1 
    in (heap', mem,  EPlus(reduced, e2))
  | EMult (ENat n1, ENat n2) -> (heap, memory, ENat (n1 * n2))
  | EMult (ENat n1, e) -> 
    let heap', mem, reduced = reduceE heap memory e
    in (heap', mem,  EMult (ENat n1, reduced))
  | EMult (e1, e2) -> 
    let heap', mem, reduced = reduceE heap memory e1 
    in (heap', mem,  EMult(reduced, e2))
  | EMinus (ENat n) -> (heap, memory, ENat (-n))
  | EMinus expr -> 
    let heap', mem, reduced = reduceE heap memory expr 
    in (heap', mem,  EMinus (reduced))
  | ETuple l -> 
    begin match l with
    | [] -> (heap, memory, ETuple [])
    | hd::tl -> 
      if is_val hd
      then 
        let heap', mem, reduced = reduceE heap memory (ETuple tl) in         
        match reduced with
        | ETuple t -> (heap', mem, ETuple (hd::t))
        | _ -> raise (Fail "error while transforming tuples")
      else 
        let heap', mem, reduced = reduceE heap memory hd
        in (heap', mem,  ETuple (reduced::tl))
    end
  | ETupIndex(e, indexer) ->
    begin
      match indexer with
      | ENat n ->
      begin
        if is_val e
        then 
          match e with
          | ETuple l -> (heap, memory, (List.nth l n))
          | _ -> raise (Fail "reducing indexers") 
        else
          let heap', mem, reduced = reduceE heap memory e
          in (heap', mem,  ETupIndex(reduced, ENat n))
      end
      | _ -> raise (Fail "Indexer is not int")
    end
  | EInj(v, e) -> 
    if is_val e
    then (heap, memory, EInj(v, e))
    else let heap', mem, reduced = reduceE heap memory e
    in (heap', mem,  EInj (v, reduced))
  | ECase(e, l) ->
    if is_val e 
    then match e with
    | EInj(label, expr) -> begin
      let _, binded_var, expr' = List.find (fun l_v_e -> let l, var, e = l_v_e in label = l) l in
      let new_expression = subst expr binded_var expr' in
      (heap, memory, new_expression)
    end
    | _ -> raise (Fail "Case applied to wrong expression")
    else let heap', mem, reduced = reduceE heap memory e
    in (heap', mem,  ECase(reduced, l))
  | EPoint e ->
    if is_val e then
    match e with
    | EPtr v -> (heap, memory, Map.find v heap)
    | _ -> raise (Fail "Not a pointer")
    else let heap', mem, reduced = reduceE heap memory e in 
        (heap', mem, EPoint reduced)
  | EPtr e -> (heap, memory, EPtr e)
    
let rec reduceC heap funcs memory command = 
  match command with 
  | CSkip -> (heap, memory, command)
  | CAssign(x, e) -> 
    if is_val e 
    then
      let newMemory = Map.add x e memory 
      in (heap, newMemory, CSkip)
    else
      let heap', newMemory, reduced = reduceE heap memory e
      in (heap', newMemory, CAssign(x, reduced))
  | CCompose(CSkip, c2) ->
    let heap', newMemory, reduced = reduceC heap funcs memory c2
    in (heap', newMemory, reduced)
  | CCompose(c1, c2) -> 
    let heap', newMemory, reduced = reduceC heap funcs memory c1
    in (heap', newMemory, CCompose(reduced, c2))
  | CIfThenElse(ENat n, c1, c2) ->
    if n = 0 
    then (heap, memory, c1)
    else (heap, memory, c2)
  | CIfThenElse(e, c1, c2) ->
    let heap', newMemory, reduced = reduceE heap memory e
    in (heap', newMemory, CIfThenElse(reduced, c1, c2))
  | CWhile(e, c) ->
    (heap, memory, CIfThenElse(e, CSkip, CCompose(c, CWhile(e, c))))
  | CNewVar(x, _, CSkip) ->
    let oldMemory = Map.remove x memory
    in (heap, oldMemory, CSkip)  
  | CNewVar(x, e, c) ->
    if is_val e 
    then
      if Map.mem x memory = false
      then 
        let extendedMemory = Map.add x e memory
        in (heap, extendedMemory, CNewVar(x, e, c))
      else 
        let heap', newMemory, reduced = reduceC heap funcs memory c
        in (heap', newMemory, CNewVar(x, e, reduced))
    else
      let heap', newMemory, reduced = reduceE heap memory e
      in (heap', newMemory, CNewVar(x, reduced, c))
  | CTypedNewVar(x, e, t, c) -> reduceC heap funcs memory (CNewVar(x, e, c))
  | CFuncCall(x, f, es) ->
    if not (is_val (ETuple es))   (* hacky *)
    then match reduceE heap memory (ETuple es) with
    | heap', newMemory, ETuple reduceds -> (heap', newMemory, CFuncCall(x, f, reduceds))
    | _, _, _ -> raise (Fail "from tuple must result tuple")
    else 
    begin
    match Map.find f funcs with
    | FuncBody(vars, ls, c, e) ->
      let newMemory = List.fold_left2 (fun m v e -> Map.add v e m) Map.empty vars es in
      let program = List.fold_left (
        fun p xe -> match xe with
        | Syntax.LVar(x, e) -> Syntax.PNewVar(x, e, p)
        | Syntax.LTypedVar(x, e, t) -> Syntax.PTypedNewVar(x, e, t, p)
        | Syntax.LPointer(x, e) -> Syntax.PNewPointer(x, e, p)
        | Syntax.LTypedPointer(x, e, t) -> Syntax.PTypedNewPointer(x, e, t, p)
        ) (Syntax.PCommand c) (List.rev ls) in
      let final_expr = processFunctionCall heap funcs newMemory program e in 
      (heap, memory, CAssign(x, final_expr))
    | _ -> raise (Fail "Not a function body") 
    end
  | CPointerAssign(x, e) -> 
    if is_val e then 
      match Map.find x memory with
      | EPtr v -> let newHeap = Map.add v e heap in (newHeap, memory, CSkip)
      | _ -> raise (Fail "Not a pointer")
    else
      let heap', newMemory, reduced = reduceE heap memory e
      in (heap', newMemory, CPointerAssign(x, reduced))
  | CNewPointer(x, _, CSkip) ->
    let oldMemory = Map.remove x memory 
    in (heap, oldMemory, CSkip)  
  | CNewPointer(x, e, c) ->
    if is_val e 
    then
      if Map.mem x heap = false
      then 
        let extendedMemory = Map.add x (EPtr x) memory in
        let extendedHeap = Map.add x e heap in
        (extendedHeap, extendedMemory, CNewPointer(x, e, c))
      else 
        let heap', newMemory, reduced = reduceC heap funcs memory c
        in (heap', newMemory, CNewPointer(x, e, reduced))
    else
      let heap', newMemory, reduced = reduceE heap memory e
      in (heap', newMemory, CNewPointer(x, reduced, c))
  | CTypedNewPointer(x, e, t, c) -> reduceC heap funcs memory (CNewPointer(x, e, c))
 
and reduceP heap funcs memory program =
  match program with
  | PNewVar(x, e, p) -> 
    if is_val e 
    then 
      let newMemory = Map.add x e memory 
      in (heap, funcs, newMemory, p)
    else 
      let heap', mem, reduced = reduceE heap memory e 
      in (heap, funcs, mem, PNewVar(x, reduced, p))
  | PTypedNewVar(x, e, t, p) -> reduceP heap funcs memory (PNewVar(x, e, p))
  | PNewPointer(x, e, p) -> 
    if is_val e 
    then 
      let newMemory = Map.add x (EPtr x) memory in
      let newHeap = Map.add x e heap in 
      (newHeap, funcs, newMemory, p)
    else 
      let heap', mem, reduced = reduceE heap memory e 
      in (heap', funcs, mem, PNewPointer(x, reduced, p))
  | PTypedNewPointer(x, e, t, p) -> reduceP heap funcs memory (PNewPointer(x, e, p))
  | PCommand c -> 
    let heap', mem, reduced = reduceC heap funcs memory c
    in (heap', funcs, mem, PCommand reduced)
  | PNewFunc(f, xs, ls, c, e, p) ->
    if Map.mem f funcs
    then raise (Fail "func already defined") 
    else let param_names = Utils.takeFirsts xs in
    let func_body = Syntax.FuncBody(param_names, ls, c, e)
    in (heap, (Map.add f func_body funcs), memory, p)   
  | PNewType(_, _, p) -> (heap, funcs, memory, p)

and processFunctionCall heap funcs memory program expr =
  Printf.printf "\t\t%s; return %s\n" (Pretty.prettyP program) (Pretty.prettyE expr);
  Pretty.prettyMem memory;
  Pretty.prettyHeap heap;
  match program with
  | PCommand CSkip -> 
  begin 
    if is_val expr 
    then expr
    else let heap', _, reduced = reduceE heap memory expr in
    processFunctionCall heap' funcs memory program reduced
  end
  | _ -> 
  begin
    let heap', funcs', newMemory, reduced = reduceP heap funcs memory program
    in processFunctionCall heap' funcs newMemory reduced expr
  end

let rec make_step heap funcs memory program =
  Pretty.prettyMem memory;
  Pretty.prettyHeap heap;
  Printf.printf "%s\n" (Pretty.prettyP program);
  match program with
  | PCommand CSkip ->
    ();
  | _ -> let heap', fs, mem, reduced = reduceP heap funcs memory program in
    make_step heap' fs mem reduced

let start_program program = 
  try make_step Map.empty Map.empty Map.empty program
  with Fail e -> Printf.printf "%s\n" ("This exception shouldn't appear when program is well formed; " ^ e)
