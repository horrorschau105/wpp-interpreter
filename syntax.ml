type var = string

type typ = TNat | TTuple of typ list | TSum of (string * typ) list | TAbs of string | TPtr of typ
type pointer = Ptr of var * typ
type funcType = TFunc of typ list * typ
type expr = ENat of int 
            | EVar of var 
            | EPlus of expr * expr
            | EMinus of expr
            | EMult of expr * expr
            | ETuple of expr list
            | ETupIndex of expr * expr
            | EInj of string * expr
            | ECase of expr * (string * var * expr) list
            | EPoint of expr
            | EPtr of var

type command = CSkip
            | CIfThenElse of expr * command * command
            | CWhile of expr * command
            | CAssign of var * expr
            | CFuncCall of var * var * (expr list)
            | CNewVar of var * expr * command
            | CTypedNewVar of var * expr * typ * command
            | CTypedNewPointer of var * expr * typ * command
            | CNewPointer of var * expr * command
            | CCompose of command * command
            | CPointerAssign of var * expr

type locals = LVar of var * expr
            | LTypedVar of var * expr * typ
            | LPointer of var * expr
            | LTypedPointer of var * expr * typ

type program = PCommand of command
            | PNewVar of var * expr * program
            | PTypedNewVar of var * expr * typ * program
            | PNewPointer of var * expr * program
            | PTypedNewPointer of var * expr * typ * program
            | PNewFunc of var * (var * typ) list * locals list * command * expr * program
            | PNewType of var * typ * program

type globals = GVar of var * expr
            | GTypedVar of var * expr * typ
            | GPointer of var * expr
            | GTypedPointer of var * expr * typ
            | GFunc of var * (var * typ) list * locals list * command * expr
            | GType of var * typ

type func = FuncBody of (var list) * (locals list) * command * expr

let rec make_globals var_dcls c = 
  match var_dcls with 
  | [] -> PCommand c
  | GFunc(x, xs, ls, c', e)::tl -> PNewFunc(x, xs, ls, c', e, make_globals tl c)
  | GVar(x, e)::tl -> PNewVar(x, e, make_globals tl c)
  | GTypedVar(x, e, t)::tl -> PTypedNewVar(x, e, t, make_globals tl c)
  | GType(x, t)::tl -> PNewType(x, t, make_globals tl c)
  | GTypedPointer(x, e, t)::tl -> PTypedNewPointer(x, e, t, make_globals tl c)
  | GPointer(x, e)::tl -> PNewPointer(x, e, make_globals tl c)

let rec declare_vars xs c =
  match xs with
  | [] -> c
  | LVar(x, e)::tl -> CNewVar(x, e, declare_vars tl c)
  | LTypedVar(x, e, t)::tl -> CTypedNewVar(x, e, t, declare_vars tl c)
  | LPointer(x, e)::tl -> CNewPointer(x, e, declare_vars tl c)
  | LTypedPointer(x, e, t)::tl -> CTypedNewPointer(x, e, t, declare_vars tl c)