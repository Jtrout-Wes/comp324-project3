(* COMP 324 Project 3:  C- interpreter with dynamic security enforcement.
 *
 * N. Danner
 *)

module Ast = Ast
module Id = Ast.Id
module E = Ast.Expr
module S = Ast.Stm

(* Raised when a function body terminates without executing `return`.
 *)
exception NoReturn of Ast.Id.t

(* MultipleDeclaration x is raised when x is declared more than once in a
 * block.
 *)
exception MultipleDeclaration of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* UndefinedFunction f is raised when f is called but has not been defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Raised when public output depends on private input.
 *)
exception SecurityError

(* Raised when the no sensitive upgrade condition is violated.
 *
 * Code that fails the NSU check must raise this exception, even if
 * [SecurityError] could be raised for other reasons.
 *)
exception NSU_Error


(* Values.
 *)
module PrimValue = struct
  type t = 
    | V_Undefined
    | V_None
    | V_Int of int
    | V_Bool of bool
    | V_Str of string
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined -> "?"
    | V_None -> "None"
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Str s -> s
end

(* Security labels.
 *
 * This module defines the two-point security lattice Low <= High.  However,
 * clients must treat the security labels abstractly in the sense that they
 * may only use the values specified in the given signature.  Notice that
 * those values:
 * - Define the type of security labels, but not the definition of that type;
 * - Define bottom, leq, and join, which taken together should make the type
 *   of security labels into a join semi-lattice.
 * - Define some convenience functions for printing and equality checking.
 * - Define [of_channel], which returns the security level associated to a
 *   given input or output channel by name.
 *
 * For this particular security lattice, `bottom` = `Low` and security labels
 * are associated to I/O channels as follows:
 *
 * stdout_lo, stdin_lo --> Low
 * stdout_hi, stdin_hi --> High
 *)
module SecLab : sig
  (** The type of a security label.
   *)
  type t

  (** The bottom security label.  It must be that [leq bottom x] = [true] for
   *  all x.
   *)
  val bottom : t

  (** A partial order on security labels.
   *)
  val leq : t -> t -> bool

  (** Equality predicate for security labels.  It must be that
   * [eq x y] = [leq x y && leq y x].
   *)
  val eq : t -> t -> bool

  (** [join x y] is the least upper bound of [x] and [y] with respect to
   * [leq].
   *)
  val join : t -> t -> t

  (** [to_string x] = a string representation of [x].
   *)
  val to_string : t -> string

  (** [pp] is a formatter for [t].
   *)
  val pp : Format.formatter -> t -> unit

  (** [of_channel ch] = the security level associated to the channel [ch].
   *)
  val of_channel : Ast.Id.t -> t
end = struct

  type t = Low | High
  [@@deriving show]

  let bottom = Low

  (* to_string x = a string representation of x.
   *)
  let to_string (x : t) : string =
    match x with
    | Low -> "L"
    | High -> "H"

  (* eq x y = true,  if x and y are the same security label
   *          false, otherwise.
   *)
  let eq (x : t) (y : t) : bool =
    match (x, y) with
    | (Low, Low) -> true
    | (High, High) -> true
    | _ -> false

  (* leq x y = true,  eq x y or x = Low and y = High
   *           false, o/w.
   * true will always be returned if x <= y. 
   *)
  let leq (x : t) (y : t) : bool =
    match eq x y with  
    | true -> true
    | false -> 
      match (x, y) with
      | (Low, High) -> true
      | _ -> false

  (* join x y = the maximum of x and y with respect to `leq`.
   *)
  let join (x : t) (y : t) : t =
    match leq x y with 
    | true -> y 
    | false -> x

  (* [of_channel ch] = the security label associated to the channel [ch].
   *)
  let of_channel (ch : Ast.Id.t) : t =
    match ch with
    | ("stdout" | "stdin") -> Low
    | ("stdout_lo" | "stdin_lo") -> Low
    | ("stdout_hi" | "stdin_hi") -> High
    | _ -> invalid_arg ch

end

(* Module for Values. 
*)

module Value = struct
  type t = 
    | V_Undefined of SecLab.t
    | V_None of SecLab.t 
    | V_Int of int * SecLab.t 
    | V_Bool of bool * SecLab.t   
    | V_Str of string * SecLab.t 
    [@@deriving show]

    let get_sec (v : t) : SecLab.t = 
      match v with 
      | V_Undefined s -> s
      | V_None s -> s 
      | V_Int (_, s) -> s
      | V_Bool (_, s) -> s
      | V_Str (_, s) -> s
    
    let get_prim (v : t) : PrimValue.t = 
      match v with 
      | V_None _ -> PrimValue.V_None
      | V_Undefined _ -> PrimValue.V_Undefined
      | V_Int (n, _) -> PrimValue.V_Int n
      | V_Bool (b, _) -> PrimValue.V_Bool b
      | V_Str (s, _) -> PrimValue.V_Str s 

    let to_string(v : t) : string =
      match v with 
      | V_Undefined _ -> "?"
      | V_None _ -> "None"
      | V_Int (prim, _) -> Int.to_string prim
      | V_Bool (prim, _) -> Bool.to_string prim
      | V_Str (prim, _) -> prim
  end

(* Module for input/output built-in functions.
 *)
module Io = struct

  (* The input source and output destination is abstracted, because there
   * are two use cases that are rather different.  The interactive
   * interpreter uses standard input and standard output for input and
   * output.  But the automated tests need the input source and output
   * destination to be programmatic values (the former is read from a test
   * specification, and the latter has to be compared to the test
   * specification).  The "right" way to do this is to make the interpreter
   * itself a functor that takes an IO module as an argument, but that is a
   * little much for this project, so instead we define this Io module with
   * the input source (`in_channel`) and output destination (`output`)
   * references that can be changed by the client that is using the
   * interpreter.
   *)

  (* The input channel.  Default is standard input.
   *)
  let in_channel : Scanf.Scanning.in_channel ref =
    ref Scanf.Scanning.stdin

  (* The output function.  Default is to print the string to standard output
   * and flush.
   *)
  let output : (string -> unit) ref = 
    ref (
      fun s ->
        Out_channel.output_string Out_channel.stdout s ;
        Out_channel.flush Out_channel.stdout
    )

  (* tail s = s[1..].
   *)
  let tail (s : string) : string =
    String.sub s 1 (String.length s - 1)

  (* tailtail s = tail(tail s).
   *)
  let tailtail (s : string) : string =
    tail (tail s)

  (* scons c s = String.make 1 c ^ s.
   *)
  let scons (c : char) (s : string) : string =
    String.make 1 c ^ s

  (* do_fprintf fmt vs:  print [vs] to stdout according to [fmt].
   *)
  let do_fprintf (fmt : string) (vs : PrimValue.t list) : unit =
    let rec build_result (fmt : string) (vs : PrimValue.t list) : string =
      if fmt = "" 
      then
        match vs with
        | [] -> ""
        | _ -> raise @@ TypeError "Too many values to print for format string"
      else if fmt.[0] != '%' then scons fmt.[0] (build_result (tail fmt) vs)
      else if String.length fmt = 1
      then raise @@ TypeError "Malformed format string (incomplete %)"
      else
        match (String.sub fmt 0 2, vs) with
        | (_, []) ->
          raise @@ TypeError "Too few values to print for format string"
        | ("%d", V_Int n :: vs') -> 
          Printf.sprintf
            "%d%s"
            n
            (build_result (tailtail fmt) vs')
        | ("%b", V_Bool b :: vs') -> 
          Printf.sprintf
            "%b%s"
            b
            (build_result (tailtail fmt) vs')
        | ("%s", V_Str s :: vs') -> 
          Printf.sprintf
            "%s%s"
            s
            (build_result (tailtail fmt) vs')
        | _ ->
          raise @@ TypeError "Bad % specifier or incorrect value type"
    in

    !output (build_result fmt vs)

  (* do_fscanf fmt = v, where v is the value read from stdin according to fmt.
   *)
  let do_fscanf (fmt : string) : PrimValue.t =
    let fmt' : string = String.trim fmt in
    match fmt' with
    | "%d" -> V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
    | "%b" -> V_Bool (Scanf.bscanf !in_channel " %b" (fun b -> b))
    | "%s" -> V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
    | _ ->
      raise @@ TypeError (
        Printf.sprintf
        "Bad scanf format string: %s"
        fmt
      )

end

(* Module for environments.
 *)
module Env = struct

  type t = (Ast.Id.t * Value.t) list 
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []

  (* find the value in the env for evaluation and acts as a 
  checker for duplicates
  *)
  let rec lookup (rho :t) (x : Ast.Id.t) : Value.t option =
    match rho with
    | [] -> None 
    | (y,v) ::rest -> if x = y then Some v else lookup rest x

  (* for let function, bind the value to env
  *)
  let bind (rho : t) (x : Ast.Id.t) (v : Value.t) : t =
    (x, v) :: rho

  (* declares a variable *)
  let declare (rho : t)(x : Ast.Id.t)(sec : SecLab.t): t = 
    match lookup rho x with 
    | Some _ -> raise (MultipleDeclaration x)
    | None -> bind rho x (Value.V_Undefined sec)                (* Not entirely sure about the security label when declaring a variable. 
                                                                Based on the operational semantics, it needs a security label based 
                                                                off of the security context. If so, do I need to pass in the security context
                                                                from my program execution? *)
  
  (* for calling a function with multiple values
  *)
  let rec bind_multi (rho : t) (params : Ast.Id.t list) (args : Value.t list) : t=
    match (params, args) with
    | [],[] -> rho
    | p::ps, a::as' -> bind_multi (bind rho p a) ps as'
    | _ -> raise (TypeError("Mismatch in number parameters and args"))

  let rec update (rho : t) (x : Ast.Id.t) (v : Value.t) : t = 
    match rho with 
    | [] ->  raise (UnboundVariable x)
    | (y, ys) :: rest -> 
      if x = y then 
        (y,v) :: rest
      else  
        (y,ys) :: update rest x v

end

module EnvList = struct

  type t = (Env.t) list 
  [@@deriving show]

  (*  empty = ρ, where dom ρ = ∅.
   *)
  let empty : t = []

  
  let rec lookup (envlist: t) (x: Ast.Id.t): Value.t = 
    match envlist with
    |[] -> raise(UnboundVariable x)
    | hd::rest -> 
        match Env.lookup hd x with 
        | Some v -> v
        | None -> lookup rest x 
  
  (* Declares a variable within the head of an EnvList *)
  let declare (envlist: t) (x : Ast.Id.t) (sec : SecLab.t): t = 
    match envlist with 
    |[] -> failwith "empty environment"
    |hd::rest -> (Env.declare hd x sec) :: rest
  
  
  let rec update (envlist: t) (x: Ast.Id.t) (v: Value.t): t = 
    match envlist with 
    |[] -> raise (UnboundVariable x)
    |hd::rest -> 
        match Env.lookup hd x with 
        | Some _ -> (Env.update hd x v) :: rest
        | None -> hd :: update rest x v
end

module Frame = struct 

  type t = 
  | EnvFrame of EnvList.t
  | ReturnFrame of Value.t 

end

let unop (op : E.unop) (v : Value.t) : Value.t =
  match (op, v) with
  | (E.Neg, Value.V_Int(n,level)) -> Value.V_Int (-n, level)
  | (E.Neg, _) -> raise (TypeError("Type cannot have negative value"))
  | (E.Not, Value.V_Bool (b, level)) -> Value.V_Bool (not b, level)
  | (E.Not, _)-> raise(TypeError("Type cannot be negated"))

let binop (op : E.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (E.Plus, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Int ((n + n'), SecLab.join lab1 lab2)
  | (E.Minus, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Int ((n - n'), SecLab.join lab1 lab2)
  | (E.Times,  Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Int ((n * n'), SecLab.join lab1 lab2)
  | (E.Div,  Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Int ((n / n'), SecLab.join lab1 lab2)
  | (E.Mod,  Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Int ((n mod n'), SecLab.join lab1 lab2)
  | (E.And, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b && b'), SecLab.join lab1 lab2)
  | (E.Or, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b || b'), SecLab.join lab1 lab2)
  | (E.Eq, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b = b'), SecLab.join lab1 lab2)
  | (E.Eq, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) ->Value.V_Bool ((n = n'), SecLab.join lab1 lab2)
  | (E.Ne, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b <> b'), SecLab.join lab1 lab2)
  | (E.Ne, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) ->Value.V_Bool ((n <> n'), SecLab.join lab1 lab2)
  | (E.Lt, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b < b'), SecLab.join lab1 lab2)
  | (E.Gt, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b > b'), SecLab.join lab1 lab2)
  | (E.Le, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b <= b'), SecLab.join lab1 lab2)
  | (E.Ge, Value.V_Bool (b, lab1), Value.V_Bool (b', lab2)) -> Value.V_Bool ((b >= b'), SecLab.join lab1 lab2)
  | (E.Lt, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Bool ((n < n'), SecLab.join lab1 lab2)
  | (E.Le, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Bool ((n <= n'), SecLab.join lab1 lab2)
  | (E.Gt, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Bool ((n > n'), SecLab.join lab1 lab2)
  | (E.Ge, Value.V_Int (n, lab1), Value.V_Int (n', lab2)) -> Value.V_Bool ((n >= n'), SecLab.join lab1 lab2)
  | (E.Eq, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool ((String.equal s s'), SecLab.join lab1 lab2)
  | (E.Ne, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool (not (String.equal s s'), SecLab.join lab1 lab2)
  | (E.Le, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool (((Id.compare s s')<=0), SecLab.join lab1 lab2)
  | (E.Lt, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool (((Id.compare s s')<0), SecLab.join lab1 lab2)
  | (E.Ge, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool (((Id.compare s s')>=0), SecLab.join lab1 lab2)
  | (E.Gt, Value.V_Str (s, lab1), Value.V_Str (s', lab2)) -> Value.V_Bool (((Id.compare s s')>0), SecLab.join lab1 lab2)
  | (_, Value.V_Str _, _)-> raise(TypeError("No binary operations on strings aside from equality/inequality checking"))
  | (_, _, Value.V_Str _)-> raise(TypeError("No binary operations on strings aside from equality/inequality checking"))
  | (_, Value.V_Bool _ , _) -> raise (TypeError("Bool type cannot undergo this binary operation"))
  | (_, _, Value.V_Bool _) -> raise (TypeError("Bool type cannot undergo this binary operation"))
  | (_, Value.V_Int _ , _) -> raise (TypeError("Int type cannot be used in this binary operation"))
  | (_, _, Value.V_Int _) -> raise (TypeError("Int type cannot be used in this binary operation"))
  | (_, Value.V_None _, _) -> raise(TypeError("No binary operations on None type"))
  | (_, Value.V_Undefined _, _) -> raise(TypeError("No binary operations on Undefined"))

(* exec p:  Execute the program `p`.
 *)
let exec (p : Ast.Prog.t) : unit =
  let (Pgm funcs) = p in

  let rec eval (rho : EnvList.t) (e : Ast.Expr.t) : Value.t =
    match e with
    | Var x -> EnvList.lookup rho x
    | Num n -> Value.V_Int n
    | Bool b -> Value.V_Bool b
    | Str s -> Value.V_Str s
    | Unop (op, e1) ->
        let v = eval rho e1 in
        unop op v
    | Binop (op, e1, e2) ->
        let v1 = eval rho e1 in
        let v2 = eval rho e2 in
        binop op v1 v2
    | Call (funcName, args) ->
        match funcName with
        | "fprintf" ->
          (match args with 
          | _ :: fmt :: xs -> 
            let pFormat = match eval rho fmt with
              | Value.V_Str str -> str
              | _ -> raise (TypeError "Format must be a string to print")
            in let vs = List.map (eval rho) xs in
             Io.do_fprintf pFormat vs;
             Value.V_None

            | _ -> raise (TypeError "fprintf expects stdout and a format string"))

        | "fscanf" ->
            raise (TypeError "fscanf is a statement, not an expression")
        | _ -> (* main and user defined functions*)
            let func =
              match List.find_opt
                      (fun (name, _, _) -> name = funcName)
                      funcs with
              | Some f -> f
              | None -> raise(UndefinedFunction(funcName))
            in
            let (_, params, body_stms) = func in
            let arg_vals = List.map (eval rho) args in
            let func_env = Env.bind_multi Env.empty params arg_vals in
            let func_rho : EnvList.t = [func_env] in
            match exec_seq body_stms func_rho with
            | Frame.EnvFrame _ ->  raise(NoReturn(funcName))
            | Frame.ReturnFrame v -> v

    (* and allows for mutually recursive functions. Statements will 
    either result in a changed EnvList or returns a Value.t; 
    hence, we need Frame here *)
    and exec_stm (stm : Ast.Stm.t) (rho : EnvList.t) : Frame.t = 
      match stm with 
      | VarDec declarations -> 
        let rho' = List.fold_left (fun env (x, _) -> EnvList.declare env x) rho declarations
          in let rho'' = List.fold_left (fun env (x, e_opt) -> 
            match e_opt with
            | None -> env
            | Some e -> let v = eval rho e in EnvList.update env x v) rho' declarations in Frame.EnvFrame rho''
    | Fscanf (_, fmt, var_id) ->
        let v = Io.do_fscanf fmt in
        let rho' = EnvList.update rho var_id v in
        Frame.EnvFrame rho'
    | Assign (x, e) ->
        let v = eval rho e in
        let rho' = EnvList.update rho x v in
        Frame.EnvFrame rho'
    | Expr e ->
        let _ = eval rho e in
        Frame.EnvFrame rho
    | Block stms ->
        let rho' = Env.empty :: rho in
        (match exec_seq stms rho' with
         | Frame.ReturnFrame v -> Frame.ReturnFrame v
         | Frame.EnvFrame (_ :: rest) -> Frame.EnvFrame rest
         | Frame.EnvFrame [] -> Frame.EnvFrame rho)
    | IfElse (e, s, s') ->
        (match eval rho e with
         | Value.V_Bool true -> exec_stm s rho
         | Value.V_Bool false -> exec_stm s' rho
         | _ -> raise (TypeError "If condition must be boolean"))
    | While (cond, body) ->
        exec_while rho cond body
    | Return opt ->
        let v = match opt with
          | None -> Value.V_None
          | Some e -> eval rho e
        in
        Frame.ReturnFrame v

  and exec_seq (stms : Ast.Stm.t list) (rho : EnvList.t) : Frame.t =
    match stms with
    | [] -> Frame.EnvFrame rho
    | hd :: rest ->
        match exec_stm hd rho with
         | Frame.EnvFrame rho' -> exec_seq rest rho'
         | Frame.ReturnFrame v -> Frame.ReturnFrame v

  and exec_while (rho : EnvList.t) (cond : Ast.Expr.t) (body : Ast.Stm.t) : Frame.t =
    match eval rho cond with
    | Value.V_Bool false -> Frame.EnvFrame rho
    | Value.V_Bool true ->
        (match exec_stm body rho with
         | Frame.ReturnFrame v -> Frame.ReturnFrame v
         | Frame.EnvFrame rho' -> exec_while rho' cond body)
    | _ -> raise (TypeError "While condition must be boolean")

  in

  let _ = eval [[]] (Call ("main", [])) in
  ()



