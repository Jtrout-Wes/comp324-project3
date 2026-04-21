(* COMP 324 Project 3:  C- interpreter with dynamic security enforcement.
 *
 * N. Danner
 *)

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
  let eq (_ : t) (_ : t) : bool =
    failwith "Unimplemented:  SecLab.eq"

  (* leq x y = true,  eq x y or x = Low and y = High
   *           false, o/w.
   *)
  let leq (_ : t) (_ : t) : bool =
    failwith "Unimplemented:  SecLab.leq"

  (* join x y = the maximum of x and y with respect to `leq`.
   *)
  let join (_ : t) (_ : t) : t =
    failwith "Unimplemented:  SecLab.join"

  (* [of_channel ch] = the security label associated to the channel [ch].
   *)
  let of_channel (ch : Ast.Id.t) : t =
    match ch with
    | ("stdout" | "stdin") -> Low
    | ("stdout_lo" | "stdin_lo") -> Low
    | ("stdout_hi" | "stdin_hi") -> High
    | _ -> invalid_arg ch

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

(* exec p:  Execute the program `p`.
 *)
let exec (_ : Ast.Prog.t) : unit =
  failwith "Unimplemented:  exec"

