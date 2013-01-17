(* CS421 - Spring 2011
 * MP0
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp0common

(* Problem 1 *)
let a = 17;;  (* You will want to change this. *)

(* Problem 2 *)
let s = "Hi there";;

(* Problem 3 *)
let add_a n =
  a + n

(* Problem 4 *)
let s_paired_with_a_times b =
  raise(Failure "Function not implemented yet.")

(* Problem 5 *)
let greetings name =
  if name = "Sam" then print_string ("Hi " ^ name ^ "!")
  else print_string ("Hello, " ^ name ^ ". I hope you enjoy CS421.\n")

(* Problem 6 *)
let greetstring name =
  if name = "Sam" then ("Hi " ^ name ^ "!")
  else "Hello, " ^ name ^ ". I hope you enjoy CS421.\n"

(*Problem 7 *)
let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0
