(*
 * This file will be preprocessed to generate the actual OCaml file.
 *)

#define TESTRUN(WEIGHT, FNAME, F1, F2, PGM)\
  FNAME, mptest WEIGHT (ss_pair1 F1 F2 PGM)
#define TESTFUN(WEIGHT, FNAME, ARGS)\
  #FNAME^" "^#ARGS, mptest WEIGHT (ss_pair0 (test (fun () -> Solution.FNAME ARGS)) (test (fun () -> Student.FNAME ARGS)))

open Grader
open Test

(*
 * use a timeout of 4 seconds
 *)

let mptest weight pair = compare (=) 4 weight pair

#include "tests"

let _ = Main.main rubric extra_rubric rubric_title rubric_version
