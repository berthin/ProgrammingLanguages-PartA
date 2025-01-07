(* Homework0 Simple Test *)

(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = double 17 = 34

val test2 = double 0 = 0

val test3 = triple ~4 = ~12:bn

val test4 = triple 0 = 0

val test5 = f(12,27) = 324

(* You can add more tests here, for example you can uncomment the line below
by deleting the first two character and last two characters on the line *)

(* val test6 = triple ~1 = ~3 *)

val test6 = double ~3 = (~3 * 2)


syntax: e1 < e2
type checking: e1 and e2 should have the same type and evaluate to type bool
evaluation: in the dynamic env we compare the values of e1 and e2, the result is true if e1 is less than e2, otherwise the evaluation is false