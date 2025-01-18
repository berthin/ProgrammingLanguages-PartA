exception Exception;
fun assert(expr) = if expr = true then () else raise Exception;

(* 1. *)
assert(only_capitals(["Sample", "smAple", "S", "s"]) = ["Sample", "S"]);
assert(only_capitals([]) = []);
assert(only_capitals(["asfa"]) = []);

(* 2. *)
assert(longest_string1(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string1(["aA", "AA", "a"]) = "aA");
assert(longest_string1(["aA", "AA", "a", "A2"]) = "aA");
assert(longest_string1(["1", "aA", "AA", "a"]) = "aA");
assert(longest_string1([]) = "");

(* 3. *)
assert(longest_string2(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string2(["aA", "AA", "a"]) = "AA");
assert(longest_string2(["aA", "AA", "a", "A2"]) = "A2");
assert(longest_string2(["1", "aA", "AA", "a"]) = "AA");
assert(longest_string2([]) = "");

(* 4.a *)
assert(longest_string3(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string3(["aA", "AA", "a"]) = "aA");
assert(longest_string3(["aA", "AA", "a", "A2"]) = "aA");
assert(longest_string3(["1", "aA", "AA", "a"]) = "aA");
assert(longest_string3([]) = "");

(* 4.b *)
assert(longest_string4(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string4(["aA", "AA", "a"]) = "AA");
assert(longest_string4(["aA", "AA", "a", "A2"]) = "A2");
assert(longest_string4(["1", "aA", "AA", "a"]) = "AA");
assert(longest_string4([]) = "");

(* 5. *)
assert(longest_capitalized(["a", "AA", "asdfjnk"]) = "AA");
assert(longest_capitalized(["aA", "AA", "Abce"]) = "Abce");
assert(longest_capitalized(["aA", "AA", "a", "A2"]) = "AA");
assert(longest_capitalized(["1", "aA", "AA", "a"]) = "AA");

(* 6. *)
assert(rev_string("HouseOfCards") = "sdraCfOesuoH");
assert(rev_string("12345") = "54321");
assert(rev_string("") = "");

(* 7. *)
assert(first_answer (fn x => if x > 0 then SOME x else NONE) [~1, 2, ~3, 4] = 2);
assert(first_answer (fn x => if x > 2 then SOME x else NONE) [~1, 2, ~3, 4] = 4);
assert(first_answer (fn x => if x = 2 then SOME x else NONE) [~1, 2, ~3, 4] = 2);
assert(first_answer (fn x => if x = 8 then SOME x else NONE) [~1, 2, ~3, 4] = 2 handle NoAnswer => true);
assert(first_answer (fn x => if x = 8 then SOME x else NONE) [] = 0 handle NoAnswer => true);

(* 8. *)
assert(all_answers(fn x => if x > 0 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = NONE);
assert(all_answers(fn x => if x > 10 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = SOME [0, ~1, 2, ~3, 4]);
assert(all_answers(fn x => if x < 10 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = NONE);

(* 9.a *)
val pattern1 = TupleP [Wildcard, Wildcard, TupleP [UnitP, ConstP 4 ], Variable "name"];
val pattern2 = TupleP [Wildcard, Wildcard, TupleP [UnitP, ConstP 4, Wildcard, Wildcard], Variable "name"];

assert(count_wildcards pattern1 = 2);
assert(count_wildcards pattern2 = 4);

(* 9.b *)
assert(count_wild_and_variable_lengths pattern1 = 6);
assert(count_wild_and_variable_lengths pattern2 = 8);

(* 9.c *)
assert(count_some_var("name", pattern1) = 1);
assert(count_some_var("name", pattern2) = 1);
assert(count_some_var("name", Wildcard) = 0);
assert(count_some_var("name", Variable "Jonh") = 0);
assert(count_some_var("name", Variable "name") = 1);
assert(count_some_var("name", TupleP [ Variable "name", Variable "name"]) = 2);

(* 10 *)
assert(check_pat(pattern1) = true);
assert(check_pat(pattern2) = true);
assert(check_pat(Variable "Jonh") = true);
assert(check_pat(Variable "name") = true);
assert(check_pat(TupleP [ Variable "name", Variable "name"]) = false);
assert(check_pat(TupleP [ Variable "name", pattern2]) = false);

(* 11 *)
val pairs = ListPair.zip([1,2,3], [6,7,8]);
val sum = (fn (x, y) => SOME [(x, y, x + y)]);
all_answers sum pairs;

assert(match(Const(1), UnitP) = NONE);
assert(match(Const(1), ConstP 1) = SOME[]);
assert(match(Tuple [Const 1, Unit], Variable "name") = SOME[("name", Tuple[Const 1, Unit])]);
assert(match(Tuple [Const 34, Unit, Tuple [Unit, Const 4], Constructor ("Something", Unit)], pattern1) = SOME [("name", Constructor("Something", Unit))]);

(* 12. *)
assert(first_match (Const(1)) [UnitP, ConstP 2, ConstP 1] = SOME []);
assert(first_match (Const(10)) [UnitP, ConstP 2, ConstP 1] = NONE);
assert(first_match (Unit) [UnitP, ConstP 2, ConstP 1] = SOME []);
assert(first_match (Const(10)) [UnitP, ConstP 2] = NONE);
assert(first_match (Tuple [
        Unit, 
        Tuple [Const 2], 
        Constructor ("Constructor", Unit)
    ])
    [
        TupleP [UnitP, Wildcard, ConstructorP ("variable", Variable "variable")],
        Wildcard,
        ConstP 10
    ]
= SOME []);

assert(match (Const(3), Variable("a")) = SOME [("a", Const(3))]);
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

assert(only_capitals ["A","B","C"] = ["A","B","C"]);
assert(longest_string1 ["A","bc","C"] = "bc");
assert(longest_string2 ["A","bc","C"] = "bc");
assert(longest_string3 ["A","bc","C"] = "bc");
assert(longest_string4 ["A","B","C"] = "C");
assert(longest_capitalized ["A","bc","C"] = "A");
assert(rev_string "abc" = "cba");
assert(first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4);
assert(all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE);
assert(count_wildcards Wildcard = 1);
assert(count_wild_and_variable_lengths (Variable("a")) = 1);
assert(count_some_var ("x", Variable("x")) = 1);
assert(check_pat (Variable("x")) = true);
assert(match (Const(1), UnitP) = NONE);
assert(first_match Unit [UnitP] = SOME []);
