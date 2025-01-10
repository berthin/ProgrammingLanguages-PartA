(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

exception Exception;
fun assert(expr) = if expr = true then () else raise Exception;

(* 1. *)
fun only_capitals(words) = List.filter (fn x => Char.isUpper(String.sub(x, 0))) words;

assert(only_capitals(["Sample", "smAple", "S", "s"]) = ["Sample", "S"]);
assert(only_capitals([]) = []);
assert(only_capitals(["asfa"]) = []);

(* 2. *)
fun longest_string1(words) = foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" words;

assert(longest_string1(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string1(["aA", "AA", "a"]) = "aA");
assert(longest_string1(["aA", "AA", "a", "A2"]) = "aA");
assert(longest_string1(["1", "aA", "AA", "a"]) = "aA");
assert(longest_string1([]) = "");

(* 3. *)
fun longest_string2(words) = foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) "" words;

assert(longest_string2(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string2(["aA", "AA", "a"]) = "AA");
assert(longest_string2(["aA", "AA", "a", "A2"]) = "A2");
assert(longest_string2(["1", "aA", "AA", "a"]) = "AA");
assert(longest_string2([]) = "");

(* 4. *)
(* int * int -> bool -> string list -> string *)
(* cmp -> string list -> string *)
fun longest_string_helper(cmp) = 
    fn words => foldl (fn (s1, s2) => if cmp(String.size s1, String.size s2) then s1 else s2) "" words;

(* 4.a *)
fun longest_string3(words) = longest_string_helper (fn (x, y) => x > y) words;
assert(longest_string3(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string3(["aA", "AA", "a"]) = "aA");
assert(longest_string3(["aA", "AA", "a", "A2"]) = "aA");
assert(longest_string3(["1", "aA", "AA", "a"]) = "aA");
assert(longest_string3([]) = "");

(* 4.b *)
fun longest_string4(words) = longest_string_helper (fn (x, y) => x >= y) words;

assert(longest_string4(["a", "AA", "asdfjnk"]) = "asdfjnk");
assert(longest_string4(["aA", "AA", "a"]) = "AA");
assert(longest_string4(["aA", "AA", "a", "A2"]) = "A2");
assert(longest_string4(["1", "aA", "AA", "a"]) = "AA");
assert(longest_string4([]) = "");

(* 5. *)
val longest_capitalized = longest_string1 o only_capitals;

assert(longest_capitalized(["a", "AA", "asdfjnk"]) = "AA");
assert(longest_capitalized(["aA", "AA", "Abce"]) = "Abce");
assert(longest_capitalized(["aA", "AA", "a", "A2"]) = "AA");
assert(longest_capitalized(["1", "aA", "AA", "a"]) = "AA");

(* 6. *)

(* string -> string *)
val rev_string = String.implode o rev o String.explode;

assert(rev_string("HouseOfCards") = "sdraCfOesuoH");
assert(rev_string("12345") = "54321");
assert(rev_string("") = "");

(* 7. *)
(* (('a -> 'b option) -> 'a list ) -> 'b *)
fun first_answer(expr) = 
    fn xs => 
        let 
            fun try([]) = raise NoAnswer
                | try(x::xs') = case expr(x) of NONE => try(xs') | SOME y => y
        in
            try(xs)
        end;

(* TODO: what if it's empty *)
assert(first_answer (fn x => if x > 0 then SOME x else NONE) [~1, 2, ~3, 4] = 2);
assert(first_answer (fn x => if x > 2 then SOME x else NONE) [~1, 2, ~3, 4] = 4);
assert(first_answer (fn x => if x = 2 then SOME x else NONE) [~1, 2, ~3, 4] = 2);
assert(first_answer (fn x => if x = 8 then SOME x else NONE) [~1, 2, ~3, 4] = 2 handle NoAnswer => true);
assert(first_answer (fn x => if x = 8 then SOME x else NONE) [] = 0 handle NoAnswer => true);

(* 8. *)
fun all_answers(cond) =
    fn xs =>
        let 
            fun try([]) = SOME []
                | try(x::xs') = case cond(x) of NONE => raise NoAnswer | SOME y => SOME (y @ valOf(try(xs')))
        in
            try(xs) handle NoAnswer => NONE
        end;

assert(all_answers(fn x => if x > 0 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = NONE);
assert(all_answers(fn x => if x > 10 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = SOME [0, ~1, 2, ~3, 4]);
assert(all_answers(fn x => if x < 10 then NONE else SOME [x]) [0, ~1, 2, ~3, 4] = NONE);


(* 9 *)
(* 9.a *)
(* 
fun g return_1 return_0 p =
    let 
	val r = g return_1 return_0 
    in
	case p of
	    Wildcard          => return_1 ()
	  | Variable x        => return_0 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end 
*)

val count_wildcards = g (fn is_wildcard => 1) (fn is_variable => 0);

val pattern1 = TupleP [Wildcard, Wildcard, TupleP [UnitP, ConstP 4 ], Variable "name"];
val pattern2 = TupleP [Wildcard, Wildcard, TupleP [UnitP, ConstP 4, Wildcard, Wildcard], Variable "name"];

assert(count_wildcards pattern1 = 2);
assert(count_wildcards pattern2 = 4);

(* 9.b *)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x);

assert(count_wild_and_variable_lengths pattern1 = 6);
assert(count_wild_and_variable_lengths pattern2 = 8);

(* 9.c str, pattern -> num of times string appears in pattern*)
fun count_some_var (word, pattern) = g (fn _ => 0) (fn x => if x = word then 1 else 0) pattern;

assert(count_some_var("name", pattern1) = 1);
assert(count_some_var("name", pattern2) = 1);
assert(count_some_var("name", Wildcard) = 0);
assert(count_some_var("name", Variable "Jonh") = 0);
assert(count_some_var("name", Variable "name") = 1);
assert(count_some_var("name", TupleP [ Variable "name", Variable "name"]) = 2);


(* 10 *)
val check_pat =
    let
        fun all_variables(pattern) =
            case pattern of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p', xs) => all_variables(p') @ xs) [] ps
              | ConstructorP(_, p') => all_variables(p')
              | _ => []
        
        fun duplicates([]) = false
            | duplicates(x::[]) = false
            | duplicates(x::xs) = (List.exists (fn y => x = y) xs) orelse duplicates(xs)
    in
        not o duplicates o all_variables
    end;


assert(check_pat(pattern1) = true);
assert(check_pat(pattern2) = true);
assert(check_pat(Variable "Jonh") = true);
assert(check_pat(Variable "name") = true);
assert(check_pat(TupleP [ Variable "name", Variable "name"]) = false);
assert(check_pat(TupleP [ Variable "name", pattern2]) = false);

(* 11 *)

(* Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match 
SOME lst where lst is the list of bindings if it does.

Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. 

Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. 
Sample solution is 13 lines. 
Remember to look above for the
rules for what patterns match what values, and what bindings they produce. 
These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier. *)

(* valu datatype Const of int| Unit | Tuple of value list *)
(* fun match(val: valu, pattern: pattern): (string * value) list option = NONE; *)
