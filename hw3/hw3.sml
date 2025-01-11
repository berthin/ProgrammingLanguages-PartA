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

(* 1. *)
fun only_capitals(words) = List.filter (fn x => Char.isUpper(String.sub(x, 0))) words;

(* 2. *)
fun longest_string1(words) = foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" words;

(* 3. *)
fun longest_string2(words) = foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) "" words;

(* 4. *)
fun longest_string_helper(cmp) = 
    fn words => foldl (fn (s1, s2) => if cmp(String.size s1, String.size s2) then s1 else s2) "" words;

(* 4.a *)
fun longest_string3(words) = longest_string_helper (fn (x, y) => x > y) words;

(* 4.b *)
fun longest_string4(words) = longest_string_helper (fn (x, y) => x >= y) words;

(* 5. *)
val longest_capitalized = longest_string1 o only_capitals;

(* 6. *)
val rev_string = String.implode o rev o String.explode;

(* 7. *)
fun first_answer(expr) = 
    fn xs => 
        let 
            fun try([]) = raise NoAnswer
                | try(x::xs') = case expr(x) of NONE => try(xs') | SOME y => y
        in
            try(xs)
        end;

(* 8. *)
fun all_answers(cond) =
    fn xs =>
        let 
            fun try([]) = SOME []
                | try(x::xs') = case cond(x) of NONE => raise NoAnswer | SOME y => SOME (y @ valOf(try(xs')))
        in
            try(xs) handle NoAnswer => NONE
        end;

(* 9 *)
(* 9.a *)
val count_wildcards = g (fn is_wildcard => 1) (fn is_variable => 0);

(* 9.b *)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x);

(* 9.c *)
fun count_some_var (word, pattern) = g (fn _ => 0) (fn x => if x = word then 1 else 0) pattern;

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


(* 11 *)
fun match(v: valu, p: pattern) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Constructor (s1, v'), ConstructorP (s2, p')) => if s1 = s2 then match(v', p') else NONE
      | (Tuple vs, TupleP ps) => 
        if (List.length vs = List.length ps) then 
            all_answers match (ListPair.zip(vs, ps))
        else NONE
      | _ => NONE;

(* 12. *)
fun first_match v = 
    fn ps => 
        SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE;