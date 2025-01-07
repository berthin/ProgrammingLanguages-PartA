(* Dan Grossman, Coursera PL, HW2 Provided Code *)

exception Exception 

fun assert_true expr = 
    case expr of 
        true => expr
     | false => raise Exception;

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1.a *)

fun all_except_option(_, []) = NONE
    | all_except_option(word, x::xs) = 
        if same_string(word, x) then 
            SOME xs 
        else 
            let 
                val rest = all_except_option(word, xs)
            in
                if isSome rest then
                    SOME (x :: valOf rest)
                else
                    NONE
            end;


assert_true(all_except_option("he", ["he"]) = SOME []);
assert_true(all_except_option("he", ["he2"]) = NONE);
assert_true(all_except_option("he", []) = NONE);
assert_true(all_except_option("he", ["he", "hell", "hello", "hellow"]) = SOME ["hell", "hello", "hellow"]);
assert_true(all_except_option("hello", ["he", "hell", "hello", "hellow"]) = SOME ["he", "hell", "hellow"]);
assert_true(all_except_option("hellow", ["he", "hell", "hello", "hellow"]) = SOME ["he", "hell", "hello"]);
assert_true(all_except_option("hello2", ["he", "hell", "hello", "hellow"]) = NONE);

(* 1.b *)

fun get_substitutions1([], word) = []
    | get_substitutions1(x :: [], word) = 
    let 
        val result = all_except_option(word, x)
    in
        case result of
            NONE => []
          | SOME x' => x'
    end
    | get_substitutions1(x :: xs, word) = get_substitutions1([x], word) @ get_substitutions1(xs, word);
    

assert_true(get_substitutions1([["one", "two", "three"], ["is", "one", "mouse"]], "one") 
    = ["two", "three", "is", "mouse"]);
assert_true(get_substitutions1([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") 
    = ["Fredrick", "Freddie", "F"]);
assert_true(get_substitutions1([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
    = ["Jeffrey", "Geoff", "Jeffrey"]);


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
(* datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove *)

(* put your solutions for problem 2 here *)