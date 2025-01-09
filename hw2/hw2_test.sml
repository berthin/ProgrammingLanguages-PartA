(* Dan Grossman, Coursera PL, HW2 Provided Code *)

exception Exception;

fun assert_true expr = 
    case expr of 
        true => expr
     | false => raise Exception;

(* 1.a *)

assert_true(all_except_option("he", ["he"]) = SOME []);
assert_true(all_except_option("he", ["he2"]) = NONE);
assert_true(all_except_option("he", []) = NONE);
assert_true(all_except_option("he", ["he", "hell", "hello", "hellow"]) = SOME ["hell", "hello", "hellow"]);
assert_true(all_except_option("hello", ["he", "hell", "hello", "hellow"]) = SOME ["he", "hell", "hellow"]);
assert_true(all_except_option("hellow", ["he", "hell", "hello", "hellow"]) = SOME ["he", "hell", "hello"]);
assert_true(all_except_option("hello2", ["he", "hell", "hello", "hellow"]) = NONE);

(* 1.b *)

assert_true(get_substitutions1([["one", "two", "three"], ["is", "one", "mouse"]], "one") 
    = ["two", "three", "is", "mouse"]);
assert_true(get_substitutions1([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") 
    = ["Fredrick", "Freddie", "F"]);
assert_true(get_substitutions1([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
    = ["Jeffrey", "Geoff", "Jeffrey"]);


(* 1.c *)
assert_true(get_substitutions2([["one", "two", "three"], ["is", "one", "mouse"]], "one") 
    = ["is", "mouse", "two", "three"]);
assert_true(get_substitutions2([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") 
    = ["Freddie", "F", "Fredrick"]);
assert_true(get_substitutions2([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
    = ["Geoff", "Jeffrey", "Jeffrey"]);

(* 1.d *)
assert_true(similar_names([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], {first = "Fred", middle = "W", last = "Smith"}) 
    = [{first = "Fred", last = "Smith", middle = "W"},
       {first = "Fredrick", last = "Smith", middle = "W"},
       {first = "Freddie", last = "Smith", middle = "W"},
       {first = "F", last = "Smith", middle = "W"}]);

assert_true(similar_names([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], {first = "Fredy", middle = "W", last = "Smith"}) 
    = [{first = "Fredy", last = "Smith", middle = "W"}]);

(* 2.a *)
assert_true(card_color(Clubs, 10) = Black);
assert_true(card_color(Hearts, Queen) = Red);
assert_true(card_color(Spades, 3) = Black);
assert_true(card_color(Diamonds, Ace) = Red);

(* 2.b *)
assert_true(card_value(Clubs, Num 10) = 10);
assert_true(card_value(Clubs, Queen) = 10);
assert_true(card_value(Clubs, Ace) = 11);
assert_true(card_value(Clubs, Num 2) = 2);

(* 2.c *)
exception NotFound;

assert_true(remove_card([(Clubs, Queen), (Spades, Num 1)], (Clubs, Queen), NotFound)
    = [(Spades, Num 1)]);
assert_true(remove_card([(Clubs, Queen), (Spades, Num 1)], (Spades, Num 1), NotFound)
    = [(Clubs, Queen)]);
assert_true((remove_card([], (Spades, Num 2), NotFound) handle NotFound => [])
    = []);
assert_true((remove_card([(Clubs, Queen), (Spades, Num 1)], (Spades, Num 2), NotFound)
    handle NotFound => []) = []);

(* 2.d *)
assert_true(all_same_color([(Clubs, Queen), (Spades, Num 1), (Spades, Num 2), (Spades, Num 3)]) = true);
assert_true(all_same_color([(Clubs, Queen), (Spades, Num 1), (Hearts, Num 2)]) = false);
assert_true(all_same_color([(Clubs, Queen), (Spades, Num 1), (Clubs, Num 2)]) = true);
assert_true(all_same_color([(Spades, Num 1), (Clubs, Num 2)]) = true);
assert_true(all_same_color([(Spades, Num 2)]) = true);
    
(* 2.e *)
assert_true(sum_cards([(Clubs, Queen), (Spades, Num 1), (Spades, Num 2), (Spades, Num 3)]) = 10+1+2+3);
assert_true(sum_cards([(Clubs, Queen), (Spades, Num 1), (Hearts, Ace)]) = 10+1+11);
assert_true(sum_cards([(Clubs, Queen), (Spades, Num 1), (Clubs, Num 2)]) = 10+1+2);
assert_true(sum_cards([(Spades, Num 1), (Clubs, Num 2)]) = 1+2);
assert_true(sum_cards([(Spades, Num 2)]) = 2);

(* 2.f *)
assert_true(score([(Clubs, Queen), (Spades, Num 1), (Hearts, Ace)], 20) = 3 * (10 + 1 + 11 - 20));
assert_true(score([(Clubs, Queen), (Spades, Num 1), (Clubs, Ace)], 20) = 3 * (10 + 1 + 11 - 20) div 2);

(* 2.g *)
assert_true(officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6);

assert_true(officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3);

assert_true((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true);
             
(* This might not be true *)
assert_true(officiate([], [Draw, Draw], 10) = 10 div 2);

(* Test from coursera *)

assert_true(all_except_option ("string", ["string"]) = SOME []);
assert_true(get_substitutions1 ([["foo"],["there"]], "foo") = []);
assert_true(get_substitutions2 ([["foo"],["there"]], "foo") = []);
assert_true(similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) 
    = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]);
assert_true(card_color (Clubs, Num 2) = Black);
assert_true(card_value (Clubs, Num 2) = 2);
assert_true(remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []);
assert_true(all_same_color [(Hearts, Ace), (Hearts, Ace)] = true);
assert_true(sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4);
assert_true(score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4);

