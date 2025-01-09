fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1. *)
(* 1.a *)
fun all_except_option(word, xs) =
    case xs of
        [] => NONE
      | x::xs' => 
            if same_string(word, x) then 
                SOME xs' 
            else 
                case all_except_option(word, xs') of
                    NONE => NONE
                    | SOME y => SOME (x::y);

(* 1.b *)
fun get_substitutions1([], word) = []
    | get_substitutions1(x :: [], word) = 
    let in
        case all_except_option(word, x) of
            NONE => []
          | SOME x' => x'
    end
    | get_substitutions1(x :: xs, word) = get_substitutions1([x], word) @ get_substitutions1(xs, word)
    
(* 1.c *)
fun get_substitutions2(list_list_words, word) =
    let 
        fun tail_recursive([], answer) = answer
            | tail_recursive(x :: [], answer) = 
                let 
                    val result = all_except_option(word, x)
                in
                    case result of
                        NONE => answer
                      | SOME x' => x' @ answer
                end
            | tail_recursive(x :: xs, answer) = tail_recursive(xs, tail_recursive([x], answer))
    in
        tail_recursive(list_list_words, [])
    end;
    
(* 1.d *)
fun similar_names(list_list_names, {first, middle, last}) = 
    let 
        fun compose_names([]) = []
            | compose_names(x::xs) = {first=x, middle=middle, last=last} :: compose_names(xs)
    in 
        {first=first, middle=middle, last=last} :: compose_names(get_substitutions1(list_list_names, first))
    end;

(* 2. *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2.a *)
fun card_color(s, _) = 
    case s of
        Clubs => Black
      | Spades => Black
      | _ => Red;

(* 2.b *)
fun card_value(s, r) = 
    case r of
        Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num x => x;

(* 2.c *)
fun remove_card([], c, e) = raise e
    | remove_card(x::xs, c, e) = if x = c then xs else x::remove_card(xs, c, e)


(* 2.d *)
fun all_same_color([]) = true
    | all_same_color(_::[]) = true
    | all_same_color(c1::(c2::xs)) = if card_color(c1) <> card_color(c2) then false else all_same_color(c2::xs);

(* 2.e *)
fun sum_cards(cs) = 
    let
        fun sum_cards_tail_recursive([], sum) = sum
            | sum_cards_tail_recursive(c::xs, sum) = sum_cards_tail_recursive(xs, card_value(c) + sum) 
    in
        sum_cards_tail_recursive(cs, 0)
    end;

(* 2.f *)
fun score(cs, goal) = 
    let
        val sum = sum_cards(cs)
        val same_color = all_same_color(cs)
        val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if same_color then preliminary_score div 2 else preliminary_score
    end;
        
(* 2.g *)
fun officiate(cs, mvs, goal) = 
    let
        fun play(_, [], _, hand) = score(hand, goal)
            | play([], _, _, hand) = score(hand, goal)
            | play(c::cs, mv::mvs, goal, hand) = 
                case mv of
                    Discard d => play(c::cs, mvs, goal, remove_card(hand, d, IllegalMove))
                  | Draw => 
                        let 
                            val hand' = c :: hand
                            val hand_value = sum_cards(hand')
                        in
                            if hand_value > goal then score(hand', goal) else play(cs, mvs, goal, hand')
                        end
    in
        play(cs, mvs, goal, [])
    end;