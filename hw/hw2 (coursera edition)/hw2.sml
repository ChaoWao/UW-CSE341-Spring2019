(* Solution for assignment 2. Chao Wang Sun Nov 22 2020 *)
(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a. all_except_option :
   takes a string and a string list, returns NONE if the string is not in
   the list, else return SOME lst whre lst is identical to the argument
   list except the string is not in it. 
   ASSUME the string is in the list at most once. *)
fun all_except_option (str, strs) =
    case strs of
	[] => NONE
      | s::strs' => if same_string(s, str)
		    then SOME strs'
		    else
			case all_except_option (str, strs') of
			    NONE => NONE
			  | SOME ss => SOME(s::ss)

(* 1.b. get_substitutions1 :
   takes a string list list (substitutions) and a string s, returns a string list.
   The result has all the strings that are in some list in substitutions that also
   has s, but s itself should not be in the result. *)
fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
      | sub::subs' => case all_except_option(s, sub) of
			  NONE => get_substitutions1(subs', s)
			| SOME ss => ss @ get_substitutions1(subs', s)

(* 1.c. get_substitutions2 :
   same as get_substitutions1 but it use a tail-recursive local helper function *)
fun get_substitutions2 (subs, s) =
    let fun aux (subs, acc) =
	    case subs of
		[] => acc
	      | sub::subs' => case all_except_option(s, sub) of
				  NONE => aux(subs', acc)
				| SOME ss => aux(subs', acc@ss)
    in
	aux(subs, [])
    end

(* 1.d. similar_names :
   takes a string list list (substitutions) and a full name (string list)
   substitute the first name and return all similar names *)
fun similar_names (subs, name) =
    let val {first=x, middle=y, last=z} = name
	val similar = get_substitutions1(subs, x)
	fun aux (similar, acc) =
	    case similar of
		[] => acc
	      | s::similar' => aux(similar', acc@[{first=s, middle=y, last=z}])
    in
	aux(similar, [name])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2.a. card_color :
   takes a card and returns its color *)
fun card_color (s, _) =
    case s of
	Clubs => Black
      | Spades => Black
      | _ => Red

(* 2.b. card_value :
   takes a card and returns its value *)
fun card_value (_, r) =
    case r of
	Num i => i
      | Ace => 11
      | _ => 10

(* 2.c. remove_card :
   takes a list of cards cs, a card c, and an exception e
   returns a list than has all the elements of cs except c
   If c is in the list more than once, remove the first
   Else raise the exception e *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | c'::cs' => if c' = c then cs' else c'::remove_card(cs', c, e)
							  
(* 2.d. all_same_color :
   takes a list of cards and returns true if all the cards are
   the same color *)
fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true
      | head::(neck::rest) => card_color(head) = card_color(neck)

(* 2.e. sum_cards :
   takes a list of cards and returns the sum of their values.
   Use tail recursive *)
fun sum_cards cs =
    let fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => aux(cs', card_value(c) + acc)
    in
	aux(cs, 0)
    end

(* 2.f. score :
   takes a card list (held-cards) and an int (goal)
   returns score as described above *)
fun score (hcs, goal) =
    let val sum = sum_cards hcs
	val pre_score = if sum > goal then 3*(sum-goal) else goal-sum
    in
	if all_same_color(hcs)
	then pre_score div 2
	else pre_score
    end
	
(* 2.g. officiate :
   runs a game, takes a card-list and a move list and the goal
   returns the score at the end of the game after all the move. *)
fun officiate (cl, ml, goal) =
    let fun run (cl, ml, hl) =
	    case ml of
		[] => hl
	      | (Discard c)::ml' => run(cl, ml', remove_card(hl, c, IllegalMove))
	      | (Draw)::ml' => case cl of
				   [] => hl
				 | c::cl' => if sum_cards(c::hl) >= goal
					     then c::hl
					     else run(cl', ml', c::hl)
    in
	score(run(cl, ml, []), goal)
    end

(* 3.a.1. score_challenge :
   same as fun score but ace can be 1 or 11 *)
fun score_challenge (cs, goal) =
    let fun raw_score x = if x > goal then 3*(x-goal) else goal-x
	fun min_pre_score (cs, acc) =
	    case cs of
		[] => raw_score acc
	      | (s, r)::cs' => case r of
				   Ace => Int.min(min_pre_score(cs', acc+1), min_pre_score(cs', acc+11))
				 | _ => min_pre_score(cs', acc+card_value(s,r))
	val pre_score = min_pre_score(cs, 0)
    in
	if all_same_color(cs)
	then pre_score div 2
	else pre_score
    end

(* 3.a.2. officiate_challenge :
   same as fun officiate but ace can be 1 or 11 *)
fun officiate_challenge (cl, ml, goal) =
    let fun no_sum (cs, acc) =
	    case cs of
		[] => acc > goal
	      | (s, r)::cs' => case r of
			      Ace => no_sum(cs', acc+1)
			    | _ => no_sum(cs', acc+card_value(s,r))
	fun run (cl, ml, hl) =
	    case ml of
		[] => hl
	      | (Discard c)::ml' => run(cl, ml', remove_card(hl, c, IllegalMove))
	      | (Draw)::ml' => case cl of
				   [] => hl
				 | c::cl' => if no_sum(c::hl, 0)
					     then c::hl
					     else run(cl', ml', c::hl)
    in
	score_challenge(run(cl, ml, []), goal)
    end

(* 3.b. careful_player :
   takes a card list and a goal, returns a move list *)
fun careful_player (cl, goal) =
    let fun can_draw (hl, c) = sum_cards hl <= goal
	fun must_draw (hl) = goal - 10 > sum_cards hl
	fun stop_move (hl) = score(hl, goal) = 0
	fun must_discard (hl, c) =
	    let fun discard (cannot, may, c) =
		    case may of
			[] => NONE
		      | c'::may' => if score(c::cannot@may', goal) = 0
				    then SOME c'
				    else discard (c'::cannot, may', c)
	    in
		discard ([], hl, c)
	    end
	fun try_move (cl, hl) =
	    case cl of
		[] => if must_draw(hl) then [Draw] else []
	      | c::cl' => if stop_move(hl)
			  then []
			  else if must_draw(hl)
			  then Draw::try_move(cl', c::hl)
			  else case must_discard(hl, c) of
				   NONE => []
				 | SOME c' => [Discard c', Draw]
    in
	try_move (cl, [])
    end
