(* CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)
(* 1. only_capitals :
   takes a string list, returns strings starts with an uppercase letter *)
fun only_capitals strs = List.filter (fn x => Char.isUpper(String.sub(x, 0))) strs

(* 2. longest_string1 :
   takes a string list, returns the longest string in the list 
   the first one *)
fun longest_string1 strs =
    List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" strs

(* 3. longest_string2 :
   takes a string list, returns the longest string in the list
   the last one *)
fun longest_string2 strs =
    List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" strs

(* 4.1. longest_string_helper :
   looks a lot like longest_string1 but takes a function as argument *)
fun longest_string_helper f =
    fn strs =>
       List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" strs

(* 4.2. longest_string3 :
   same as longest_string1 but use longest_string_helper and var binding *)
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

(* 4.3. longest_string4 :
   same as longest_string2 but use longest_string_helper and var binding *)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5. longest_capitalized :
   takes a string list and returns the longest string in the list that
   begins with an uppercase letter, or "" if there are no such things. *)
val longest_capitalized = longest_string1 o only_capitals

(* 6. rev_string :
   takes a string and returns the string that is the same characters in
reverse order *)
val rev_string = String.implode o List.rev o String.explode

(* 7. first_answer :
   type ('a -> 'b option) -> 'a list -> 'b
   if f a return some v, fistr_answer returns v
   otherwise raise NoAnswer *)
fun first_answer f =
    fn xs => case xs of
		 [] => raise NoAnswer
	       | x::xs' => case f(x) of
			       NONE => first_answer f xs'
			     | SOME v => v

(* 8. all_answers :
   type ('a -> 'b list option) -> 'a list -> 'b list option
   same as first_answer except returning all answers *)
fun all_answers f =
    fn xs => let fun helper (acc, xs) =
		     case xs of
			 [] => SOME acc
		       | x::xs' => case f(x) of
				       NONE => NONE
				     | SOME l => helper(acc@l, xs')
	     in
		 helper([], xs)
	     end

(* 9.a. count_wildcards :
   returns how many Wildcard patterns a pattern contains *)
val count_wildcards = g (fn() => 1) (fn x => 0)

(* 9.b. count_wild_and_variable_lengths :
   returns how many Wildcard pattern plus the sum of the string lengths of
   all variables a pattern contains *)
val count_wild_and_variable_lengths = g (fn() => 1) (fn x => String.size x)

(* 9.c. count_some_var :
   takes a string and a pattern (as a pair), returns the number of times
   the string appears as a variable in the pattern *)
fun count_some_var (str, p) = g (fn() => 0) (fn x => if x = str then 1 else 0) p

(* 10. check_pat :
   takes a pattern and returns true if and only if all the variables appearing
   in the pattern are distinct from each other. *)
fun check_pat p =
    let
	fun all_pat p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, names) => all_pat(p) @ names) [] ps
	      | ConstructorP(_, p) => all_pat p
	      | _ => []
	fun exist_repeats names =
	    case names of
		[] => false
	      | name::names' => if List.exists (fn x => x = name) names'
				then true
				else exist_repeats names'
    in
	not (exist_repeats(all_pat(p)))
    end

(* 11. match :
   takes a valu * pattern and returns a (string * valu) list option *)
fun match (v, p) =
    case (p, v) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s, v)]
      | (UnitP, Unit) => SOME []
      | (ConstP i, Const j) => if i = j then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (ConstructorP(s1, p), Constructor(s2, v)) => if s1 = s2
						     then match (v, p)
						     else NONE
      | _ => NONE

(* 12. first_match :
   takes a value and a list of patterns and returns a (string * valu) list option *)
fun first_match v ps =
    SOME(first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE

(* 13. typecheck_patterns : challenge problem
   to hard for me; find a solution as followed *)
(* From Anything (acc), get a type of a pattern and narrow this two type to one most
   lenient type (new acc). Do it until there is no more patterns. *)
(* And this problem is not fully detailed to understand, search "fun typecheck_pattern"
   with google to get more description *)
fun typecheck_patterns(ds, ps) =
    let
        exception NarrowFailed;
        exception NoConstructor;
        fun narrow(t0, t1) =
            case (t0, t1) of
                (Anything, _) => t1
              | (_, Anything) => t0
              | (UnitT, UnitT) => UnitT
              | (IntT, IntT) => IntT
              | (Datatype s0, Datatype s1) => if s0 = s1 then Datatype s0 else raise NarrowFailed
              | (TupleT t0s, TupleT t1s) => (TupleT(map (fn(t0, t1) => narrow(t0, t1)) (ListPair.zipEq(t0s, t1s))) handle UnequalLenths => raise NarrowFailed) 
              | _ => raise NarrowFailed;
        fun get_typ p = case p of
                            Wildcard => Anything
                          | Variable _ => Anything
                          | UnitP => UnitT
                          | ConstP _ => IntT
                          | TupleP ps' => TupleT(map (fn p => get_typ p) ps')
                          | ConstructorP(c, p) =>
                            (case (List.find (fn (c', _, _) => c' = c) ds) of
                                 NONE => raise NoConstructor
                               | SOME(_, d, t) => let val _ = narrow(get_typ p, t) in Datatype d end)
        val get_ans = List.foldl (fn(p, acc) => narrow(get_typ p, acc)) Anything
    in
        SOME(get_ans ps) handle NarrowFailed => let val _ = print "Narrow Failed!\n" in NONE end
                          | NoConstructor => let val _ = print "No Such Constructor!\n" in NONE end
    end
