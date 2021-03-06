(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)
(* 1. make_silly_json :
   takes an int i and returns a json. *)
fun make_silly_json x =
    let fun make_json x = Object [("n", (Num (int_to_real x))), ("b", True)]
	fun make_silly_json_return_list x =
	    if x = 0
	    then []
	    else (make_json x)::(make_silly_json_return_list (x-1))
    in
	Array (make_silly_json_return_list x)
    end

(* 2. assoc :
   takes k and xs, return SOME v1 if (k1, v1) is the paire in the list for which
   k == k1 *)
fun assoc (k, xs) =
    case xs of
	[] => NONE
      | (k1,v1)::xs' => if k1 = k
			then SOME v1
			else assoc (k, xs')

(* 3. dot :
   takes a json and a string, returns a json option *)
fun dot (j, f) =
    case j of
	Object xs => assoc (f, xs)
      | _ => NONE

(* 4. one_fields :
   takes a json and returns a string list. *)
fun one_fields j =
    let fun helper (xs, acc) =
	    case xs of
		[] => acc
	      | (k,v)::xs' => helper (xs', k::acc)
    in
	case j of
	    Object xs => helper (xs, [])
	  | _ => []
    end

(* 5. no_repeats :
   takes a string list and returns a bool that is true if and only if no
   string appears more than once in the input. *)
fun no_repeats xs = (length xs) = (length (dedup xs))

(* 6. recursive_no_field_repeats :
   takes a json and returns true if and only if no object anywhere 'inside'
   (arbitrarily nested) the json argument has repeated field names. *)
fun recursive_no_field_repeats j =
    let fun recursive_no_field_repeats_list js =
	    case js of
		[] => true
	      | j::js' => (recursive_no_field_repeats j) andalso (recursive_no_field_repeats_list js')
	fun recursive_no_field_repeats_object xs =
	    case xs of
		[] => true
	      | (_, v)::xs' => (recursive_no_field_repeats v) andalso (recursive_no_field_repeats_object xs')
    in
	case j of
	    Array js => recursive_no_field_repeats_list js
	  | Object xs => (no_repeats (one_fields j)) andalso recursive_no_field_repeats_object xs
	  | _ => true
    end

(* 7. count_occurrences : *)
(* use asscending order *)
fun count_occurrences (xs, exn) =
    let fun helper (xs, x, cnt, acc) =
	    case xs of
		[] => (x, cnt)::acc
	      | x'::xs' => case strcmp (x', x) of
			       LESS => raise exn
			     | EQUAL => helper(xs', x, cnt+1, acc)
			     | GREATER => helper(xs', x', 1, (x, cnt)::acc)
    in
	case xs of
	    [] => []
	  | x::xs' => helper(xs', x, 1, [])
    end
					      
(* 8. string_values_for_field : *)
fun string_values_for_field (str, js) =
    case js of
	[] => []
      | j::js' => case dot (j, str) of
		      SOME (String s) => s::string_values_for_field (str, js')
		    | _ => string_values_for_field (str, js')

(* 9. filter_field_value : *)
fun filter_field_value (name, content, js) =
    let fun helper (js) =
	    case js of
		[] => []
	      | j::js' => case dot (j, name) of
			      SOME (String s) => (case strcmp (s, content) of
						     EQUAL => j::helper(js')
						   | _ => helper(js'))
			    | _ => helper(js')
    in
	helper(js)
    end
	
(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
    histogram (string_values_for_field (f, js))

(**** PUT PROBLEMS 9-11 HERE ****)
;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

val large_event_clearance_description_histogram = histogram_for_field ("event_clearance_description", large_incident_reports_list)
val large_hundred_block_location_histogram = histogram_for_field ("hundred_block_location", large_incident_reports_list)
								 
(**** PUT PROBLEMS 12-15 HERE ****)
val forty_third_and_the_ave_reports = filter_field_value ("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)
val forty_third_and_the_ave_event_clearance_description_histogram = histogram_for_field ("event_clearance_description", forty_third_and_the_ave_reports)
val nineteenth_and_forty_fifth_reports = filter_field_value ("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)
val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field ("event_clearance_description", nineteenth_and_forty_fifth_reports)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)
(* 16 : concat_with : 
   takes a separator string and a list of strings, returns concat string.
*)
fun concat_with (sstr, strs) =
    let fun helper (strs, acc) =
	    case strs of
		[] => acc
	      | s::strs' => helper (strs', acc ^ sstr ^ s)
    in
	case strs of
	    [] => ""
	  | s::strs' => helper (strs', s)
    end

(* 17 : quote_string :
   takes a string and returns a string *)
fun quote_string (s) = "\"" ^ s ^ "\""

(* 18 : real_to_string_for_json :
   takes a real and return a string. *)
fun real_to_string_for_json (r) =
    if real_is_negative r
    then "-" ^ real_to_string (real_abs r)
    else real_to_string r

(* 19 : json_to_string : *)
fun json_to_string j =
    let fun helper1 js =
	    case js of
		[] => []
	      | j::js' => (json_to_string j) :: (helper1 js')
	fun helper2 xs =
	    case xs of
		[] => []
	      | (k,v)::xs' => ((quote_string k) ^ " : " ^ (json_to_string v)) :: (helper2 xs')
    in
	case j of
	    Num x => real_to_string_for_json x
	  | String s => quote_string (s)
	  | False => "false"
	  | True => "true"
	  | Null => "null"
	  | Array js => "[" ^ concat_with(", ", (helper1 js)) ^ "]"
	  | Object xs => "{" ^ concat_with(", ", (helper2 xs)) ^ "}"
    end
									
(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

