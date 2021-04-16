(* Solution for assignment 1. Chao Wang Sun Nov 15 2020 *)

(* 1. is_older : 
	takes two dates and evaluates to true or false.
	It evaluates to true if the first date comes before the second date.
	If the two dates are the same, the result is false. *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2)
    then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)

(* 2. number_in_month : 
	takes a list of dates and a month (an int),
	returns how many dates in the list are in the given month. *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
	let val num = number_in_month ((tl dates), month)
	in
	    if (#2 (hd dates)) = month
	    then num + 1
	    else num
	end

(* 3. number_in_months : 
	takes a list of dates and a list of months (an int list),
	returns the number of dates in the list of dates that are in any of the months
	in the list of months.
	Assume the list of months has no number repeated. *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, (hd months)) + number_in_months (dates, (tl months))

(* 4. dates_in_month :
	takes a list of dates and a month (an int),
	returns a list holding the dates from the argument list of dates that are in the month.
	The dates in the returned list is in the order they were originally given. *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
	let val in_dates = dates_in_month ((tl dates), month)
	in
	    if (#2 (hd dates)) = month
	    then (hd dates) :: in_dates
	    else in_dates
	end

(* 5. dates_in_months :
	takes a list of dates and a list of months (an int list),
	returns a list holding the dates from the argument list of dates that are 
	in any of the months in the list of months.
	Assume the list of months has no number repeated. *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, (hd months)) @ dates_in_months (dates, (tl months))

(* 6. get_nth :
	takes a list of strings and an int n and returns the nth element
	of the list where the head of the list is 1st.
	Assume there are enough elements in the list *)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then (hd strings)
    else get_nth((tl strings), n-1)

(* 7. date_to_string :
	takes a date and returns a string of the form January 20, 2013. 
	Use a list holding 12 strings for months. *)
fun date_to_string (date : int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth (months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(* 8. number_before_reaching_sum :
	takes a positive int called sum and an int list containing positive numbers
	returns an int such that the first n elements of the list add to less than sum,
	but the first n+1 elements of the list add to sum or more.
	Assume the entire list sums to more than the passed in value. *)
fun number_before_reaching_sum (sum : int, nums : int list) =
    if (hd nums) >= sum
    then 0
    else 1 + number_before_reaching_sum(sum-(hd nums), (tl nums))

(* 9. what_month :
	takes a day of year (an int between 1 and 365),
	returns (an int) what month that day is in (1 for January, 2 for February, etc.).
	Use a list holding 12 integers for days in every month *)		       
fun what_month (days : int) =
    let val days_every_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(days, days_every_month) + 1
    end

(* 10. month_range :
	takes two days of the year day1 and day2,
	returns an int list [m1,m2,...,mn] where m1 is the month of day1, 
	m2 is the month of day1+1, ..., and mn is the month of day day2.
	NOTE: the result will have length day2 - day1 + 1 or length 0 if day1 > day2. *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* 11. oldest :
	takes a list of dates and evaluates to an (int*int*int) option.
	It evaluates to NONE if the list has no dates and 
	SOME d if the date d is the oldest date in the list. *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
	let fun oldest_nonempty (dates : (int*int*int) list) =
		if null (tl dates)
		then (hd dates)
		else 
		    let val tl_ans = oldest_nonempty(tl dates)
		    in
			if is_older((hd dates), tl_ans)
			then (hd dates)
			else tl_ans
		    end
	in
	    SOME (oldest_nonempty dates)
	end

(* 12. cumulative_sum :
   takes a list of numbers, returns a list of the partial sums of them. *)
fun cumulative_sum (numbers: int list) =
    let fun helper (numbers: int list, sum: int) =
	    if null numbers
	    then []
	    else ((hd numbers) + sum) :: helper((tl numbers), (hd numbers) + sum)
    in
	helper(numbers, 0)
    end
	    
(* Below is the challenge problem part *)

(* 13.0.1. in_list :
	takes an int and a list of int
	returns true if the int is in the int list *)
fun in_list (elem : int, elems : int list) =
    if null elems
    then false
    else if (hd elems) = elem
    then true
    else in_list(elem, (tl elems))

(* 13.0.2. remove_duplicates :
	takes a list of int, returns a list containing all the different ints
	of the given list. *)
fun remove_duplicates (elems : int list) =
    if null elems
    then []
    else if in_list((hd elems), (tl elems))
    then remove_duplicates((tl elems))
    else (hd elems) :: remove_duplicates((tl elems))

(* 13.1. number_in_months_challenge :
	takes a list of dates and a list of months (an int list),
	returns the number of dates in the list of dates that are in any of the months
	in the list of months.
	Duplicates in months are counted only once. *)
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

(* 13.2. dates_in_months_challenge :
	takes a list of dates and a list of months (an int list),
	returns a list holding the dates from the argument list of dates that are 
	in any of the months in the list of months.
	A date should not be included twice even though there are two same months. *)
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

(* 14. reasonable_date :
	takes a date and returns true if it's a real date in the common era.
	Days possibly lost in the conversion to the Gregorian calendar in the Late 1500s are ignored. *)
fun reasonable_date (date : int*int*int) =
    let fun leap_year (year : int) = (year mod 400 = 0) orelse (not(year mod 100 = 0) andalso year mod 4 = 0)
	fun reasonable_year (year : int) = year > 0
	fun reasonable_month (month : int) = month > 0 andalso month <= 12
	fun reasonable_day (day : int) = day > 0
	fun get_nth (nums : int list, n : int) =
	    if n = 1
	    then hd nums
	    else get_nth(tl nums, n-1)
	val non_leap_year_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val leap_year_days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun reasonable_leap_date (date : int*int*int) =
	    reasonable_year (#1 date) andalso reasonable_month(#2 date) andalso reasonable_day(#3 date)
	    andalso leap_year(#1 date) andalso (#3 date) <= get_nth(leap_year_days, (#2 date))
	fun reasonable_nonleap_date (date : int*int*int) = 
	    reasonable_year (#1 date) andalso reasonable_month(#2 date) andalso reasonable_day(#3 date)
	    andalso not(leap_year(#1 date)) andalso (#3 date) <= get_nth(non_leap_year_days, (#2 date))
    in
	reasonable_leap_date(date) orelse reasonable_nonleap_date(date)
    end
