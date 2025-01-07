
fun assert_true(condition : bool) = if not condition then raise Fail "error" else ();

(* is_older 
    takes two dates and evaluates to true or false. It evaluates to true if
    the first argument is a date that comes before the second argument. 
    If the two dates are the same, the result is false *)

assert_true(is_older((2022,2,3), (2024,2,1)) = true);
assert_true(is_older((2021,2,3), (2021,2,3)) = false);
assert_true(is_older((2021,1,3), (2022,1,5)) = true);
assert_true(is_older((2025,1,3), (2022,1,5)) = false);

(* number_in_month: 
    takes a list of dates and a month (i.e., an int) and returns
    how many dates in the list are in the given month *)
assert_true(number_in_month([(1992, 2, 3), (2023, 4, 3)], 2) = 1);
assert_true(number_in_month([(1992, 2, 3), (2023, 4, 3)], 7) = 0);
assert_true(number_in_month([(1992, 2, 3)], 2) = 1);
assert_true(number_in_month([(1992, 9, 3)], 3) = 0);
assert_true(number_in_month([], 3) = 0);

(* number_in_months: 
    takes a list of dates and a list of months (i.e., an int list)
    and returns the number of dates in the list of dates that are in 
    any of the months in the list of months.
    Assume the list of months has no number repeated. 
    Hint: Use your answer to the previous problem. *)

assert_true(number_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], [1, 2]) = 3);
assert_true(number_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 3, 2)], [1, 2]) = 2);
assert_true(number_in_months([(1992, 1, 2), (1991, 1, 2)], [1, 2]) = 2);
assert_true(number_in_months([(1992, 1, 2), (1991, 1, 2)], [4, 2]) = 0);
assert_true(number_in_months([(1992, 1, 2), (1991, 1, 2)], [1]) = 2);
assert_true(number_in_months([], [1, 2]) = 0);
assert_true(number_in_months([], []) = 0);

(* dates_in_month: 
    takes a list of dates and a month (i.e., an int) and returns a
    list holding the dates from the argument list of dates that are in the month. 
    The returned list should contain dates in the order they were originally given *)

assert_true(dates_in_month([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], 2) = [(1990, 2, 2)]);
assert_true(dates_in_month([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], 1) = [(1992, 1, 2), (1991, 1, 2)]);
assert_true(dates_in_month([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], 12) = []);

(* dates_in_months
    takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates 
    that are in any of the months in the list of months. 
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem and SML’s list-append operator (@). *)

assert_true(dates_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], [2]) = [(1990, 2, 2)]);
assert_true(dates_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], [1]) = [(1992, 1, 2), (1991, 1, 2)]);
assert_true(dates_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], [1, 2]) = [(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)]);
assert_true(dates_in_months([(1992, 1, 2), (1991, 1, 2), (1990, 2, 2)], [3]) = []);

(* get_nth 
    takes a list of strings and an int n and returns the nth element of the
    list where the head of the list is 1st. 
    Do not worry about the case where the list has too few elements:
        your function may apply hd or tl to the empty list in this case, which is okay *)
assert_true(get_nth(["0", "1", "2"], 2) = "1");
assert_true(get_nth(["0", "1", "2"], 1) = "0");
assert_true(get_nth(["0", "1", "2"], 2) = "1");
    
(* date_to_string 
    takes a date and returns a string of the form January 20, 2013
    (for example). 
    Use the operator^ for concatenating strings and the library function Int.toString
    for converting an int to a string. For producing the month part, do not use a 
    bunch of conditionals.
    Instead, use a list holding 12 strings and your answer to the previous problem. 
    For consistency, put a comma following the day and use capitalized 
    English month names: January, February, March, April, May, June, July, 
    August, September, October, November, December *)

assert_true(date_to_string((1990, 2, 24)) = "February 24, 1990");
assert_true(date_to_string((1980, 12, 12)) = "December 12, 1980");
assert_true(date_to_string((1980, 1, 31)) = "January 31, 1980");

(* number_before_reaching_sum
    takes an int called sum, which you can assume is positive, 
    and an int list, which you can assume contains all positive numbers, 
    and returns an int.
    You should return an int n such that the first n elements of the 
    list add to less than sum, but the first n + 1 elements of the list 
    add to sum or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case *)

assert_true(number_before_reaching_sum(23, [21, 24, 3, 4, 5, 6, 7]) = 1);
assert_true(number_before_reaching_sum(23, [1, 2, 3, 4, 5, 6, 7]) = 6);
assert_true(number_before_reaching_sum(23, [21, 1, 3, 4, 5, 6, 7]) = 2);
assert_true(number_before_reaching_sum(23, [21, 2, 3, 4, 5, 6, 7]) = 1);
assert_true(number_before_reaching_sum(23, [221, 2, 3, 4, 5, 6, 7]) = 0);

(* what_month
    takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.). 
    Use a list holding 12 integers and your answer to the previous problem *)
assert_true(what_month 1 = 1); 
assert_true(what_month(40) = 2);
assert_true(what_month(365) = 12);
assert_true(what_month(365-31) = 11);

(* month_range
    takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., 
    and mn is the month of day day2. 
    Note the result will have length day2 - day1 + 1 or length 0 if day1>day2 *)
assert_true(month_range(30, 33) = [1, 1, 2, 2]);
assert_true(month_range(366, 365) = []);
assert_true(month_range(365, 365) = [12]);
assert_true(month_range(364, 365) = [12, 12]);

fun count(numbers: int list) = if null numbers then 0 else 1 + count(tl numbers);

assert_true(count(month_range(3, 123)) = 123 - 3 + 1);
assert_true(count(month_range(30, 33)) = 33 - 30 + 1);


(* oldest 
    takes a list of dates and evaluates to an (int*int*int) option. 
    It evaluates to NONE if the list has no dates and SOME d 
    if the date d is the oldest date in the list. *)
assert_true(oldest([(1992, 2, 1), (1990, 3, 4)]) = SOME(1990, 3, 4));
assert_true(oldest([(1992, 2, 1), (1990, 3, 4), (2000, 2, 3)]) = SOME(1990, 3, 4));
assert_true(oldest([(1093, 2, 1), (1999, 3, 4), (2000, 2, 3)]) = SOME(1093, 2, 1));
assert_true(oldest([]) = NONE);

(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = is_older ((1,2,3),(2,3,4)) = true;
val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];
val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];
val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013";
val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;
val test9 = what_month 70 = 3;
val test10 = month_range (31, 34) = [1,2,2,2];
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);

assert_true(test1);
assert_true(test2);
assert_true(test3);
assert_true(test4);
assert_true(test5);
assert_true(test6);
assert_true(test7);
assert_true(test8);
assert_true(test9);
assert_true(test10);
assert_true(test11);