(* date yy, mm, dd: int * int * int *)

fun get_year(one_date: int * int * int) = #1 one_date;

fun get_month(one_date: int * int * int) = #2 one_date;

fun get_day(one_date: int * int * int) = #3 one_date;

(* is_older 
    takes two dates and evaluates to true or false. It evaluates to true if
    the first argument is a date that comes before the second argument. 
    If the two dates are the same, the result is false *)
fun is_older(one_date: int * int * int, other_date: int * int * int) =
    get_year one_date < get_year other_date orelse
    (get_year one_date = get_year other_date andalso
        (get_month one_date < get_month other_date orelse
            (get_month one_date = get_month other_date andalso
                get_day one_date < get_day other_date
            )
        )
    );


(* number_in_month: 
    takes a list of dates and a month (i.e., an int) and returns
    how many dates in the list are in the given month *)
fun number_in_month(dates: (int * int * int) list, month: int) =
    if null dates then
        0
    else
        let 
            fun same_month(date: int * int * int, month: int) =
                if get_month(date) = month then
                    1
                else
                    0 
        in
            same_month(hd dates, month) + number_in_month(tl dates, month)
        end;

(* number_in_months: 
    takes a list of dates and a list of months (i.e., an int list)
    and returns the number of dates in the list of dates that are in 
    any of the months in the list of months.
    Assume the list of months has no number repeated. 
    Hint: Use your answer to the previous problem. *)
fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null months then
       0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months);

(* dates_in_month: 
    takes a list of dates and a month (i.e., an int) and returns a
    list holding the dates from the argument list of dates that are in the month. 
    The returned list should contain dates in the order they were originally given *)
fun dates_in_month(dates: (int * int * int) list, month: int) =
    if null dates then
        []
    else
        let 
            val remaining = dates_in_month(tl dates, month)
        in
            if get_month(hd dates) <> month then
                remaining
            else
                hd dates :: remaining
        end;

(* dates_in_months
    takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates 
    that are in any of the months in the list of months. 
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem and SML’s list-append operator (@). *)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null months then
        []
    else
        let
            val dates_per_month = dates_in_month(dates, hd months)
            val remaining = dates_in_months(dates, tl months)
        in
            if null dates_per_month then
                remaining
            else
                dates_per_month @ remaining
        end;

(* get_nth 
    takes a list of strings and an int n and returns the nth element of the
    list where the head of the list is 1st. 
    Do not worry about the case where the list has too few elements:
        your function may apply hd or tl to the empty list in this case, which is okay *)
fun get_nth(words: string list, n: int) = 
    let
        val element = hd words
    in
        if n <= 1 then
            element
        else
            get_nth(tl words, n - 1)
    end;

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
val MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date: int * int * int) =
    let
        val day = Int.toString(get_day date)
        val month = get_nth(MONTHS, get_month date)
        val year = Int.toString(get_year date)
    in
        month ^ " " ^ day ^ ", " ^ year
    end;

(* number_before_reaching_sum
    takes an int called sum, which you can assume is positive, 
    and an int list, which you can assume contains all positive numbers, 
    and returns an int.
    You should return an int n such that the first n elements of the 
    list add to less than sum, but the first n + 1 elements of the list 
    add to sum or more. Assume the entire list sums to more than the passed in
    value; it is okay for an exception to occur if this is not the case *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
    if null numbers then
        0
    else
        let
            val first = hd numbers
        in
            if first < sum then
                1 + number_before_reaching_sum(sum - first, tl numbers)
            else
                0
        end;

(* what_month
    takes a day of year (i.e., an int between 1 and 365) and returns
    what month that day is in (1 for January, 2 for February, etc.). 
    Use a list holding 12 integers and your answer to the previous problem *)
val DAYS_IN_MONTH = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

fun what_month(day_of_year: int) =
    1 + number_before_reaching_sum(day_of_year, DAYS_IN_MONTH);

(* month_range
    takes two days of the year day1 and day2 and returns an int list
    [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., 
    and mn is the month of day day2. 
    Note the result will have length day2 - day1 + 1 or length 0 if day1>day2 *)
fun month_range(day1: int, day2: int) =
    if day1 > day2 then
        []
    else
        let
            val month = what_month(day1)
        in
            month :: month_range(day1 + 1, day2)
        end;

(* oldest 
    takes a list of dates and evaluates to an (int*int*int) option. 
    It evaluates to NONE if the list has no dates and SOME d 
    if the date d is the oldest date in the list. *)
fun oldest(dates: (int * int * int) list) =
    if null dates then
        NONE
    else
        let
            val first = hd dates
            val second = oldest(tl dates)
        in
            if isSome(second) andalso is_older(valOf(second), first) then
                second
            else
                SOME first
        end;


fun in_list(number: int, numbers: int list) =
    if null numbers then
        false
    else
        if hd numbers = number then
            true
        else
            in_list(number, tl numbers)


fun remove_duplicates(numbers: int list) =
    if null numbers then
        []
    else
        let
            val first = hd numbers
            val is_unique = not (in_list(first, tl numbers))
            val others = remove_duplicates(tl numbers)
        in
            if is_unique then
                first :: others
            else
                others
        end;


remove_duplicates([1,2,3,4,2,3,2,1,4]) = [3,2,1,4];
remove_duplicates([5,2,3,4,2,3,2,1,4]) = [5,3,2,1,4];

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
    number_in_months(dates, remove_duplicates(months));


fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
    dates_in_months(dates, remove_duplicates(months));


fun leap_year(year: int) =
    let
        val normal = (year mod 4 = 0)
        val except = (year mod 100 = 0)
        val unless = (year mod 400 = 0)
    in
        normal andalso (not except orelse unless)
    end;


fun get_nth_int(numbers: int list, nth: int) = 
    if nth = 1 then
        hd numbers
    else
        get_nth_int(tl numbers, nth - 1);


fun reasonable_date(date: int * int * int) =
    let
        val year = get_year(date)
        val month = get_month(date)
        val day = get_day(date)
        val is_leap_year = leap_year(year)
    in
        if year <= 0 orelse month <= 0 orelse month > 12 then 
            false
        else
            let 
                val days = get_nth_int(DAYS_IN_MONTH, month)
                val days_in_month = if is_leap_year andalso month = 2 then days + 1 else days
            in
                day <= days_in_month
            end
    end;


reasonable_date(1992, 13, 2) = false;
reasonable_date(1992, 1, 2) = true;
reasonable_date(1992, 31, 21) = false;
reasonable_date(1992, 2, 31) = false;
reasonable_date(1992, 2, 29) = true;
