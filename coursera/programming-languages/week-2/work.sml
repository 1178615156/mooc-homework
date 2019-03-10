fun get_year ( date: (int * int * int))= #1 date ;
fun get_month (date: (int * int * int))= #2 date ;
fun get_day  ( date: (int * int * int))= #3 date ; 

fun count_countains_number(goal:int,data:int list)= 
    if null data 
    then 0 
    else (if hd(data) = goal then 1 else 0) + count_countains_number(goal, tl data); 

(* solutions *)

fun is_older(a : int * int * int , b : int * int * int ) =
    let 
        val diff_year  = #1 b - #1 a 
        val diff_month = #2 b - #2 a 
        val diff_day   = #3 b - #3 a 
    in 
        if diff_year > 0  then true
        else if diff_year = 0 andalso diff_month > 0 then true 
        else if diff_year = 0 andalso diff_month = 0 andalso diff_day > 0 then true 
        else false  
    end ;

fun number_in_months (date:( int * int * int )list , months : int list )= 
    if null date 
    then 0 
    else count_countains_number(get_month(hd(date)),months) + number_in_months(tl date , months);

fun number_in_month (date : (int * int * int ) list , goal_month : int ) = 
    number_in_months(date,[goal_month]);
    
fun dates_in_months(date : (int * int * int ) list , months : int list  ) = 
    if null date 
    then [] 
    else 
        if count_countains_number(get_month(hd date) , months) > 0 
        then hd(date) :: dates_in_months(tl date , months)
        else dates_in_months(tl date , months);

fun dates_in_month(date : (int * int * int ) list , month : int ) = 
    dates_in_months(date,[month]);

fun get_nth(data : string list , i : int ) = 
    if i = 1 
    then hd data
    else get_nth(tl data ,  i - 1 );

fun date_to_string(date : int * int * int ) = 
    let 
        val year = Int.toString(get_year(date))
        val month = if get_month(date) = 1 then "January"
                    else if get_month(date) = 2 then "February"
                    else if get_month(date) = 3 then "March"
                    else if get_month(date) = 4 then "April"
                    else if get_month(date) = 5 then "May"
                    else if get_month(date) = 6 then "June"
                    else if get_month(date) = 7 then "July"
                    else if get_month(date) = 8 then "August"
                    else if get_month(date) = 9 then "September"
                    else if get_month(date) = 10 then "October"
                    else if get_month(date) = 11 then "November"
                    else  "December"
        val day = Int.toString(get_day(date))
    in 
        month ^ " " ^ day ^ ", " ^ year
    end ; 

fun number_before_reaching_sum(sum:int , l : int list )= 
    if null l orelse hd(l) >= sum  
    then 0 
    else 1 + number_before_reaching_sum(sum - hd l , tl l) ;

fun what_month(i :int ) = 
         if i <=  31 then 1 
    else if i <=  59 then 2 
    else if i <=  90 then 3 
    else if i <= 120 then 4 
    else if i <= 151 then 5 
    else if i <= 181 then 6 
    else if i <= 212 then 7 
    else if i <= 243 then 8 
    else if i <= 273 then 9 
    else if i <= 304 then 10 
    else if i <= 334 then 11 
    else 12 ;

fun month_range(start : int , to : int ) = 
    if start > to
    then [] 
    else what_month(start) :: month_range(start + 1 , to);

fun oldest(dates : (int * int * int) list ) = 
    let 
        fun max(a:(int*int*int),b:(int*int*int))= 
            if is_older(a,b) then a else b 
        fun max_list(l : (int * int * int ) list ) = 
            if null (tl (l))
            then hd l 
            else max(hd l , max_list(tl l ))
    in 
        if null dates 
        then NONE 
        else SOME ( max_list (dates))
    end ; 
