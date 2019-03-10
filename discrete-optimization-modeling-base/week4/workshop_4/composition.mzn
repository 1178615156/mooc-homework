int : n ;

array[1..n] of var 1..n : order ;
array[1..n] of var 1..n : posn ;
array[1..n-1] of var int :diff_value = [abs(order[i] - order[i+1]) |i in 1..n-1];
include "alldifferent.mzn";
include "inverse.mzn";
constraint inverse(order,posn);
constraint abs(posn[1]-posn[n]) = 1;
constraint alldifferent(order);
constraint alldifferent(diff_value);
output ["order : \(order)\n","diff_value : \(diff_value)\n","posn :\(posn)"];
solve satisfy;

