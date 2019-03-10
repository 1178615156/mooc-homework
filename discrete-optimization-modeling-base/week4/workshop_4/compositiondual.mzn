include "alldifferent.mzn";
int : n ;
array[1..n] of var 1..n : posn ;
constraint alldifferent(posn);
constraint abs(posn[1]-posn[n]) = 1;

solve satisfy;