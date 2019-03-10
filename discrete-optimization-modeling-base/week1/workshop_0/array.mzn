array[1..n] of var 0..9 : x;
int : n;
constraint product(x) = sum(x);
solve satisfy;
