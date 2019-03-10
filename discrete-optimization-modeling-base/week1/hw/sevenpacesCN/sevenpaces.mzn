var 1..9: S;
var 0..9: E;
var 1..9: V;
var 0..9: N;
var 1..9: P;
var 0..9: A;
var 0..9: C;
var 0..9: R;

var int : verse =(E + S * 10 + R * 100 + E * 1000 + V * 10000);
constraint 
    (N + E * 10 + V * 100 + E * 1000 + S * 10000)
  + (S + E * 10 + C * 100 + A * 1000 + P * 10000)
  = verse;

include "alldifferent.mzn";
constraint alldifferent([S,E,V,N,P,A,C,R]);
solve maximize verse;


