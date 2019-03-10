var 0..9: B;
var 0..9: E;
var 0..9: A;
var 0..9: N;
var 0..9: S;
var 0..9: C;
var 0..9: R;
var 0..9: Y;
var 0..9: I;
var 0..9: G;

include "alldifferent.mzn";

constraint 2 * (S + 10 * N + 100 * A + 1000 * E + 10000 *B)
          =     G + N * 10 + I * 100 + Y * 1000 + R * 10000 + C * 100000;
          
constraint alldifferent([B,E,A,N,S,C,R,Y,I,G]);

solve satisfy;