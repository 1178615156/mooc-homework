int: n; % number of court members
set of int: COURT = 1..n;
int: emperor = 1;
array[COURT] of int: rank;
array[COURT] of int: ability;

int: m; % number of horses
set of int: HORSE = 1..m;
array[HORSE] of int: beauty;
array[HORSE] of int: speed;

array[COURT,HORSE] of int: enjoy;

array[COURT] of var 0..m : horse;
include "globals.mzn";
include "alldifferent.mzn";

constraint forall(i in COURT)(
  if horse[i] > 0 then enjoy[i,horse[i]] >= 0 else 1=1 endif
);
constraint alldifferent_except_0(horse);

set of int: ALL_COURT = 2..n;
constraint forall(i in ALL_COURT)(
  enjoy[emperor,horse[emperor]] >
    if horse[i] = 0 
    then 0 
    else enjoy[i,horse[i]] 
    endif);

constraint if n > m 
  then sum(i in horse where i > 0)(1) = m 
  else sum(i in horse where i > 0)(1) = n
  endif;
  

constraint 
    forall(i in COURT)(
    forall(j in COURT where rank[i] > rank[j])(
      (horse[i]=0 /\ horse[j] =0) \/
      (horse[i]>0 /\ horse[j] =0) \/
      (beauty[horse[i]] >= beauty[horse[j]] )
    )
  );

var int : speed_bool = 
  sum(i in COURT)(
    sum(j in COURT)(
      speed[horse[i]] > speed[horse[j]] /\ ability[i] < ability[j]
    )
  );



var int : obj;
constraint obj = sum(i in COURT)(
  if horse[i] > 0 then enjoy[i,horse[i]] else 0 endif
) + speed_bool * -100;
solve maximize obj;

output [
 "horse = \(horse);\n"
,"obj = \(obj);\n"
% ,"speed_bool = \(speed_bool)"
];
