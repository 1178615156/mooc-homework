% Beauty Trap
% Perform the most seductive dance


int: maxlen;
set of int: STEP = 1..maxlen;
array[LEGS] of int: maxlegs;
array[ARMS] of int: maxarms;
array[FACE] of int: maxface;
constraint assert(maxlegs[stand] >= maxlen, "maxlegs[stand] smaller than maxlen");
constraint assert(maxarms[neutral] >= maxlen, "maxarms[neutral] smaller than maxlen");
constraint assert(maxface[blank] >= maxlen, "maxface[blank] smaller than maxlen");
array[LEGS,ARMS] of int: dance_value;
array[ARMS,FACE] of int: entice_value;
constraint assert(dance_value[stand,neutral] = 0, "incorrect dance_value array");
constraint assert(entice_value[neutral,blank] = 0, "incorrect entice_value array");
int: boredom; % how bored each step make the viewer

var STEP: len;
array[STEP] of var LEGS: legs;
array[STEP] of var ARMS: arms;
array[STEP] of var FACE: face;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constraint forall(ele in LEGS)(sum(i in legs)(i = ele) <= maxlegs[ele]);
constraint forall(ele in ARMS)(sum(i in arms)(i = ele) <= maxarms[ele]);
constraint forall(ele in FACE)(sum(i in face)(i = ele) <= maxface[ele]);
constraint forall(i in len+1..maxlen)(legs[i]=stand);
constraint forall(i in len+1..maxlen)(arms[i]=neutral);
constraint forall(i in len+1..maxlen)(face[i]=blank);
constraint len < maxlen;
enum LEGS = {spin, leap, waltz, curtsey, prep, stand};
enum ARMS = {beckon, out, up, wrapped, neutral};
enum FACE = {smile, wink, batt, think, glow, blank};

%%% legs
constraint forall(i in 1..len)(legs[i]=leap ->
                    legs[i-1]=prep);
constraint forall(i in 1..len)(legs[i]=spin ->
                    exists(e in [curtsey, prep, stand])(e=legs[i+1]));
constraint forall(i in 1..len)(legs[i]=leap ->
                    exists(e in [spin,waltz,stand])(e=legs[i+1]));
constraint sum(i in legs)(i = waltz) <= 3;
constraint forall(i in 1..len)
                 (legs[i]=curtsey -> legs[i+1] !=prep);
constraint forall(i,j in 1..len where i < j )
                 (legs[i] = waltz /\ legs[j]=curtsey ->
                    exists(e in i..j)(legs[e]=stand));
%%% arms
constraint forall(i in 1..len)
                 (arms[i]=beckon ->
                 exists(e in [ up, out])(e=arms[i-1]));
constraint forall(i in 1..len)
                 (arms[i]=wrapped -> 
                 exists(e in [beckon, wrapped, neutral])(e=arms[i-1]));                 
constraint forall(i in 1..len-1)
                 (arms[i]!=neutral -> arms[i]!=arms[i+1]);

%%% face 
constraint forall(i in 1..len-1)
                 (face[i]!=blank -> face[i]!=face[i+1]);
constraint forall(i in 2..len)
                  (face[i]=think -> face[i-1] !=smile /\ face[i-1] != batt);

constraint forall(i in 1..len)(dance_value[legs[i],arms[i]] >= 0 /\
                               entice_value[arms[i],face[i]]>= 0);

var int : obj ;
constraint obj = sum(i in 1..len)
                    (dance_value[legs[i],arms[i]] + 
                    entice_value[arms[i],face[i]] )
                  - boredom * len;
solve maximize obj ;





















