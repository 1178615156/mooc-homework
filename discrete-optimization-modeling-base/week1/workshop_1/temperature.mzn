array[int] of int: readings;
int : start;
int : len = length(readings) ; 

array[1..(len + 1)] of var int : temp ;
constraint temp[1] == start;
 
array[1..(len + 1)] of var 25..30 : real_temp ;
constraint real_temp[1] = start;

constraint forall(i in 1..len)(temp[i+1] = (readings[i] + real_temp[i]) div 2 );
constraint forall(i in 1..len)(real_temp[i+1] = temp[i+1] + change_tr[choices[i]]);


enum OPT = {heat,strongly_heat,cool,strongly_cool,do_nothing};
array[OPT] of int : change_tr   = [1,4,-2,-5,0];
array[OPT] of int : change_cost = [1,5, 3, 9,0];
array[1..len] of var OPT : choices ; 


var int : total_cost = sum([ change_cost[choice] | choice in choices ]);
solve minimize total_cost;

output ["real_temp : \(real_temp)\n",
        "choice : \(choices)\n",
        "total_cost = \(total_cost)"]


 