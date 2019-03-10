array[int] of int: readings;

int: start;
array[1..(length(readings)+1)] of var int : temp ;
constraint temp[1] == start ; 
constraint forall(i in 1..length(readings))(temp[i+1]=(readings[i] + temp[i]) div 2 );
solve satisfy;
