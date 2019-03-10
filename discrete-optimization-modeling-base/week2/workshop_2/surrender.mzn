% workshop 2
% select a set of trade negotiation party
enum NEGOTIATOR;
NEGOTIATOR: dummy; 

int: l; % minimum party size
int: u; % maximum party size
int: m; % minimum joint ability

array[NEGOTIATOR] of int: honor;
array[NEGOTIATOR,NEGOTIATOR] of int: joint;

var set of NEGOTIATOR : party;
constraint card(party) >= l ;
constraint card(party) <= u ;

var int: total_ability = sum(i in party,j in party where i != j)(joint[i,j]) div 2 ;
constraint total_ability >=m ;
var int :obj;
constraint forall(i in party)(obj <= honor[i]);
solve maximize obj;

output ["party = \(party)\n",
        "obj = \(obj)"]