(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals(sl : string list) = 
	List.filter (fn s => Char.isUpper(String.sub(s,0))) sl;

(* 2 *)
fun longest_string1 (sl:string list) = 
	List.foldl (fn (l,r) => if (String.size(l) > String.size(r)) then l else r ) "" sl ;

(* 3 *)
fun longest_string2 (sl:string list) = 
	List.foldl (fn (l,r) => if (String.size(l) >= String.size(r)) then l else r ) "" sl ;

(* 4 - help *)
fun longest_string_helper f sl = 
	List.foldl (fn (l,r) => if f(String.size(l),String.size(r)) then l else r ) "" sl;

(* 4 - string3 *)
val longest_string3 = longest_string_helper (fn (l,r) =>l > r ) ;

(* 4 - string4 *)
val longest_string4 = longest_string_helper(fn (l,r) => l >= r );

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals;

(* 6 *)
val rev_string = String.implode o List.rev o String.explode ; 

(* 7 *)
fun first_answer f l = 
	case l of 
		[]				=> raise NoAnswer
		| head :: tail 	=> case f head of 
				SOME value  => value 
				|NONE       => first_answer f tail ;

(* 8 *)
fun all_answers f l = 
	let fun help (l,r) = case (f l,r) of 
			(SOME value,SOME acc_list) => SOME (acc_list @ value)
			| _ => NONE 
	in List.foldl help (SOME []) l
	end ;

(* 9 - a *)
val count_wildcards = g (fn () => 1) (fn a => 0);

(* 9 - b *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size ;

(* 9 - c *)
fun count_some_var (s:string,p) = g (fn () => 0) (fn ss => if ss = s then 1 else 0) p;

(* 10 *)
fun check_pat p = 
	let 
		fun get_all_variable_string p = case p of 
			Variable s 		  => [s]
			| ConstructorP (_ , pp) => get_all_variable_string(pp)
			| TupleP l 			  => List.foldl(fn (pp,acc) => get_all_variable_string(pp) @ acc ) [] l 
			| _ 				  => []
		fun all_different l = case l of 
			[] => true 
			| head :: tail =>( not (List.exists(fn s => s = head) tail)) andalso all_different(tail) ;
	in 
		all_different (get_all_variable_string (p))
	end ;
	
(* 11 *)
fun match (v:valu, p:pattern) = case (p,v) of 
	  (Wildcard,_) 			 => SOME []
	| (Variable s,v ) 		 => SOME [(s,v)]
	| (UnitP,Unit) 			 => SOME []
	| (ConstP e1,Const e2) 	 => if e1=e2 then SOME [] else NONE 
	| (TupleP ps , Tuple vs) => all_answers match (ListPair.zip(vs,ps))
	| (ConstructorP(s1,pp),Constructor(s2,vv)) =>
		if (s1 = s2) 
		then match (vv,pp)
		else NONE 
	| _ 					 => NONE ;

(* 12 *)
fun first_match v ps = 
	SOME (first_answer (fn p => match(v,p)) ps)
	handle NoAnswer => NONE ;