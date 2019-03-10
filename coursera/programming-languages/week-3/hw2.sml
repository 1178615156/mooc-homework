
(* help fun *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
fun reverse(l : 'a list ,result)= case l of 
    [] => result 
    |head :: tail => reverse(tail,head :: result)


(* put your solutions for problem 1 here *)
(* 1 *)
fun all_except_option (s:string ,l : string list ) = 
    case l of 
        []           => NONE 
       |head :: tail => case all_except_option(s,tail) of 
                 NONE    => if same_string(head,s) then SOME(tail) else NONE  
                |SOME(e) => if same_string(head,s) then SOME(e)    else SOME(head :: e) ;

(* 2 *)
fun get_substitutions1(l : string list list , s : string ) = 
    case l of 
         []           => []
        |head :: tail => case all_except_option(s,head) of 
                  NONE   =>   get_substitutions1(tail,s) 
                 |SOME(e)=> e@get_substitutions1(tail,s); 

(* 3 *)
fun get_substitutions2(l : string list list , s : string ) = 
    let 
        fun tail_rec(l : string list list , result: string list ) = 
            case l of 
                []            => result
                |head :: tail => case all_except_option(s,head) of 
                         NONE    => tail_rec(tail,result)
                        |SOME(e) => tail_rec(tail,result @ e) 
    in 
        tail_rec(l,[])
    end ; 

(* 4 *) 
fun similar_names(l : string list list , fullname: {first:string, last:string, middle:string} )=
    case fullname of  {first=first,last=last,middle=middle} =>
        let 
            fun mk_fullname(first_names : string list,result)=
                  case first_names of  
                      [] => reverse(result,[])
                    | head :: tail =>  mk_fullname(tail,{first=head,middle=middle,last=last} :: result  )
        in 
            fullname ::mk_fullname(get_substitutions2(l,first),[])
        end ;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 5 *)
fun card_color(c : card ) = 
    case c of 
         (Diamonds,_) => Red 
        |(Hearts  ,_) => Red 
        |_            => Black ; 

(* 6 *)
fun card_value (c : card) = 
    case c of 
         (_ , Ace)   => 11 
        |(_ , Num n) => n
        |(_ , _)     => 10;

(* 7 *)
fun remove_card (cs : card list , c : card , e) =
    case cs of 
         [] => raise e 
        |head :: tail => if head = c then tail else head :: remove_card(tail,c,e);

(* 8 *)
fun all_same_color(l : card list ) = 
    case l of 
         [] => true 
        |head :: [] => true  
        |one :: tow :: other => (card_color(one) = card_color(tow)) andalso all_same_color(tow :: other ) ;

(* 9 *)
fun sum_cards (l : card list) = 
    let 
        fun tail_rec (l , result) = 
            case l of  
                 [] => result 
                |head :: tail => tail_rec(tail,result + card_value(head))
    in 
        tail_rec( l , 0 )
    end ;

(* 10 *)
fun score(l : card list , goal : int ) = 
    let 
        val sum = sum_cards(l)
        val preliminary = if sum > goal  then 3 * (sum - goal) else (goal - sum )
    in 
        if all_same_color(l) then preliminary div 2 else preliminary 
    end ;  

(* 11 *)
fun officiate (cards : card list , moves : move list ,goal : int ) = 
    let fun run(cards : card list , held_cards : card list , moves : move list ) = 
         case (cards,moves) of
            (_,[])          => score(held_cards,goal)
           |([],Draw :: _ ) => score(held_cards,goal)
           |(cards,Discard(card) :: move_tail) => run(cards, remove_card(held_cards,card,IllegalMove),move_tail)
           |(card_head::card_tail,Draw :: move_tail) => 
           let 
                val new_held_cards = card_head :: held_cards
            in 
                if sum_cards(new_held_cards) > goal 
                then score(new_held_cards,goal)
                else run(card_tail,new_held_cards,move_tail)
            end; 
    in  run(cards,[],moves)  
    end ; 
