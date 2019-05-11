(*Generalized with 0 as []*)

open Signature_a0
    module A0 : BigInt = struct
  (*Your code goes here*)

    type bigint = sign * int list
    and sign=Neg|NonNeg ;;
   
    (*To convert int to list of int*)
    let rec mList (x:int) = match x with
        0->[]
        |_->(mList (x/10))@ [x mod 10]
    ;; 
    (*int -> int list*)

    (*This fuction is to convert int to bigint*)
    (*If you are giving negative number than write in brackets eg for x= -12 write mk_big (-12)*)
    let mk_big (x:int) : bigint = if x<0 then (Neg,mList ((-1)*x)) 
			else (NonNeg,mList x);; 

    (*This fuction is to calculate length of list*)
    let rec leng l = match l with
        []->0
        |x::xs->1+(leng xs);; 
    (*val leng : 'a list -> int = <fun>*)

    (*This fuction is to reverse the list*)
    let rec rev l = match l with
        []->[]
        |x::xs-> (rev xs)@[x]
    ;; 
    (*val rev : 'a list -> 'a list = <fun>*)
    
    (*This fuction is to remove unwanted zeroes from list wherever needed in implemented functions*)
    let rec shave (l:int list):int list = match l with
        []->[]
        |(x::xs)->if x=0 then shave(xs)
                else l ;;            
    (*val shave : int list -> int list = <fun>*)
    
    let abs (l:bigint) : bigint = match l with
        (_,[])-> (NonNeg,[])
        |(NonNeg,xs)->(NonNeg,xs)
        |(Neg,xs)->(NonNeg,xs)
    ;; 
    (*val abs : bigint -> bigint = <fun>*)

    let minus (l:bigint) : bigint = match l with
        (Neg,[])->(NonNeg,[])
        |(NonNeg,[])->(Neg,[])
        |(NonNeg,xs)-> (Neg,xs)
        |(Neg,xs)->(NonNeg,xs)
    ;; 
    (*val minus : bigint -> bigint = <fun>*)
    
    
    (*For running this sub-function length of lists l1 and l2 must be equal*)
    let rec pos_gt (l1:int list) (l2:int list) = match l1 with  
        []->(match l2 with
            []->false
            |_->false
            )
        |x::xs->(match l2 with
            [] -> true
            |y::ys->if x<y then false
                else if x>y then true
                else pos_gt xs ys );; 
    (*val pos_gt : int list -> int list -> bool = <fun>*)
                            
    (*For running this sub-function length of lists l1 and l2 must be equal*)
    let rec neg_gt l1 l2 = match l1 with  
        []->(match l2 with
            []->false
            |_->true)
        |x::xs->(match l2 with
                    []->false
                    |y::ys->if x>y then false
                            else if x<y then true
                            else neg_gt xs ys) ;; 
    (*val neg_gt : int list -> int list -> bool = <fun>*)

    (*For running this sub-function length of lists l1 and l2 must be equal*)
    let rec eq_l l1 l2 = match l1 with  
        []->(match l2 with
            []->true
            |_->false)
        |[0]->(match l2 with
            [0]->true
            |_->false)
        |x::xs->(match l2 with
                    []->false
                    |y::ys->if x<y then false
                            else if x>y then false
                            else eq_l xs ys );; 
    (*val eq_l : int list -> int list -> bool = <fun>*)

    (*This function uses length then value inside list to compare bigint*)
    let eq (x:bigint) (y:bigint):bool = match x with
        (_,[]) -> (match y with (_, []) -> true
                        |(Neg,l) -> false
                        |(NonNeg,l) -> false)
        |(NonNeg,l1)->(match y with 
                    (_,[]) -> false
                    |(Neg,l2)->false
                    |(NonNeg,l2)-> if leng l1 < leng l2 then false
                                    else if leng l1 > leng l2 then false
                                    else eq_l l1 l2 )
        |(Neg,l1)->(match y with 
                    (_,[]) -> false
                    |(NonNeg,l2)->false
                    |(Neg,l2)-> if leng l1 < leng l2 then false
                                    else if leng l1 > leng l2 then false
                                    else eq_l l1 l2 )
    ;; 
    (*val eq : bigint -> bigint -> bool = <fun>*)

    (*This function uses length then value inside list to compare bigint*)
    let gt (x:bigint) (y:bigint):bool = match x with
        (_,[]) -> (match y with (_, []) -> false
                        |(Neg,l) -> true
                        |(NonNeg,l) -> false)
        |(NonNeg,l1)->(match y with 
                        (_,[])->true
                        |(Neg,l2) -> true
                        |(NonNeg,l2)-> if leng l1 > leng l2 then true
                                else if leng l1 < leng l2 then false
                                else (match l1 with
                                    []->false
                                    |x::xs->(match l2 with
                                            []->false
                                            |y::ys->if x>y then true
                                                    else if x<y then false
                                                    else pos_gt l1 l2 )))
        |(Neg,l1)->(match y with (_,[]) -> false
                |(NonNeg,l2) -> false
                |(Neg,l2)-> if leng l1 < leng l2 then true
                                else if leng l1 > leng l2 then false
                                else (match l1 with
                                    []->false
                                    |x::xs->(match l2 with
                                            []->false
                                            |y::ys->if x<y then true
                                                    else if x>y then false
                                                    else neg_gt l1 l2)))
    ;; 
    (*val gt : bigint -> bigint -> bool = <fun>*)

    (*This function uses length then value inside list to compare bigint*)
    let lt (x:bigint) (y:bigint):bool = match x with
        (_,[]) -> (match y with (_, []) -> false
                        |(Neg,l) -> false
                        |(NonNeg,l) -> true)
        |(NonNeg,l1)->(match y with 
                        (_,[])->false
                        |(Neg,l2)->false
                        |(NonNeg,l2)-> if leng l1 > leng l2 then false
                                else if leng l1 < leng l2 then true
                                else (match l1 with
                                    []->false
                                    |x::xs->(match l2 with
                                            []->false
                                            |y::ys->if x>y then false
                                                    else if x<y then true
                                                    else neg_gt l1 l2)) )
        |(Neg,l1)->(match y with (_,[])->false
                        |(NonNeg,l2)->true
                        |(Neg,l2)-> if leng l1 < leng l2 then false
                                else if leng l1 > leng l2 then true
                                else (match l1 with
                                    []->false
                                    |x::xs->(match l2 with
                                            []->false
                                            |y::ys->if x<y then false
                                                    else if x>y then true
                                                    else pos_gt l1 l2 ))) 
    ;; 
    (*val lt : bigint -> bigint -> bool = <fun>*)

    (*This function uses earlier implemented functions gt and eq to compare bigint*)
    let geq (x:bigint) (y:bigint) : bool = if gt x y then true
                                            else if eq x y then true
                                                else false
    ;; 
    (*val geq : bigint -> bigint -> bool = <fun>*)
                                        
    (*This function uses earlier implemented functions gt and eq to compare bigint*)
    let leq (x:bigint) (y:bigint):bool = if lt x y then true
                                            else if eq x y then true
                                                else false
    ;; 
    (*val leq : bigint -> bigint -> bool = <fun>*)

    (*This function just take two int a and b and carry c=0 initially and returns (ans bit , carry bit)*)
    let addc (a:int) (b:int) (c:int): (int*int) = (match a with
                0->(match b with
                    0->(c,0)
                    |_->if c=0 then (b,0)
                            else ((b+c) mod 10,(b+c)/10))
                |_->(match b with
                    0->if c=0 then (a,0)
                            else ((a+c) mod 10,(a+c)/10)
                    |_->if c=0 then ((a+b) mod 10,(a+b)/10)
                            else ((a+b+c) mod 10,(a+b+c)/10)))
    ;; 
    (*val addc : int -> int -> int -> int * int = <fun>*)

    (*This function just take two int a and b and borrow c=0 initially and will give (ans bit , borrowed/not)*)
    let subc (a:int) (b:int) (c:int): (int*int) = match a with
                0->(match b with
                    0->if c=0 then (0,0)
                        else (9,1)
                    |_->if c=0 then (10-b,1)
                            else (9-b,1))
                |_->(match b with
                    0->(if c=0 then (a,0)
                            else ((a-c) mod 10,0))
                    |_->(if a<b then 
                            (if c=0 then (a+10-b,1)
                                else (a-1+10-b,1))
                        else (if c=0 then (a-b,0)
                                else (if a-1 > b then (a-1-b,0)
                                        else if a-1 = b then (0,0)
                                    else (a-1+10-b,1))
                            ))
                    )
    ;; 
    (*val subc : int -> int -> int -> int * int = <fun>*)

    (*Take two lists l1 and l2 and c=0 and will give the answer as reversed list*)
    let rec add_list (l1:int list) (l2:int list) (c:int) : int list = match rev l1 with
        []->(match rev l2 with
            []-> if c=0 then []
                    else [c]
            |(x::xs)-> if c=0 then (rev l2)
                    else let (m,n) = addc x 0 c in
                    m::(add_list [] (rev xs) n))
        |x::xs->(match rev l2 with
                    []->(if c=0 then (rev l1)
                    else let (m,n) = addc x 0 c in
                        m::(add_list [] (rev xs) n))
                    |y::ys-> let(m,n) = addc x y c in
                    m::(add_list (rev xs) (rev ys) n));; 
    (*val add_list : int list -> int list -> int -> int list = <fun>*)    

    
    (*only called when l1>=l2 and give reversed list.... it may happen that it gives [8;0] in place of 8 so take care when u reverse it to remove head , For this I have made shave function which is used wherever necessary*)
    let rec sub_list (l1:int list) (l2:int list) (c:int) : int list = match rev l1 with
        []->(match rev l2 with
            []-> if c=0 then []
                    else [c]
            |(x::xs)-> if c=0 then (rev l2)
                    else let (m,n) = subc x 0 c in
                    m::(add_list [] (rev xs) n))
        |x::xs->(match rev l2 with
                    []->(if c=0 then (rev l1)
                        else let (m,n) = subc x 0 c in
                        m::(sub_list (rev xs) [] n))
                    |y::ys-> let(m,n) = subc x y c in
                    m::(sub_list (rev xs) (rev ys) n));; 
    (*val sub_list : int list -> int list -> int -> int list = <fun>*)                

    let add (l1:bigint) (l2:bigint) : bigint = (match l1 with
        (_,[])->l2
        |(Neg,xs)-> (match l2 with
                    (_,[])->l1
                    |(Neg,ys) -> (Neg,rev (add_list xs ys 0))
                    |(NonNeg,ys) -> (if (gt (NonNeg,xs) (NonNeg,ys)) = true then (Neg,rev (sub_list xs ys 0))
                                        else if gt (NonNeg,ys) (NonNeg,xs) = true then (NonNeg,rev (sub_list ys xs 0))
                                        else (NonNeg,[0]) ) ) (*-a+a = [0] or []*)
        |(NonNeg,xs)-> (match l2 with
                        (_,[])->l1
                        |(NonNeg,ys) -> (NonNeg,rev (add_list xs ys 0))
                        |(Neg,ys) -> (if gt (NonNeg,xs) (NonNeg,ys) = true then (NonNeg,rev (sub_list xs ys 0))
                                            else if gt (NonNeg,ys) (NonNeg,xs) = true then (Neg,rev (sub_list ys xs 0))
                                            else (NonNeg,[0]) ) ) ) (*-a+a = [0] or []*)
    ;; 
    (*val add : bigint -> bigint -> bigint = <fun>*)
                                        
    let sub (l1:bigint) (l2:bigint) : bigint = (match l1 with
        (_,[])->(match l2 with
            (_,[]) -> (NonNeg,[])
            |(Neg,xs)->(NonNeg,shave(xs))
            |(NonNeg,xs)->(Neg,shave(xs)))
        |(Neg,xs)->(match l2 with
            (_,[])->l1
            |(NonNeg,ys) -> (Neg,shave(rev (add_list xs ys 0)))
            |(Neg,ys) -> (if gt (NonNeg,xs) (NonNeg,ys) = true then (Neg,shave(rev (sub_list xs ys 0)))
                                else if gt (NonNeg,ys) (NonNeg,xs) = true then (NonNeg,shave(rev (sub_list ys xs 0)))
                                else (NonNeg,[]))) (*-a+a = []*)
        |(NonNeg,xs)->(match l2 with
                        (_,[])->l1
                        |(Neg,ys) -> (NonNeg,shave(rev (add_list xs ys 0)))
                        |(NonNeg,ys) -> (if gt (NonNeg,xs) (NonNeg,ys) = true then (NonNeg,shave(rev (sub_list xs ys 0)))
                                            else if gt (NonNeg,ys) (NonNeg,xs) = true then (Neg,shave(rev (sub_list ys xs 0)))
                                            else (NonNeg,[]) ) )) (*-a+a = []*)
    ;; 
    (*val sub : bigint -> bigint -> bigint = <fun>*)


    (*takes input as reverse list l and then multiply by m* and give again rev list i.e. for 1723*2 give input l=3271 and output is rev list*)
    let rec intmult (l:int list) (m:int) (c:int): int list= (match l with
        []->if c=0 then []
            else [c]
        |[0]->[0]
        |x::xs -> (match m with
                    0 -> [0]
                    |_->(if c=0 then ((x*m) mod 10) :: (intmult xs m ((x*m)/10))
                        else ((x*m + c) mod 10) :: (intmult xs m ((x*m + c)/10)))))
    ;; 
    (*val intmult : int list -> int -> int -> int list = <fun>*)

    (*multiplying two lists and get reversed list as answer and gives 1 extra zero in ans list*)
    let rec mult_l (l1:int list) (l2:int list) : int list =( match rev l1 with
        []->(match rev l2 with
            []-> []
            |[0]->[0]
            |_-> [])
        |(x::xs)->(match rev l2 with
                []->[]
                |[0]->[0]
                |(y::ys)-> (rev (add_list (rev (intmult (rev l1) y 0)) (mult_l l1 (rev ys)) 0 ))@[0] ))
    ;; 

    (*Function to outputs multiplication result in form of list given two bigint*)
    let mult (l1:bigint) (l2:bigint) : bigint = (match l1 with
        (_,[])->(NonNeg,[])
        |(_,[0])->(NonNeg,[])
        |(Neg,xs)->(match l2 with
                        (_,[])->(NonNeg,[0])
                        |(_,[0])->(NonNeg,[])
                        |(NonNeg,ys)->(Neg,(rev (List.tl(rev (mult_l xs ys)))) )
                        |(Neg,ys) -> (NonNeg,(rev (List.tl(rev (mult_l xs ys)))) )
                    )
        |(NonNeg,xs)->(match l2 with
                            (_,[])->(NonNeg,[])
                            |(_,[0])->(NonNeg,[])
                            |(NonNeg,ys)->(NonNeg,(rev (List.tl(rev (mult_l xs ys)))) )
                            |(Neg,ys) -> (Neg,(rev (List.tl(rev (mult_l xs ys)))) )
                    ))
    ;; 

    exception Negative of int;; 
    (* Take first n elements of l *)
    let rec take n l = if n < 0 then raise (Negative n)
                        else match l with
                            [] -> []
                        |   x::xs -> if n = 0 then [ ]
                                    else x::(take (n-1) xs);; 

    (* Drop first n elements of l *)
    let rec drop n l = if n < 0 then raise (Negative n)
                        else match l with
                            [] -> []
                        |   x::xs -> if n = 0 then l
                            else (drop (n-1) xs);; 
                            
                            
    (*This sub function returns quotient and remainder l2|l1| *)
    let rec div_sub (l1:int list) (l2:int list) (c:int): int list * int list = match l1 with
        []->([0],[])
        |_-> if (gt (NonNeg,l2) (NonNeg,l1)) = true then ([c],l1)
                else if eq (NonNeg,l2) (NonNeg,l1) = true then ([c+1],[0])
                else (div_sub (shave(rev (sub_list l1 l2 0) )) l2 (c+1)) 
    ;; 

    (*Function to append m zeroes in list l1*)
    let rec append l1 m =  match m with
        0->l1
        |_->(append (l1@[0]) (m-1))
    ;;
    
      
    (*Finding quotient by finding quotient initially and then appending required zeroes and further adding numbers to get required quotient value*)
    let rec div_list_q (l1:int list) (l2:int list) (n:int list): int list = (match l1 with
        []->n
        |_-> (if (gt (NonNeg,l2) (NonNeg,l1)) = true then n
            else
                (if (gt (NonNeg,l2) (NonNeg,(take (leng l2) l1)) ) = false then
                    let (m,k) = (div_sub (take (leng l2) l1) l2 0) in
                    div_list_q (shave(rev (sub_list l1 (append (rev (List.tl(rev (mult_l l2 m)))) (leng l1 - leng l2)) 0))) l2 (rev (add_list (append m (leng l1 - leng l2)) n 0))
                else 
                let (m,k) = (div_sub (take ((leng l2)+1) l1) l2 0) in
                div_list_q (shave(rev (sub_list l1 (append (rev (List.tl(rev (mult_l l2 m)))) (leng l1 - leng l2 -1)) 0))) l2 (rev (add_list (append m ((leng l1)-(leng l2) -1)) n 0)))
            ));;
    

    exception ZeroDivisionError;; 
    (*Function to output quotient in form of list given two bigint*)
    let div (l1:bigint) (l2:bigint) : bigint = (match l1 with
        (_,[])->(match l2 with
                    (_,[])->raise ZeroDivisionError
                    |_ -> (NonNeg,[])
                )
        |(_,[0])->(match l2 with
                    (_,[])->raise ZeroDivisionError
                    |_ -> (NonNeg,[])
                )
        |(Neg,xs)->(match l2 with
                        (_,[])->raise ZeroDivisionError
                        |(_,[0])->raise ZeroDivisionError   (*raise exception*)
                        |(NonNeg,ys)->(Neg,(div_list_q xs ys []))
                        |(Neg,ys) -> (NonNeg,(div_list_q xs ys []))
                    )
        |(NonNeg,xs)->(match l2 with
                            (_,[])->raise ZeroDivisionError
                            |(_,[0])->raise ZeroDivisionError  (*raise exception*)
                            |(NonNeg,ys)->(NonNeg,(div_list_q xs ys []))
                            |(Neg,ys) -> (Neg,(div_list_q xs ys [])))
                    )
    ;; 
    (*Function to output remainder in form of list given two bigint and giving sign same as divident*)
    let rem (l1:bigint) (l2:bigint) : bigint = (sub l1 (mult l2 (div l1 l2)))
    ;; 
    (*This function converts the list into integer basically and Neg to "-" to show that integer is negative *)
    let rec print_num (l1: bigint):string = match l1 with
        (_,[])->""
        |(Neg,xs)->""^"-"^(print_num (NonNeg,xs))
        |(NonNeg,x::xs)->(string_of_int x)^print_num (NonNeg,xs)
    ;; 

    end

