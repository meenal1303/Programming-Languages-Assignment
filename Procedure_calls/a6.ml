exception Error;;

(* abstract syntax *)
type  exptree =
  Var of string
  | N of int      
  | Value of exptree * exptree
  | F of exptree * exptree
  | Call of string * exptree                  
  | Ret
;;

let (cs:(string * string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) ) list ref) = ref ([]);; (*dynamic links*)
let (sl:((string) list) ref) = ref ([]);; (*static links*)
let (cf:((string) list) ref) = ref ([]);; (*What procedures can be called*)
let (va:((string*(int ref)) list) ref) = ref ( []);; (*What variables can be accesed*)
(*Parent , child , formal parameters , local var  *)
let tree_info (x:string) : (string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) ) = (match x with
    "Main" -> ([],["P";"Q"],[],[("a",ref (-1));("b",ref (-1));("c",ref (-1))])
    |"P"   -> (["Main"],["R";"S"],[("x",ref (-1));("y",ref (-1))],[("z",ref (-1));("a",ref (-1))])
    |"Q"   -> (["Main"],["T";"U"],[("z",ref (-1));("w",ref (-1))],[("x",ref (-1));("b",ref (-1))])
    |"R"   -> (["Main";"P"],["V"],[("w",ref (-1));("i",ref (-1))],[("j",ref (-1));("b",ref (-1))])
    |"S"   -> (["Main";"P"],[],[("c",ref (-1));("k",ref (-1))],[("m",ref (-1));("n",ref (-1))])
    |"V"   -> (["Main";"P";"R"],[],[("m",ref (-1));("n",ref (-1))],[("c",ref (-1))])
    |"T"   -> (["Main";"Q"],["W"],[("a",ref (-1));("y",ref (-1))],[("i",ref (-1));("f",ref (-1))])
    |"U"   -> (["Main";"Q"],[],[("c",ref (-1));("z",ref (-1))],[("p",ref (-1));("g",ref (-1))])
    |"W"   -> (["Main";"Q";"T"],[],[("m",ref (-1));("p",ref (-1))],[("j",ref (-1));("h",ref (-1))])
    |_ -> failwith "Tree Node DNE"
);;
(*Parent , child , formal parameters , local var  *)
let node (x:string) (q:int) (r:int): (string * string list * string list * ( ( string * (int ref) ) list) * ( ( string * ( int ref ) ) list ) ) = (match x with
    "Main" -> ("Main",[],["P";"Q"],[],[("a",ref (-1));("b",ref (-1));("c",ref (-1))])
    |"P"   -> ("P",["Main"],["R";"S"],[("x",ref (q));("y",ref (r))],[("z",ref (-1));("a",ref (-1))])
    |"Q"   -> ("Q",["Main"],["T";"U"],[("z",ref (q));("w",ref (r))],[("x",ref (-1));("b",ref (-1))])
    |"R"   -> ("R",["Main";"P"],["V"],[("w",ref (q));("i",ref (r))],[("j",ref (-1));("b",ref (-1))])
    |"S"   -> ("S",["Main";"P"],[],[("c",ref (q));("k",ref (r))],[("m",ref (-1));("n",ref (-1))])
    |"V"   -> ("V",["Main";"P";"R"],[],[("m",ref (q));("n",ref (r))],[("c",ref (-1))])
    |"T"   -> ("T",["Main";"Q"],["W"],[("a",ref (q));("y",ref (r))],[("i",ref (-1));("f",ref (-1))])
    |"U"   -> ("U",["Main";"Q"],[],[("c",ref (q));("z",ref (r))],[("p",ref (-1));("g",ref (-1))])
    |"W"   -> ("W",["Main";"Q";"T"],[],[("m",ref (q));("p",ref (r))],[("j",ref (-1));("h",ref (-1))])
    |_ -> failwith "Tree Node DNE"
);;
let rec contains x y = match y with 
                    [] -> false
                    |(a,t)::ss -> if a=x then true else (contains x ss )
;;
let rec contains2 x y = match y with 
                    [] -> false
                    |(a)::ss -> if a=x then true else (contains2 x ss )
;;
let rec union l1 l2 = match l2 with
                            [] -> []
                            |(x,y)::xs -> if (contains x l1) then (union l1 xs) else ((x,y)::(union l1 xs))    
;;
let rec g_par (k:string) t = if k="Main" then t else (match (tree_info k) with (a,b,c,d) -> (match a with u::r -> (g_par u (u::t))))
;;

let get_static_link x = match x with (e1,a1,b1,c1,d1) -> a1

;;
(*let rec union2 l1 l2 = match l2 with
                            [] -> []
                            |x::xs -> if (contains x l1) then (union2 l1 xs) else (x::(union2 l1 xs))  
;;*)
let rec union3 l1 l2 = match l2 with
                            [] -> l1
                            |x::xs -> if (contains2 x l1) then (union3 l1 xs) else (x::(union3 l1 xs))  
;;
(*c varia with values*)   (*DOUBT*)
let rec get_childs sc = match sc with
                            []     -> []
                            |x::xs -> (match (tree_info x) with (a1,b1,c1,d1) -> (union3 b1 (get_childs (xs))))
;;
(*b child list*)
let rec get_frame x y = if List.tl(x) = [] then (get_childs y) else (union3 (get_childs y) y)
;;
(*x is local var list*)
let rec rep2 (x:( ( string * (int ref) ) list)) y z = (match x with 
                (l,k)::ms -> if l=y then (k:=z) else (rep2 ms y z)
                |_ -> failwith "rep2 fail" )
;;
(*variables assigning with Value*)
let rec replace x kl (y:string) (z:int)= (match x with 
				[]    -> failwith "No match has been found"
                |w::ws ->(match (List.hd(kl)) with (e1,a1,b1,c1,d1) -> if (w=e1) then ( if (contains y d1) then (rep2 d1 y z) else (replace ws kl y z)) else (replace (w::ws) (List.tl(kl)) y z))
                |_    -> failwith "replace")
;;
let rec get_var_acc (x:string list) (ll) = (match x with
                        []     -> []
                        |y::ys -> (match (List.hd(ll)) with (e,a,b,c,d) -> if e=y then ((c)@(d)@(union ((c)@(d)) (get_var_acc ys (List.tl(ll))) )) else  (get_var_acc (y::ys) (List.tl(ll))))
                        |_     -> failwith "get_var_acc"
);;

let rec update x va1 y = (match va1 with
                                    (z,p)::zs -> if z=x then (eval (Value(Var(y),N(!p)))) else (update x zs y)
                                    |_ -> failwith "update")
and
eval (e:exptree)  = match e with
    Value(Var(x),y)   -> (match y with
                          Var(y1) -> (update y1 (!va) x) (*replace from accessible var values*)
                          |N(i)   -> (match (List.hd(!cs)) with (e,a,b,c,d) ->( if (contains x c) then ((rep2 c x i);(va:=(get_var_acc (List.rev(!sl)) (!cs))))
                                                            else if (contains x d) then ((rep2 d x i);(va:=(get_var_acc (List.rev(!sl)) (!cs))))
                                                      else ((replace (List.rev(!sl)) (!cs) x i );(va:=(get_var_acc (List.rev(!sl)) (!cs))))))
                          |_     -> failwith "Value error"
                         )
    |Call("Main",F(N(p),N(q)))-> (cs:=((node "Main" p q)::!cs)) ; (sl:=["Main"]) ; (va:=[("a",ref(-1));("b",ref(-1));("c",ref(-1))]) ; (cf:=["Main";"P";"Q"])                  
    |Call(x,F(N(y),N(z))) -> (if (contains2 x (!cf)) then ((node x y z) ; (cs:=(node x y z)::(!cs)) ; (sl:=((get_static_link (List.hd(!cs)))@[x])); (va:=(get_var_acc (List.rev(!sl)) (!cs))) ; (cf:=(get_frame (!cs) (!sl))) ) else failwith "eval Error")
    |Ret -> (cs:=(List.tl(!cs)));(sl:=(get_static_link (List.hd(!cs)))@[match (List.hd(!cs)) with (e1,a1,b1,c1,d1) -> e1]);(va:=(get_var_acc (List.rev(!sl)) (!cs)));(cf:=(get_frame (!cs) (!sl)))
    |_                -> failwith "eval fail"
;;
