open A1
exception Match_Type_Error

(*val find : (string * exptype) list -> string -> exptype = <fun>*)
let rec find (g:((string * exptype) list)) (i:string) : exptype = (match g with
    []         -> raise Match_Type_Error
    |(a,b)::xs ->if a=i then b
                    else (find xs i));; 
(*Get ith element from the list*)
let rec geti t1 i = 
                    (match t1 with
                        (n,el)-> (match el with
                                    x::xs -> (if (i==1) then x
                                                else (geti (n-1,xs) (i-1)))
                                    |_    -> raise Match_Type_Error)
                    ) ;;
                        
let rec union t1 t2 :((string * exptype) list) = 
            (match t1 with [] -> t2
            |x::xs -> (union xs (x::t2))
            )
;;
(*For matching of two list*)
let rec matching_list l1 l2 = if (((List.length l1)=(List.length l2)) && l1=l2) then true else false

let rec new_gamma g [d] answer =( match d with
    Simple(x,y,e)   -> if ((typeof g e Tunit) = y) then [(x,y)] else raise Match_Type_Error
    |Sequence(ds) -> (match ds with 
                        x::xs -> (new_gamma ((new_gamma g [x] answer)@g) xs answer)@(new_gamma g [x] answer)
                        |_->raise Match_Type_Error)
    |Parallel(ds) ->( match ds with 
                        x::xs -> (new_gamma (g) xs answer)@(new_gamma g [x] answer)
                        |_->raise Match_Type_Error)
    |Local(d1,d2) -> (new_gamma ((new_gamma g [d1] answer)@g) [d2] answer)
    )
and
typeof2 g ys t ans= match ys with
                        []->ans
                        |x::xs -> (typeof2 g xs t ans@[(typeof g x t)])
and
typeof g x t : exptype= (match x with
    N(i)                       -> Tint
    |B(b)                      -> Tbool 
    |Var(x)                    -> (find g x)
    |Add(t1,t2)                -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tint else raise Match_Type_Error
    |Sub(t1,t2)                -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tint else raise Match_Type_Error
    |Mult(t1,t2)               -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tint else raise Match_Type_Error
    |Div(t1,t2)                -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tint else raise Match_Type_Error
    |Rem(t1,t2)                -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tint else raise Match_Type_Error
    |Conjunction(t1,t2)        -> if ((typeof g t1 t )= Tbool && (typeof g t2 t )=Tbool) then Tbool else raise Match_Type_Error
    |Disjunction(t1,t2)        -> if ((typeof g t1 t )= Tbool && (typeof g t2 t )=Tbool) then Tbool else raise Match_Type_Error
    |Equals(t1,t2)             -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tbool else raise Match_Type_Error
    |GreaterTE(t1,t2)          -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tbool else raise Match_Type_Error 
    |LessTE(t1,t2)             -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tbool else raise Match_Type_Error 
    |GreaterT(t1,t2)           -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tbool else raise Match_Type_Error 
    |LessT(t1,t2)              -> if ((typeof g t1 t )= Tint && (typeof g t2 t )=Tint) then Tbool else raise Match_Type_Error  
    |InParen(t1)               -> (typeof g t1 t )
    |IfThenElse(t1,t2,t3)      -> (if ((typeof g t1 t)=Tbool) then (if ((typeof g t2 t)=(typeof g t3 t)) then (typeof g t2 t) else raise Match_Type_Error) else raise Match_Type_Error)
    |Tuple(a,el)               ->(match el with
                                            y::ys-> (Ttuple( (typeof g y t)::(typeof2 g ys t [])))
                                            |_   ->raise Match_Type_Error )
    |Not(t1)                   -> Tbool  
    |Negative(t1)              -> Tint  
    |Abs(t1)                   -> Tint  
    |Project((i,n),t1)         -> (match t1 with 
                                        Tuple(n,el) -> (typeof g (geti (n,el) i) t)
                                        |_ -> (match (typeof g t1 t) with 
                                                        Ttuple(ell)-> (geti (n,ell) i)
                                                        |_->raise Match_Type_Error))
    |Let(defi,t1)              -> (typeof (union (new_gamma g [defi] []) g ) t1 t)                             
    |FunctionAbstraction(x,y,t1) -> Tfunc(y,(typeof (union [(x,y)] g) t1 Tunit))
    |FunctionCall(t1,t2)       ->(let Tfunc(k1,k2)=(typeof g t1 t) in (if k1=(typeof g t2 t) then k2 else raise Match_Type_Error)
                                ))
                               
;;
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = try (match e with
    N(i)                     -> if (t=Tint) then true else false
    |B(b)                    -> if (t=Tbool) then true else false 
    |Var(x)                  -> if (t=(find g x)) then true else false
    |Add(t1,t2)              -> if (t=Tint) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |Sub(t1,t2)              -> if (t=Tint) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |Mult(t1,t2)             -> if (t=Tint) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |Div(t1,t2)              -> if (t=Tint) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |Rem(t1,t2)              -> if (t=Tint) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |Conjunction(t1,t2)      -> if (t=Tbool) then (if ((hastype g t1 Tbool) && (hastype g t2 Tbool)) then true else false) else false
    |Disjunction(t1,t2)      -> if (t=Tbool) then (if ((hastype g t1 Tbool) && (hastype g t2 Tbool)) then true else false) else false 
    |Equals(t1,t2)           -> if (t=Tbool) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false
    |GreaterTE(t1,t2)        -> if (t=Tbool) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false 
    |LessTE(t1,t2)           -> if (t=Tbool) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false 
    |GreaterT(t1,t2)         -> if (t=Tbool) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false 
    |LessT(t1,t2)            -> if (t=Tbool) then (if ((hastype g t1 Tint) && (hastype g t2 Tint)) then true else false) else false  
    |InParen(t1)             -> if (hastype g t1 t) then true else false
    |IfThenElse(t1,t2,t3)    -> if ((hastype g t1 Tbool) && (hastype g t2 t) && (hastype g t3 t)) then true else false
    |Tuple(a,el)             ->( match t with Ttuple(ell)->
                                    (match el with
                                        []->true
                                        |(x::xs)->(if ((hastype g x (List.hd(ell))) && (hastype g (Tuple(a-1,xs)) (Ttuple(List.tl(ell))))) then true else false)
                                    )
                                    |_ -> raise Match_Type_Error)
    |Not(t1)                  -> if t=Tbool then (if (hastype g t1 Tbool) then true else false) else false  
    |Negative(t1)             -> if t=Tint then (if (hastype g t1 Tint) then true else false) else false  
    |Abs(t1)                  -> if t=Tint then (if (hastype g t1 Tint) then true else false) else false  
    |Project((i,n),t1)        -> if (typeof g (Project((i,n),t1)) t)=t then true else false
    |Let(defi,t1)             -> (if (hastype (union (new_gamma g [defi] []) g) t1 t) then true else false )  
    |FunctionAbstraction(x,y,t1)->if t=(typeof (union g [(x,y)]) (FunctionAbstraction(x,y,t1)) t) then true else false
    |FunctionCall(t1,t2)      -> if (let Tfunc(k1,k2)=(typeof g t1 t) in (if ((hastype g t2 k1) && t=k2) then true else false))then true else false
)
with Match_Type_Error -> false
;;

(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d (g_dash:((string * exptype) list)) =try( match d with
    Simple(x,y,e)   -> (match g_dash with
                        [(x,b)]-> if b=y then true else false
                        |_ -> raise Match_Type_Error
                    ) 
    |Sequence(ds) -> if (matching_list (new_gamma g [Sequence(ds)] [])  g_dash) then true else false
    |Parallel(ds) -> if (matching_list (new_gamma g [Parallel(ds)] [])  g_dash) then true else false
    |Local(d1,d2) -> if (matching_list (new_gamma g [Local(d1,d2)] []) g_dash) then true else false
) with Match_Type_Error -> false
;;
