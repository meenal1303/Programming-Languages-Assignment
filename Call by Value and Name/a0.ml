exception InvalidOperation
exception TypeError

type expr = 
    V of string 
    | Bool of bool 
    | Integer of int 
    | Cmp of expr 
    | Lambda of (expr * expr) 
    | App of (expr * expr) 
    | Plus of (expr * expr) 
    | Mult of (expr * expr) 
    | And of (expr * expr) 
    | Or of (expr * expr) 
    | If_Then_Else of (expr * expr * expr)
;;
type types = Tint | Tbool | Tfunc of types*types ;;

let rec type_check (e:expr) (g:((types) list)):types = 
    (match e with
        V(x) -> if g=[] then (raise TypeError) else (List.hd(g))
        |Bool(b) -> Tbool
        |Integer(i) -> Tint
        |Cmp(e) -> if ((type_check e g)=Tint) then Tbool else raise TypeError
        |Lambda(e1,e2) -> (match e1 with V(x) ->Tfunc((type_check e1 g),(type_check e2 g))
                                |_ -> raise TypeError)
        |App(e1,e2) -> (match (type_check (e1) ((type_check e2 g)::g)) with
                        Tfunc(a1,a2) -> if (a1=(type_check e2 g)) then a2 else raise TypeError
                        |_           -> raise TypeError)
        |Plus(e1,e2)-> if ((type_check e1 g)=Tint && (type_check e2 g)=Tint) then Tint else raise TypeError
        |Mult(e1,e2)-> if ((type_check e1 g)=Tint && (type_check e2 g)=Tint) then Tint else raise TypeError
        |And(e1,e2) -> if ((type_check e1 g)=Tbool && (type_check e2 g)=Tbool) then Tbool else raise TypeError
        |Or(e1,e2)  -> if ((type_check e1 g)=Tbool && (type_check e2 g)=Tbool) then Tbool else raise TypeError
        |If_Then_Else(e1,e2,e3) -> if ((type_check e1 g)=Tbool && ((type_check e2 g)=(type_check e3 g))) then (type_check e2 g) else raise TypeError
)

let type_call (e:expr):bool = 
    (match e with
        V(x)        -> true
        |Bool(b)    -> true
        |Integer(i) -> true
        |Cmp(e)     -> if (type_check (Cmp(e)) [])=Tbool then true else false
        |Plus(e1,e2)-> if (type_check (Plus(e1,e2)) [])=Tint then true else false
        |Mult(e1,e2)-> if (type_check (Mult(e1,e2)) [])=Tint then true else false
        |And(e1,e2) -> if (type_check (And(e1,e2)) [])=Tbool then true else false
        |Or(e1,e2)  -> if (type_check (Or(e1,e2)) [])=Tbool then true else false
        |If_Then_Else(e1,e2,e3) -> if ((type_check (If_Then_Else(e1,e2,e3)) [])=(type_check e3 [])) then true else false
        |Lambda(e1,e2) -> (match e1 with V(x) -> 
                            (match (type_check (Lambda(e1,e2)) []) with
                                Tfunc(a1,a2) -> if (type_check e2 []) = a2 then true else false
                                |_           -> false)
                        |_ -> false)
        |App(e1,e2) -> (match (type_check e1 ((type_check e2 [])::[])) with
                        Tfunc(a1,a2) -> if ((type_check (App(e1,e2)) [])=a2) then true else false
                        |_           -> false)
)
type k_answer = 
     Num of int*((expr*k_answer) list)
    |Boolean of bool*((expr*k_answer) list)
    |Var of string*((expr*k_answer) list)
    |Clos of (expr * ((expr*k_answer) list))
;;

let rec check (s:k_answer list) (g:((expr*k_answer) list)) (x:string) : k_answer =(match g with
        [] -> failwith "check"
        |(V(z),zs)::ys -> (if (z=x) then 
                            (match zs with 
                                Num(i,b)        -> Num(i,b)
                                |Boolean(bl,b)  -> Boolean(bl,b)
                                |Var(ss,b)      ->(krivine (V(ss)) s b)
                                |Clos(e,b)      ->(krivine e s b))
                            else (check s ys x))
)
(*let extract (x:k_answer):*) 
and krivine (e:expr) (s:k_answer list) (gamma: ((expr*k_answer) list)): (k_answer)= 
    (match e with
        V(x)             -> (check s gamma x)
        |Bool(b)         -> (match s with
                                (Clos((And((Bool(true)),x2)),g)::ms) -> 
                                (match x2 with
                                        Bool(i2)->(krivine (Bool((b&&i2))) ms g)
                                        |_->(krivine x2 ((Clos((And((Bool(true)),Bool(b))),g))::ms) g ))
                                
                                                
                                |(Clos((Or((Bool(false)),x2)),g)::ms) -> 
                                        (match x2 with
                                        Bool(i2)->(krivine (Bool((b||i2))) ms g)
                                        |_->(krivine x2 ((Clos((Or((Bool(false)),Bool(b))),gamma))::ms) g ))
                                        
                                |(Clos((If_Then_Else(x1,x2,x3)),gamma)::ms) ->
                                        (if b=true then (krivine x2 ms gamma) else (krivine x3 ms gamma))

                               | _ -> (Boolean((b),[])))
        |Integer(i)      -> (match s with
                                (Clos(Plus(Integer(0),x2),g)::ms) -> 
                                        (match x2 with
                                                                        Integer(i2)->(krivine (Integer((i+i2))) ms g)
                                                                        |_->(krivine x2 ((Clos((Plus((Integer(0)),Integer(i))),g))::ms) g ))
                                                
                                |(Clos(Mult(Integer(1),x2),g)::ms) -> (match x2 with
                                                                        Integer(i2)->(krivine (Integer((i*i2))) ms g)
                                                                        |_->(krivine x2 ((Clos((Mult((Integer(1)),Integer(i))),g))::ms) g ))
                                |(Clos(Cmp(Integer(0)),g)::ms) -> if (i>0) then (krivine (Bool(true)) ms gamma) else (krivine (Bool(false)) ms gamma)
                                
                               | _ -> (Num((i),[])))
                                    
        |Plus(x1,x2)     -> if (x1 = (Integer(0))) then (krivine x2 s gamma)
                            else (krivine x1 ((Clos((Plus((Integer(0)),x2)),gamma))::s) gamma )
                            
        |Mult(x1,x2)     -> if (x1 = (Integer(1))) then (krivine x2 s gamma)
                            else (krivine x1 ((Clos((Mult((Integer(1)),x2)),gamma))::s) gamma )
                            
        |And(x1,x2)      -> if (x1 = (Bool(true))) then (krivine x2 s gamma)
                            else (krivine x1 ((Clos((And((Bool(true)),x2)),gamma))::s) gamma )
        
        |Or(x1,x2)       -> if (x1 = (Bool(false))) then (krivine x2 s gamma)
                            else (krivine x1 ((Clos((Or((Bool(false)),x2)),gamma))::s) gamma )
                            
        |Cmp(x)          -> (krivine x ((Clos(Cmp(Integer(0)),gamma))::s) gamma)
    
        |If_Then_Else(x1,x2,x3) -> (krivine x1 ((Clos((If_Then_Else(x1,x2,x3)),gamma))::s) gamma)
        
        |Lambda(x1,x2)   -> (match s with
                                []     -> (Clos(Lambda(x1,x2),gamma))
                                |x::xs -> (match x1 with V(var) -> (krivine x2 xs ((V(var),x)::gamma))
                                                         |_     -> failwith "Lambda")
                            )
        |App(x1,x2)      -> (krivine x1 ([Clos(x2,gamma)]@s) gamma)
                            
    )
;;

type opcode = VAR of string | B of bool | N of int | PLUS | MULT | CONJ | DISJ | CMP | COND of ((opcode list)*(opcode list)) | CLOS of ((opcode list)*(opcode list)) | RET | APP ;;

let rec compile (t:expr):opcode list = match t with
    V(x)                                        -> [VAR(x)]
    | Bool(b)                                   -> [B(b)]
    | Integer(i)                                -> [N(i)]
    | Cmp(t1)                                   -> (compile t1)@[CMP]
    | Lambda(t1,t2)                             -> [CLOS((compile t1),(compile t2)@[RET])]
    | App(t1,t2)                                -> (compile t1)@(compile t2)@[APP] 
    | Plus(t1,t2)                               -> (compile t1)@(compile t2)@[PLUS]
    | Mult(t1,t2)                               -> (compile t1)@(compile t2)@[MULT]
    | And(t1,t2)                                -> (compile t1)@(compile t2)@[CONJ] 
    | Or(t1,t2)                                 -> (compile t1)@(compile t2)@[DISJ] 
    | If_Then_Else(t1,t2,t3)                    -> (compile t1)@[COND((compile t2),(compile t3))]
;;
let rec find g x = (match g with
        []             -> failwith "find"
        |(z,zs)::ys    -> if (z=x) then zs else (find ys x)
);;
type s_answer = Nval of int | Bval of bool | Closure of ((string) * (opcode list) * ((string*s_answer) list) ) ;;
type dump = (s_answer list) * ((string*s_answer) list) * (opcode list) ;;

let rec secd (t:opcode list) (s: s_answer list) (rho:((string*s_answer) list)) (d:dump list) : s_answer = (match t with
    []                              -> List.hd(s)
    |(VAR(x)::xs)                   -> (secd xs ((find rho x)::s) rho d)  
    |(B(b)::xs)                     -> (secd xs (Bval(b)::s) rho d)
    |(N(i)::xs)                     -> (secd xs (Nval(i)::s) rho d)
    |(CMP::xs)                      -> (match s with
                                            Nval(y)::ys -> if (y>0) then (secd xs (Bval(true)::ys) rho d) else (secd xs (Bval(false)::ys) rho d)
                                            |_          -> failwith "CMP" )
    |(PLUS::xs)                     -> (match s with
                                        (Nval(i1)::Nval(i2)::ix) -> (secd xs (Nval(i1+i2)::ix) rho d)
                                        |_           -> failwith "PLUS")
    |(MULT::xs)                     -> (match s with
                                        (Nval(i1)::Nval(i2)::ix) -> (secd xs (Nval(i1*i2)::ix) rho d)
                                        |_           -> failwith "MULT")
    |(CONJ::xs)                     -> (match s with
                                        (Bval(b1)::Bval(b2)::ix) -> (secd xs (Bval(b1&&b2)::ix) rho d)
                                        |_           -> failwith "CONJ")
    |(DISJ::xs)                     -> (match s with
                                        (Bval(b1)::Bval(b2)::ix) -> (secd xs (Bval(b1||b2)::ix) rho d)
                                        |_           -> raise InvalidOperation)
    |(CLOS(x1,y1)::xs)              -> (match x1 with 
                                            [VAR(a)]->(secd xs (Closure(a,y1,rho)::s) rho d)
                                            |_       -> raise InvalidOperation)
    |(APP::xs)                      -> (match s with
                                        (s2::sx) -> (match sx with
                                                    (Closure(x1,y1,rho_d)::s_d) -> (secd y1 [] ([(x1,s2)]@rho_d) ((s_d,rho,xs)::d))
                                                        |_                   -> failwith "APP1")
                                        |_       -> failwith "APP2")
    |(RET::xs)                      -> (match s with
                                        (a::s1) -> (match d with
                                                        ((s_d,rho_d,c_d)::d_d) -> (secd c_d ([a]@s_d) rho_d d_d)
                                                        |_                   -> raise InvalidOperation)
                                        |_      -> raise InvalidOperation)
    |(COND(c1,c2)::xs)              -> (match s with
                                        (Bval(s1)::sx) -> if (s1=true) then (secd (c1@xs) sx rho d) else (secd (c2@xs) sx rho d)
                                        |_     -> raise InvalidOperation)
    );;
(*

Test Cases

let t="\\X.(X+3)(5) + \\X.(X+4)(6)";;

let p1 =  App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4);;

let p2 = If_Then_Else(Cmp (Integer 7),App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31),Integer 0);;

let p3 = If_Then_Else(Cmp (Integer 0),App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),Integer 110);;

let t = "\\X.(X+3)(5) + \\Y.(Y+6)(2*2) + \\Z.(3+1*4)(2)";;

let t = "1 + (if cmp(3) then \\X.(\\Y.(Y+X)(5))(3) else \\Y.(Y+6)(3+1*2) fi)";;
*)
