(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr
  | Triple   of expr * expr * expr          (*computes multiplication of 3 expressions*)
  | Mean     of expr * expr * expr	    (*sums up 3 expressions then divides by 3*)

(* exprToString: expr -> string
*  (exprToString e) is the string printing of an expression
*)
let rec exprToString e = 
   match e with
   |VarX -> "x"
   |VarY -> "y"
   |Sine s -> "sin(pi*" ^ exprToString s ^ ")"
   |Cosine c -> "cos(pi*" ^ exprToString c ^ ")"
   |Average (a,b) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ ")/2)"
   |Times (c,d) -> exprToString c ^ "*" ^ exprToString d
   |Thresh (f,g,h,i) -> "(" ^ exprToString f ^ "<" ^ exprToString g ^ "?" ^ 
                         exprToString h ^ ":" ^ exprToString i ^ ")"
   |Triple (j,k,p) -> "(" ^ exprToString j ^ "*" ^ exprToString k ^ "*" ^ exprToString p ^ ")"
   |Mean (l,m,n) -> "((" ^ exprToString l ^ "+" ^ exprToString m ^ "+" ^
                         exprToString n ^ ")/3)";; 


(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildTriple(e1,e2,e3)          = Triple(e1,e2,e3)
let buildMean(e1,e2,e3)            = Mean(e1,e2,e3)


let pi = 4.0 *. atan 1.0

(* eval: expr * float * float -> float
*  (eval (e,x,y)) is the evaluation of the expression e at the points x and y
*)
let rec eval (e,x,y) = 
  match e with
  |VarX -> x
  |VarY -> y
  |Sine s -> sin(pi *. (eval (s,x,y)))
  |Cosine c -> cos(pi *. (eval (c,x,y)))
  |Average (a,b) -> (((eval (a,x,y)) +. (eval (b,x,y)))/.2.0)
  |Times (c,d) -> ((eval (c,x,y)) *. (eval (d,x,y)))
  |Thresh (f,g,h,i) -> if (eval (f,x,y)) < (eval (g,x,y)) then
                       eval (h,x,y) 
                       else eval (i,x,y)
  |Triple (j,k,p) -> ((eval (j,x,y)) *. (eval (k,x,y)) *. (eval (p,x,y))) 
  |Mean (l,m,n) -> (((eval (l,x,y)) +. (eval (m,x,y)) +. (eval (n,x,y)))/.3.0);;


(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))

