(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

let rec assoc (d,k,l) = 
 if l = [] then d
  else let (x,y)::t = l in
    match x = k with
    |true -> y
    |false -> assoc (d,k,t);;


(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' =  match List.mem h seen with
		   |true -> seen
		   |false -> h::seen 
	 in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
   let (b',y) = f b in
   match y with 
   |false -> b'
   |true -> wwhile (f,b');;

(* fill in the code wherever it says : failwith "to be written" *)
let fixpoint (f,b) = wwhile ((fun w -> if (f w) = w then ((f w), false) else ((f w), true)),b);;

(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
