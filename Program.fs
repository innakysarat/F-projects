//open System
let rec downto1 x =
   if x <= 0 then []
   else x::downto1 (x-1);;
    
let rec downto2 =
   function
   | x when x <= 0 -> []
   | x -> x::downto2(x-1);;
let rec removeOddIdx =
   function
      | [] -> []
      | [x] -> [x]
      | x::y::z -> x::removeOddIdx z;;

let rec combinePair =
   function
      | [] -> []
      | [x] -> []
      | x::y::z -> (x, y)::combinePair z;;

type complex = float * float;;
let mkComplex x y:complex = (x, y);;
//let mkComplex x y = (x, y):complex;;
let complexToPair (a,b)= (a,b):complex;;
let (|+|) ((a,b):complex) ((c,d):complex) = (a+c, b+d);;

let (|*|) ((a,b):complex) ((c,d):complex) = (a*c - b*d, b*c + a*d);;

let (|-|) ((a,b):complex) ((c,d):complex) = (a,b) |+| (-c,-d);;

(*let (|/|) ((a,b):complex) ((c,d):complex) =
   if a <> 0 || b <> 0 then (a,b) |*| (c/(c*c + d*d),-d/(c*c + d*d))
   else raise (InvalidOperationException("undefined"));;*)
let (|/|) ((a,b):complex) ((c,d):complex) =
   (a,b) |*| (c/(c*c + d*d),-d/(c*c + d*d));;   
     
let explode1(s:string) =
       s.ToCharArray() |> Array.toList;;
   
let rec explode2 =
       function
          | "" -> [] 
          | s -> s.Chars(0)::explode2 (s.Remove(0, 1));;
  
let (.+) (x1:char) (x2:string) = System.Char.ToString(x1) + x2;;
       
let implode(lst:char list) =
   List.foldBack (fun x xs -> string x + xs) lst "";;

let implodeRev(lst:char list) =
   List.fold (fun x xs -> string xs + x) "" lst;;
   
let toUpper (s:string) = s.ToCharArray() |> Array.toList |> List.map System.Char.ToUpper |> implode;;

let rec ack (m, n) =
   if m = 0 then
      n + 1
   elif m > 0 && n = 0 then
      ack(m-1,1)
   elif m > 0 && n > 0 then
      ack(m-1, ack(m, n-1))
   else
      0;;
     