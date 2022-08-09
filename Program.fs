        // 3 -> sum 5 2 (3 + 2)
        // 2 -> sum 7 1 (5 + 2)
        // 1 -> sum 8 0 (7 + 1)
        // 0 -> 8
        
        // m=2 n=3
        // 2 + 3 + 4 + 5
        
        // 5 + 4 + 3 + 2
        // m = 2; n = 3
        // m=2+3; n=4; i=2
        // m=(2+3)+4; n=5; i=1
        // m=(2+3+4)+5; n=6; i=0
  
  
  // m=0; n=10
  // 0 + 1 + 2 + 3 + 4 + 5 + ... + 10 = 55
  
  // m=2; n=4 -> 2+3+4+5+6
  // m=3; acc=2; i=3
  // m=4; acc=5; i=2
  // acc=9;m=3;i=1
  
let rec sum m n =
    let rec sum_built_in acc m i =
        match i with
        | i when i < 0 -> acc
        | i -> sum_built_in (m+acc) (m+1) (i-1)  
    sum_built_in 0 m n;;
  
let rec length lst =
    let rec len acc lst =
        match lst with
        | [] -> acc
        | head::tail -> len (acc + 1) tail 
    len 0 lst;;

let foldBack f lst acc =
    let rec foldBackAcc lst cont = 
       match lst with
            | []      -> cont acc
            | x :: xs -> foldBackAcc xs (fun r -> cont (f x r))
    foldBackAcc lst (fun x -> x);;
    
//let rec foldBack1 f lst c =
//    let rec foldBackAcc lst c = 
//       match lst with
//            | []      -> c
//            | x :: xs -> f x (foldBack1 f xs c)
//    foldBackAcc lst c;;
    
//let factA x =
//    let rec aux x acc =
//        match x with
//        | 0 -> acc
//        | x -> aux (x - 1) (x * acc)
//    aux x 1;;
    
// c is the continuation(function) into which we put any additional calculations
let factC n =
    let rec fact n cont =
        if n = 0
            then cont 1
        else
            fact (n-1) (fun x -> cont(x * n))
    fact n (fun x -> x);;

#time;