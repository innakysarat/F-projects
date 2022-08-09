(*let rec eval t env = 
    match t with
    | Const n        -> n
    | Ident          -> Map.find s env
    | Minus          -> -(eval t env) 
    | Add(t1,t2)     -> eval t1 env + eval t2 env
    | Diff(t1,t2)    -> eval t1 env - eval t2 env
    | Prod(t1,t2)    -> eval t1 env * eval t2 env;;*)
    (*| Let(s, t1, t2) -> let v1 = eval t1 env
                        let env1 = Map.add s v1 env
                        eval t2 env1;;*)
    
type aExp =
 | N of int             // Integer value
 | V of string          // Variable
 | WL                   // Length of the word
 | PV of aExp           // Point value of character at specific word index
 | Add of (aExp * aExp) // Addition
 | Sub of (aExp * aExp) // Subtraction
 | Mul of (aExp * aExp) // Multiplication
let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)
let a2 = N 4 .+. (N 5 .-. N 6);;
let rec arithEvalSimple aexp =
   match aexp with
   | N i -> i
   | Add (e1, e2) -> arithEvalSimple e1 + arithEvalSimple e2
   | Sub (e1, e2) -> arithEvalSimple e1 - arithEvalSimple e2
   | Mul (e1, e2) -> arithEvalSimple e1 * arithEvalSimple e2;;
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

type state = Map<string, int>;;
let toInt x : int  =
    match x with
        | None -> 0
        | Some x -> x

let rec arithEvalState aexp st =
    match aexp with
        | N n -> n
        | V v -> toInt (Map.tryFind v st)
        | Add(a, b) ->
            arithEvalState a st +  arithEvalState b st
        | Sub(a, b) ->
            arithEvalState a st - arithEvalState b st
        | Mul(a, b) ->
            arithEvalState a st * arithEvalState b st;;  
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;     

let hello = [('h', 4); ('e', 1); ('l', 1); ('l', 1); ('O', 1)]
   
let rec arithEval aexp w st =
      match aexp with
        | N n -> n
        | V v -> match Map.tryFind v st with
                | Some x -> x
                | None -> 0
        | WL -> List.length w
        | PV aexp -> snd hello.[arithEval aexp w st]
        | Add(a, b) ->
            arithEval a w st +  arithEval b w st
        | Sub(a, b) ->
            arithEval a w st - arithEval b w st
        | Mul(a, b) ->
            arithEval a w st * arithEval b w st;;
            
 
type cExp =
    | C  of char      (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp      (* Character lookup at word index *)

let rec charEval cexp w st =
    match cexp with
    | C cexp -> cexp
    | ToUpper(c) -> System.Char.ToUpper(charEval c w st)
    | ToLower(c) -> System.Char.ToLower(charEval c w st)
    | CV a -> fst hello.[arithEval a w st];;
    
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)
let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
let isVowel (c: char) =
        match c with
        | 'a' | 'e' | 'i' |'o' |'u' | 'y'
        | 'A' | 'E' | 'I' | 'O' | 'U' | 'Y' -> true
        |_ -> false
let rec boolEval bexp w st =
    match bexp with
    | TT -> true
    | FF -> false
    | AEq (a1, a2) -> arithEval a1 w st = arithEval a2 w st
    | ALt (a1, a2) -> arithEval a1 w st < arithEval a2 w st
    | Not b -> boolEval ~~ b w st
    | Conj (b1, b2) -> boolEval b1 w st && boolEval b2 w st
    | IsDigit c -> System.Char.IsDigit(charEval c w st)
    | IsLetter c -> System.Char.IsLetter(charEval c w st)
    | IsVowel c -> isVowel(charEval c w st);;
 
    
        

        
        