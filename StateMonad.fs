module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = failwith "Not implemented"      

    let wordLength : SM<int> = failwith "Not implemented"      

    let characterValue (pos : int) : SM<char> = failwith "Not implemented"      

    let pointValue (pos : int) : SM<int> = failwith "Not implemented"      

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = failwith "Not implemented"
    
    let update (var : string) (value : int) : SM<unit> =
        let rec aux =
            function
            | []      -> None
            // m - current list (environment)
            // ms - last list(environment/scope) of variables
            | m :: ms -> 
                match Map.tryFind var m with
                // _ is a previous value of variable "var" in the list "m"
                | Some _ ->
                    // make a new environment by changing the existing one
                    // we change the variable(=var) with the value(=value) in the environment m
                    // an environment stands for a list/scope of variables
                    let env' = Map.add var value m
                    // we want to add the changed list (env') to others
                    match aux ms with
                    | Some ms -> Some (env'::ms)
                    // fix this!!
                    | None -> Some (env'::ms)
                // if we haven't found a variable in the current list
                // then we just move on keeping our current list untouched
                | None   ->
                    match aux ms with
                    | Some ms -> Some (m::ms)
                    | None -> None
        S (fun s -> 
              match aux (s.vars) with
              | Some vars' -> Success ((), {s with vars = vars'})
              | None   -> Failure (VarNotFound var))      
              

    