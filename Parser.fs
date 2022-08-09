module ImpParser

    open Eval
    open System

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"
    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy Char.IsLetter <?> "letter"
    
    let palphanumeric  = satisfy Char.IsLetterOrDigit <?> "alphanumeric"
    let pletters = many palphanumeric |>> fun (charList) -> String.Concat(Array.ofList(charList))
    

    let anyOf ss =
        ss |> List.map pchar |> choice <?> 
        sprintf "anyOf %A" ss

    let parenthesiseCharList = ['(' ; ')'; ' ']
    let parenthesiseChar = anyOf parenthesiseCharList
    let parentesiseHelper = many parenthesiseChar
    let underscore = anyOf ['_']
    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let braces p = pchar '{' >*>. p .>*> pchar '}' 
    let second_part = many (palphanumeric <|> underscore)  |>> fun (charList) -> String.Concat(Array.ofList(charList))
    let pid = pletter <|> underscore .>>. second_part |>> fun (c1, s2) -> String.Concat(c1, s2)

    
    let unop parser1 parser2 =
         parser1 >*>. parser2
    let binop p1 p2 p3 = (p2 .>*> p1) .>*>. p3
    
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, charref = createParserForwardedToRef<cExp>()
    
    let AexpParse = TermParse
    let CexpParse = CharParse

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
     
    // <?> in case of failure write to the console a message "Int"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse =  pid |>> V <?> "Var"
    let NegParse = pchar '-' >>. AexpParse |>> (fun x -> Mul (N -1, x)) <?>  "Negation"
    //let NegParse = pchar '-' >>. pint32 |>> (fun x -> Mul (N -1, N x)) <?>  "Negation"
    let PVParse = pstring "pointValue"  >*>. parenthesise AexpParse |>> PV <?> "PV"
    let CharToInt = pstring "charToInt" >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    let ParParse = parenthesise TermParse
    do aref := choice [CharToInt; NegParse; PVParse; NParse; VParse; ParParse]
    
    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"
    let LowerParse = pstring "toLower" >*>. parenthesise CexpParse |>> ToLower <?> "toLower"
    let UpperParse = pstring "toUpper" >*>. parenthesise CexpParse |>> ToUpper <?> "toUpper"
    let CharValue = pstring "charValue" >*>. parenthesise AexpParse |>> CV <?> "CV"
    let IntToChar = pstring "intToChar" >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"
    do charref := choice [CParse; LowerParse; UpperParse; CharValue; IntToChar]

    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()
    // do I need BAtomParse?
    let BAtomParse, baref = createParserForwardedToRef<bExp>()
    
    let BexpParse = BTermParse
    
    // or BAtom, BProd
    let ConjParse = binop (pstring @"/\")  BProdParse BTermParse |>> Conj <?> "Conjunction"
    let DisjParse = binop (pstring @"\/") BProdParse BTermParse |>> (fun (x, y) -> Not(Conj(Not x, Not y))) <?> "Disjunction"
    do btref := choice [ConjParse; DisjParse; BProdParse]
    
    
    let EqParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "Equal"
    let NotEqParse = binop (pstring @"<>") AexpParse AexpParse |>> (fun (x, y) -> Not(AEq(x, y))) <?> "Not equal"
    let LessParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "Less than"
    // Conj(Not(ALt (x, y)), Not(AEq (x,y))) -> not "<" and not "="
    // Not(Conj(Not(ALt (x, y)), Not(AEq (x,y)))))  -> not (not "<" and not "=") == "<" or "="
    // Not (Conj (Not (ALt (N 6,N 3)),Not (Not (Not (AEq (N 6,N 3))))))
    let LessOrEqParse = binop (pstring @"<=") AexpParse AexpParse |>> (fun (x, y) ->
                                                                Not(Conj(Not(ALt (x, y)), Not(AEq (x,y))))) <?> "Less or Equal"
    let MoreOrEqParse = binop (pstring @"=>") AexpParse AexpParse |>> (fun (x, y) -> Not(ALt (x, y))) <?> "More or equal"
    // ">" == not "==" and not "<"
    let MoreParse = binop (pchar '>') AexpParse AexpParse |>> fun (x,y) -> Conj (Not (AEq (x, y)), Not (ALt (x ,y)))
    do bpref := choice [NotEqParse; EqParse; LessParse; LessOrEqParse; MoreOrEqParse; MoreParse; BAtomParse]
    
    let NotParse = unop (pchar '~') BexpParse |>> Not <?> "Not"
    let TrueParse = pTrue |>> (fun _ -> TT) <?> "True"
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "False"
    let BParParse = parenthesise BTermParse
    do baref := choice [NotParse; TrueParse; FalseParse; BParParse]
    
    
    let stmTermParse, stmtref = createParserForwardedToRef<stm>()
    let stmProdParse, stmpref = createParserForwardedToRef<stm>()

    let stmntParse = stmTermParse
    //let stmntParse, stmtref = createParserForwardedToRef<stm>()
    //let stmProdParse, stmpref = createParserForwardedToRef<stm>()
    
    let SeqParse = stmProdParse .>*> pchar ';' .>*>. stmTermParse |>> Seq <?> "Sequential"
    do stmpref := choice [SeqParse; stmProdParse]
    // one space between the keyword and the identifier
    let DeclareParse = pdeclare .>>. whitespaceChar >*>. pid |>> Declare <?> "Declare"
    let AssignParse =  binop (pstring ":=") pid AexpParse |>> Ass <?> "Ass"
    let ITEParse = pif >*>. parenthesise BexpParse .>*> pthen
                   .>*>. braces stmntParse .>*> pelse .>*>. braces stmntParse
                   |>> (fun ((x, y), z) -> ITE(x, y, z)) <?> "If then else"
    let ITParse = pif >*>. parenthesise BexpParse .>*> pthen
                   .>*>. braces stmntParse
                   |>> (fun (x, y) -> ITE(x, y, Skip)) <?> "If then"
                   
    let WhileParse = pwhile >*>. parenthesise BexpParse .>*> pdo
                     .>*>. braces stmntParse
                     |>> While <?> "While"
    do stmtref := choice [DeclareParse; AssignParse; ITEParse; ITParse; WhileParse]
    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }
    //let parseBoardProg (bp : boardProg) : board = failwith "not implemented"

