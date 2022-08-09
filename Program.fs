
type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)

    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)

    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | Div of aExp * aExp (* NEW: Division *)
    | Mod of aExp * aExp (* NEW: Modulo *)
    | CharToInt of cExp (* Cast to integer *)

and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)

    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)

    | IntToChar of aExp (* Cast to character *)


type bExp =
    | TT (* True *)
    | FF (* False *)

    | AEq of aExp * aExp (* Numeric equality *)
    | ALt of aExp * aExp (* Numeric less than *)

    | Not of bExp (* Boolean not *)
    | Conj of bExp * bExp (* Boolean conjunction *)

    | IsLetter of cExp (* Check for letter *)
    | IsDigit of cExp (* Check for digit *)

type stm =
    | Declare of string (* NEW: Variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* Nop *)
    | Seq of stm * stm (* Sequential composition *)
    | ITE of bExp * stm * stm (* If-Then-Else statement *)
    | While of bExp * stm (* While statement *)

