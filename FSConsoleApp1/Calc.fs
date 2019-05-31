module Calc

open System

(*
 * A calculator expression is "x op y"
 * Quick explanation of the calculator state machine:
 * The initial state is "waiting for x" (wfx for short)
 * Upon receiving x, the new state is "waiting for operator" (wfop)
 * - The wfop state contains the value for x
 * Upon receiving the operator, the new state is wfy
 * - The wfy state contains the values for x and op
 * Upon receiving y, the result is calculated, printed, and put into x. The new state is wfop.
 * This loop continues until something invalid is entered as a number or operator
*)

type Operator =
    | Plus
    | Minus
    | Times
    | DividedBy
    | Invalid

type CalcState = 
    | WaitingForX
    | WaitingForOp of float
    | WaitingForY of float * Operator
    | Finished

let toOperator s =
    match s with
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Times
    | "/" -> DividedBy
    | _ -> Invalid

let progressFromWfx =
    printfn "Give a number"
    let input = Console.ReadLine()
    let inputAsFloat = Utils.parseFloat input
    match inputAsFloat with
    | Some(n) -> WaitingForOp n
    | None -> Finished

let progressFromWfop x =
    printfn "Give an operator (+, -, *, /)"
    let input = Console.ReadLine()
    let inputAsOperator = toOperator input
    match inputAsOperator with
    | Invalid -> Finished
    | op -> WaitingForY (x, op)

let calculate x op y =
    match op with
    | Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | DividedBy -> x / y
    | _ -> 0.0

let transitionToWfop x op y =
    let result = calculate x op y
    printfn "Current result: %A" result
    WaitingForOp result

let progressFromWfy x op =
    printfn "Give a number"
    let input = Console.ReadLine()
    let inputAsFloat = Utils.parseFloat input
    match inputAsFloat with
    | Some(n) -> transitionToWfop x op n
    | None -> Finished
    
let nextState state =
    match state with
    | WaitingForX -> progressFromWfx
    | WaitingForOp(x) -> progressFromWfop x
    | WaitingForY(x, op) -> progressFromWfy x op
    | Finished -> Finished

let startLoop = fun () ->
    // TODO: Is there a better way? Unfold needs both next element and next state
    let inputSeq = Seq.unfold (fun state ->
        match state with
        | Finished -> None
        | unfinishedState ->
            let newState = nextState unfinishedState
            Some (newState, newState)) WaitingForX

    for _ in inputSeq do ()


let calcMain = fun () ->
    printfn "I am a basic calculator"

    startLoop()

    printfn "Since you entered something invalid, I give up"
    0
