﻿module Calc

open System

type Operator =
    | Plus
    | Minus
    | Times
    | DividedBy
    | Invalid

// A calculator expression is "x op y"
type CalcState = 
    | WaitingForX
    | WaitingForOp of float
    | WaitingForY of float * Operator
    | Finished

let to_operator s =
    match s with
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Times
    | "/" -> DividedBy
    | _ -> Invalid

let progress_from_wfx =
    printfn "Give a number"
    let input = Console.ReadLine()
    let inputAsFloat = Utils.parseFloat input
    match inputAsFloat with
    | Some(n) -> WaitingForOp n
    | None -> Finished

let progress_from_wfop x =
    printfn "Give an operator (+, -, *, /)"
    let input = Console.ReadLine()
    let inputAsOperator = to_operator input
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

let transition_to_wfop x op y =
    let result = calculate x op y
    printfn "Current result: %A" result
    WaitingForOp result

let progress_from_wfy x op =
    printfn "Give a number"
    let input = Console.ReadLine()
    let inputAsFloat = Utils.parseFloat input
    match inputAsFloat with
    | Some(n) -> transition_to_wfop x op n
    | None -> Finished
    
let next_state state =
    match state with
    | WaitingForX -> progress_from_wfx
    | WaitingForOp(x) -> progress_from_wfop x
    | WaitingForY(x, op) -> progress_from_wfy x op
    | Finished -> Finished

let calc_main =
    printfn "I am a basic calculator"

    // TODO: Is there a better way? Unfold needs both next element and next state
    let input_seq = Seq.unfold (fun state ->
        match state with
        | Finished -> None
        | unfinished_state ->
            let new_state = next_state unfinished_state
            Some (new_state, new_state)) WaitingForX

    for _ in input_seq do ()
        
    printfn "Since you entered something invalid, I give up"
    0