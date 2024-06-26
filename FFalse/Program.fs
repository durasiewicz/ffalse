module FFalse.Program

open System
open System.Collections.Generic
open FFalse.Lexer
open FFalse.RuntimeStack

[<Literal>]
let TrueValue = -1

[<Literal>]
let FalseValue = 0
       
let eval code =
    let runtimeStack = Stack<StackValue>()
    let runtimeVariables = Dictionary<string, StackValue>()
    
    let rec doEval tokens =
        match tokens with
        | h :: t ->
            match h with
            | Plus | Minus | Asterisk | Slash ->
                let number2, number1 = popTwoNumbers runtimeStack
                let result = match h with
                             | Plus -> number1 + number2
                             | Minus -> number1 - number2
                             | Asterisk -> number1 * number2
                             | Slash -> number1 / number2            
                             | _ -> failwith ""
                             
                pushNumber  runtimeStack result
            | Equals ->
                let number2, number1 = popTwoNumbers runtimeStack
                (if number1 = number2 then TrueValue else FalseValue) |> pushNumber runtimeStack
            | Ampersand ->
                let number2, number1 = popTwoNumbers runtimeStack
                (if number1 = TrueValue && number2 = TrueValue then TrueValue else FalseValue) |> pushNumber runtimeStack
            | Bar ->
                let number2, number1 = popTwoNumbers runtimeStack
                (if number1 = TrueValue || number2 = TrueValue then TrueValue else FalseValue) |> pushNumber runtimeStack 
            | Tilde ->
                let number1 = popNumber runtimeStack
                (if number1 = FalseValue then TrueValue else FalseValue) |>  pushNumber runtimeStack
            | Number value -> value |> pushNumber runtimeStack 
            | Dot -> popNumber runtimeStack |> printf "%d"
            | Dollar ->
                let number = popNumber runtimeStack
                pushNumber runtimeStack number
                pushNumber runtimeStack number
            | Backslash ->
                let number2, number1 = popTwoNumbers runtimeStack
                pushNumber runtimeStack number2
                pushNumber runtimeStack number1
            | Pick -> popNumber runtimeStack |> peekAny runtimeStack |> pushAny runtimeStack
            | At ->
                let number3, number2, number1 = popThreeNumbers runtimeStack
                pushNumber runtimeStack number3
                pushNumber runtimeStack number2
                pushNumber runtimeStack number1
            | Identifier i -> i |> pushReference runtimeStack
            | Colon ->
                let ref = popReference runtimeStack
                let value = popNumber runtimeStack
                runtimeVariables[ref] <- NumberValue(value)
                ()
            | Semicolon ->
                let ref = popReference runtimeStack
                runtimeVariables.TryAdd(ref, NumberValue(0)) |> ignore
                match runtimeVariables[ref] with
                | NumberValue v -> v |> pushNumber runtimeStack
                | _ -> ()
            | t -> raise (NotImplementedException(string t))
            doEval t
        | _ -> ()
        
    lex code |> doEval
    ()
    
eval "{} 3a:a;."