module FFalse.Program

open System
open System.Collections.Generic
open FFalse.Lexer
open FFalse.RuntimeStack

[<Literal>]
let TrueValue = -1

[<Literal>]
let FalseValue = 0

[<Literal>]
let AnonymousFunctionPrefix = "<f>_"
       
let eval code =
    let runtimeStack = Stack<StackValue>()
    let runtimeVariables = Dictionary<string, StackValue>()
    let functions = Dictionary<int, TokenType[]>()
    
    let falseAnd (n1, n2) = if n1 = TrueValue && n2 = TrueValue then TrueValue else FalseValue
    
    let falseOr (n1, n2) = if n1 = TrueValue || n2 = TrueValue then TrueValue else FalseValue
    
    let falseEquals (n1, n2) = if n1 = n2 then TrueValue else FalseValue
    
    let falseNegate n = if n = FalseValue then TrueValue else FalseValue
    
    let compileFunctions tokens =
        let (|FunctionBegin|_|) t = match t with | OpenSquareBracket -> Some FunctionBegin | _ -> None
        let (|FunctionEnd|_|) t = match t with | CloseSquareBracket -> Some FunctionEnd | _ -> None
                
        let rec doCompileFunctions tokens compiledTokens functionHandle =
            match tokens with
            | h :: t ->
                match h with
                | FunctionBegin -> compileFunction t compiledTokens  [] (functionHandle + 1)
                | _ -> doCompileFunctions t (h :: compiledTokens) functionHandle
            | [] -> compiledTokens     
        and compileFunction tokens compiledTokens functionTokens functionHandle =
            match tokens with
            | h :: t ->
                match h with
                | FunctionBegin -> doCompileFunctions (h :: t) compiledTokens functionHandle
                | FunctionEnd ->
                    let functionName = AnonymousFunctionPrefix + string functionHandle
                    functions.Add(functionHandle, Array.ofList functionTokens)
                    runtimeVariables.Add(functionName, HandleValue(functionHandle))
                    Identifier(functionName) :: Colon ::compiledTokens 
                | _ -> compileFunction t compiledTokens (h :: functionTokens) functionHandle
            | [] -> failwith "Found unclosed function."
            
        let result = doCompileFunctions tokens [] 0
        
        result
    
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
            | Equals -> popTwoNumbers runtimeStack |> falseEquals |> pushNumber runtimeStack
            | Ampersand -> popTwoNumbers runtimeStack |> falseAnd |> pushNumber runtimeStack
            | Bar -> popTwoNumbers runtimeStack |> falseOr |> pushNumber runtimeStack 
            | Tilde -> popNumber runtimeStack |> falseNegate |> pushNumber runtimeStack
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
        
    lex code |> compileFunctions |> doEval
    ()
    
eval "[1 1 + .]"