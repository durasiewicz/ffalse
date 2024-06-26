open System
open System.Collections.Generic
open System.Diagnostics

[<Literal>]
let PickCharacter = 'ø'

[<Literal>]
let TrueValue = -1

[<Literal>]
let FalseValue = 0

type TokenType =
    | OpenSquareBracket
    | CloseSquareBracket
    | Identifier of value : string
    | Number of value : int
    | Literal of value : string
    | Colon
    | Semicolon
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Equals
    | GreaterThan
    | Ampersand
    | Bar
    | Dollar
    | Percent
    | Backslash
    | At
    | Question
    | Exclamation
    | Tilde
    | Dot
    | Comma
    | Underscore
    | Hash
    | Caret
    | Pick
    | Section

let parseCharacter character =
    match character with
    | '[' -> Some OpenSquareBracket
    | ']' -> Some CloseSquareBracket
    | ':' -> Some Colon
    | ';' -> Some Semicolon
    | '+' -> Some Plus
    | '-' -> Some Minus
    | '*' -> Some Asterisk
    | '/' -> Some Slash
    | '=' -> Some Equals
    | '>' -> Some GreaterThan
    | '&' -> Some Ampersand
    | '|' -> Some Bar
    | '$' -> Some Dollar
    | '%' -> Some Percent
    | '\\' -> Some Backslash
    | '@' -> Some At
    | '?' -> Some Question
    | '!' -> Some Exclamation
    | '~' -> Some Tilde
    | '.' -> Some Dot
    | ',' -> Some Comma
    | '_' -> Some Underscore
    | '#' -> Some Hash
    | '^' -> Some Caret
    | PickCharacter -> Some Pick
    | '§' -> Some Section
    | _ -> None
           
let lex code =
    let rec scanNumber code (number : string) =
        match code with
        | h :: t when Char.IsDigit(h) -> scanNumber t (number + string h)
        | _ -> (code, int number)
        
    let rec scanIdentifier code identifier =
        match code with
        | h :: t when Char.IsLetter(h) -> scanIdentifier t (identifier + string h)
        | _ -> (code, identifier)
        
    let rec scanLiteral code literal =
        match code with
        | h :: t when h <> '"' -> scanLiteral t (literal + string h)
        | _ ->
            match code with
            | [] -> failwith "Found unclosed literal."
            | _ :: t -> (t, literal)
            
    let (|CommentBegin|_|) c =
        match c with
        | '{' -> Some CommentBegin
        | _ -> None
        
    let (|CommentEnd|_|) c =
        match c with
        | '}' -> Some CommentEnd
        | _ -> None
        
    let (|Digit|_|) c =
        match c with
        | c when c |> Char.IsDigit -> Some Digit
        | _ -> None
        
    let (|Letter|_|) c =
        match c with
        | c when c |> Char.IsLetter -> Some Letter
        | _ -> None
        
    let (|Quote|_|) c =
        match c with
        | c when c = '"' -> Some Quote
        | _ -> None
    
    let rec doLex code tokens isInsideComment =
        match code with
        | head :: tail -> 
            match parseCharacter head with
            | Some t -> doLex tail (t :: tokens) isInsideComment
            | None ->
                match head with
                | h when h <> '}' && isInsideComment ->
                    match head with
                    | h when h = '{' -> failwith "Opening comment inside comment is not allowed." 
                    | _ -> doLex tail tokens true 
                | CommentBegin -> doLex tail tokens true
                | CommentEnd ->
                    match isInsideComment with
                    | c when c = true -> doLex tail tokens false
                    | _ -> failwith "Found unbalanced comment closing."
                | Digit ->
                    let code, number = scanNumber (head :: tail) ""
                    doLex code (Number(number) :: tokens) isInsideComment
                | Letter ->
                    let code, identifier = scanIdentifier (head :: tail) ""
                    doLex code (Identifier(identifier) :: tokens) isInsideComment
                | Quote ->
                    let code, literal = scanLiteral (tail) ""
                    doLex code (Literal(literal) :: tokens) isInsideComment
                | _ -> doLex tail tokens isInsideComment            
        | [] -> tokens
        
    List.rev (doLex (List.ofSeq code) [] false)
   

type StackValue =
    | NumberValue of int
    | ReferenceValue of string
    
let popAny (stack : Stack<StackValue>) : StackValue =
    let mutable value : StackValue = NumberValue(0)
    
    if stack.TryPop(&value) then
        value
    else
        failwith "Failed to pop from runtime stack."
    
let popNumber stack =
    match popAny stack with
    | NumberValue v -> v
    | _ -> failwith "Failed to pop number value from runtime stack."
    
let popTwoNumbers stack = (popNumber stack, popNumber stack)

let popThreeNumbers stack = (popNumber stack, popNumber stack, popNumber stack)

let pushNumber (stack : Stack<StackValue>) number = stack.Push(NumberValue(number))

let pushReference (stack : Stack<StackValue>) name = stack.Push(ReferenceValue(name))

let popReference (stack : Stack<StackValue>) =
    match popAny stack with
    | ReferenceValue v -> v
    | _ -> failwith "Failed to pop reference value from runtime stack."

let pushAny (stack : Stack<StackValue>) item = stack.Push(item)

let peekAny (stack : Stack<StackValue>) index = stack.ToArray()[index]
    
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