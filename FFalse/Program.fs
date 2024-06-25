open System
open System.Collections.Generic

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
            | h :: t -> (t, literal)
    
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
                | h when h = '{' -> doLex tail tokens true
                | h when h = '}' ->
                    match isInsideComment with
                    | c when c = true -> doLex tail tokens false
                    | _ -> failwith "Found unbalanced comment closing."
                | h when Char.IsDigit(h) ->
                    let code, number = scanNumber (head :: tail) ""
                    doLex code (Number(number) :: tokens) isInsideComment
                | h when Char.IsLetter(h) ->
                    let code, identifier = scanIdentifier (head :: tail) ""
                    doLex code (Identifier(identifier) :: tokens) isInsideComment
                | h when h = '"' ->
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
    
let pushNumber number (stack : Stack<StackValue>) =
   stack.Push(NumberValue(number))
    

let eval code =
    let runtimeStack = new Stack<StackValue>()
    let tokens = lex code
    
    let rec doEval tokens =
        match tokens with
        | h :: t ->
            match h with
            | Number value -> pushNumber value runtimeStack
            | Plus ->
                let number1 = popNumber runtimeStack
                let number2 = popNumber runtimeStack 
                pushNumber (number1 + number2) runtimeStack
            | Dot ->
                popNumber runtimeStack |> printf "%d"
            | t -> raise (NotImplementedException(string t))
            doEval t
        | _ -> ()
    doEval tokens
    ()
    
eval "{} 2 3 + ."