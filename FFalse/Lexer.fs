module FFalse.Lexer

open System

[<Literal>]
let PickCharacter = 'ø'

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
