open System

[<Literal>]
let PickCharacter = 'ø';

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
            | [] -> raise (InvalidOperationException("Found unclosed literal."))
            | h :: t -> (t, literal)
    
    let rec doLex code tokens =
        match code with
        | head :: tail -> 
            match parseCharacter head with
            | Some t -> doLex tail (t :: tokens)
            | None ->
                match head with
                | h when Char.IsDigit(h) ->
                    let code, number = scanNumber (head :: tail) ""
                    doLex code (Number(number) :: tokens)
                | h when Char.IsLetter(h) ->
                    let code, identifier = scanIdentifier (head :: tail) ""
                    doLex code (Identifier(identifier) :: tokens)
                | h when h = '"' ->
                    let code, literal = scanLiteral (tail) ""
                    doLex code (Literal(literal) :: tokens)
                | _ -> doLex tail tokens            
        | [] -> tokens
        
    let tokens = List.rev (doLex (List.ofSeq code) [])
    
    for element in tokens do
        printf "%A " element
    
lex "=~][^22\"ala ma kota\"]#%"