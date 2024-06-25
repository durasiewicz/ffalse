[<Literal>]
let PickCharacter = 'ø';

type TokenType =
    | OpenSquareBracket
    | CloseSquareBracket
    | Identifier
    | Number
    | Literal
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
    let rec doLex code tokens =
        match code with
        | head :: tail -> 
            match parseCharacter head with
            | Some t -> doLex tail (t :: tokens)
            | None -> doLex tail tokens            
        | [] -> tokens
        
    let tokens = List.rev (doLex (List.ofSeq code) [])
    
    for element in tokens do
        printf "%A " element
    
lex "=~][^]#%"