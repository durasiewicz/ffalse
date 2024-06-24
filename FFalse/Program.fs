[<Literal>]
let Pick = 'ø';

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

let lex code =
    let rec doLex code tokens =
        match code with
        | head :: tail ->
            printfn $"%c{head} "                
            doLex tail tokens
        | [] -> ()
    doLex (List.ofSeq code)
    
lex "{  }" |> ignore