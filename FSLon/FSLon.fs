module FSLon.FSLon

open FParsec

type LValue =
    | LNil
    | LBoolean of bool
    | LNumber of float
    | LString of string
    | LTable of Map<LValue, LValue>

let internal lnil = stringReturn "nil" LNil
let internal lbool = (stringReturn "true" (LBoolean true)) <|> (stringReturn "false" (LBoolean false))
let internal lnumber = pfloat |>> LNumber
let internal lstring = 
    // stringLiteral based on the FParsec documentation's JSON string function
    let stringLiteral quoteChar =
        let escape = anyOf (string quoteChar + "\\/abfnrtv")
                        |>> function
                            | 'a' -> "\a"
                            | 'b' -> "\b"
                            | 'f' -> "\u000C"
                            | 'n' -> "\n"
                            | 'r' -> "\r"
                            | 't' -> "\t"
                            | 'v' -> "\v"
                            | c   -> string c // every other char is mapped to itself
        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
                |> char |> string)
        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet = manySatisfy (fun c -> c <> quoteChar && c <> '\\')
        between (pstring (string quoteChar)) (pstring (string quoteChar)) (stringsSepBy normalCharSnippet escapedCharSnippet)
    ((stringLiteral '"') <|> (stringLiteral '\'')) |>> LString
let internal lname = manySatisfy isLetter |>> LString
let internal lvalue, internal lvalueRef = createParserForwardedToRef<LValue, unit>()
let internal ltable =
    let enclosedList sOpen sEnd pElement f =
        between (pstring sOpen) (pstring sEnd)
                (spaces >>. sepEndBy (pElement .>> spaces) (((pstring ";") <|> (pstring ",")) >>. spaces) |>> f)
    let fieldKey =
        between (pstring "[") (pstring "]")
                (spaces >>. lvalue .>> spaces)
    let nameKey = (spaces >>. lname .>> spaces)      
    let keyValue = (spaces >>. (lvalue <|> fieldKey <|> nameKey)) .>>. 
                   (spaces >>. pstring "=" >>. spaces >>. lvalue)
    let justValue = (preturn LNil) .>>. (spaces >>. lvalue .>> spaces)
    enclosedList "{" "}" (justValue <|> keyValue) (Map.ofList >> LTable)

do lvalueRef := choice [lnil; lbool; lnumber; lstring; ltable]

let lparser = spaces >>. lvalue .>> spaces .>> eof
