open FSLon
open FParsec

let test = """{
    ["a"] = 1;
    ["b"] = 2;
    ["c"] = 50;
    [4] = 100,
    30;
    20,
    {1,2,3},
    something = true,
    else = false,
    nothing = nil,
    x = 'singleQuoteString',
    y = "DoubleQuoteString"
}"""

let result = run FSLon.lparser test

printf "%+A" result

System.Console.Read() |> ignore
