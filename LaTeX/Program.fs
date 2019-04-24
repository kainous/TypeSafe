open System

type Token =
| Normal  of string
| Escaped of string
| Comment of string

let Tokenize (text:string) =
  let rec getEscaped (cont : char list -> char list) = function
  | [] -> Escaped (cont [] |> Escaped, [])
  | ch::tail when Char.IsWhiteSpace ch -> ((cont [] |> Escaped), tail)
  | ch::tail -> getEscaped (fun acc -> ch::acc |> cont) tail
  
  let rec go (cont : Token list -> Token list) = function
  | '\\'::tail -> getEscaped id tail
  | '\\'::'\\'::tail -> Escaped("\\\\")s

  text |> Seq.toList |> go id


[<EntryPoint>]
let main argv =
  printfn "Hello World from F#!"
  0 // return an integer exit code