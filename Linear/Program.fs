// Learn more about F# at http://fsharp.org

open System

type Prop = bool
type Var = string

type Closed<'T> = // Object
| Prop of Prop
| One
| Top
| Tensor   of Closed<'T> * Closed<'T>
| With     of Closed<'T> * Closed<'T>
| Least    of Var * Open<'T>
| Greatest of Var * Open<'T>
and Open<'T> = // Endofunctors
| Var of Var
| Closed of Closed<'T>
| Function of ('T -> 'T)
| OpenTensor of Open<'T> * Open<'T>
| OpenWith   of Open<'T> * Open<'T>

let rec Recurse a = function
| OpenTensor(Function F, Function G) -> Tensor(F a, G a)
| OpenWith(Function F, Function G) -> With(F a, G a)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
