open System.Diagnostics

module NaturalNumbers =
  [<DebuggerDisplay("{ToString(),nq}")>]
  type Nat = Z | S of Nat with
    member this.ToNumber() =
      let rec go = function
      | Z -> 0
      | S x -> 1 + go x

      go this

    override this.ToString() =
      this.ToNumber().ToString()

    static member (+) (a, b) =
      match a, b with
      | Z, y   -> y
      | S x, y -> S (x + y)

    static member (*) (a, b) =
      match a, b with
      | Z, _   -> Z
      | S x, y -> (x * y) + y


  let rec fact = function
  | Z -> S Z
  | S x -> (S x) * fact x

  let rec toNum = function
  | Z -> 0
  | S x -> 1 + toNum x

  let rec fromNum = function
  | 0 -> Z
  | x -> S (fromNum (x - 1))

open NaturalNumbers

[<EntryPoint>]
let main argv = 
  let a1 = fromNum 3
  let a2 = fromNum 4
  let a3 = a1 * a2

  let a = fromNum 6
  let b = fact a
  let c = toNum b

  printfn "%A" c
  0 // return an integer exit code
