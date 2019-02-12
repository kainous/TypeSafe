open System
open System.Text
open System.Collections.Generic

type Collat =
| A
| B
| O

let rec Collatz1 value =
  if value &&& 1I = 0I then
    value >>> 1
  else
    value * 3I + 1I

let encode2 value =
  let rec go value cont =
    if value = 0I then
      cont []
    else if value &&& 1I = 0I then
      go (value >>> 1) (fun acc -> cont (O::acc))
    else if value &&& 3I = 3I then
      go (value >>> 2) (fun acc -> cont (B::acc))
    else
      go (value >>> 2) (fun acc -> cont (A::acc))

  go value id

let encode3 value =
  let r = encode2 value
  let rec go value current count cont =
    match value with
    | h::t when h = current -> go t current (count + 1) cont
    | h::t -> go t h 1 (fun acc -> cont ((count, current)::acc))
    | [] -> 
      if (current = O) then
        cont []
      else
        (fun acc -> cont ((count, current)::acc)) []

  let (_::r2) = go r O 1 id
  r2

let encode value =
  let sb = StringBuilder()
  let rec go value =
    if value = 0I then
      value
    else if value &&& 1I = 0I then
      sb.Append " " |> ignore
      go (value >>> 1)
    else if value &&& 3I = 3I then
      sb.Append "B" |> ignore
      go (value >>> 2)
    else
      sb.Append "A" |> ignore
      go (value >>> 2)
    
  go value |> ignore
  let aas = sb.ToString().ToCharArray() //|> Seq.rev
  aas |> Seq.toArray |> System.String |> fun x -> x.Trim() |> fun x -> x.Replace(' ', '0')

let decode value =
  let rec go result = function
  | [] -> result
  | (0, _)::t -> go result t
  | (n, O)::t -> go (result <<< 1) ((n - 1, O)::t)
  | (n, A)::t -> go (1I + (result <<< 2)) (((n - 1), A)::t)
  | (n, B)::t -> go (3I + (result <<< 2)) (((n - 1), B)::t)

  go 0I (value |> List.rev)

let (|OBJ'|) collat =
  let rec go result = function
  | (n, v)::(((_, u)::_) as t) when v = collat && u = collat -> go (result + n) t
  | (n, v)::t when v = collat -> (result + n, t)
  | t -> result, t
  go 0

let (|A'|) = (|OBJ'|) A
let (|B'|) = (|OBJ'|) B
let (|O'|) = (|OBJ'|) O

type Reduction = {
  Cover : (int * Collat) list
  KnownClosure : (int * Collat) list
}

// Needs something that supports 0 as a pattern
let rec reduce = function
| (0, _)::t -> reduce t
| A' (_, [])               -> { KnownClosure = [1, A]; Cover = [1, A] }
| A' (_, [(1, B)])         -> { KnownClosure = [1, A]; Cover = [2, A] }
| [(1, A); (2, O); (1, A)] -> { KnownClosure = [1, A]; Cover = [1, A; 1, B] }

| [(1, B); (1, O); (1, A)] -> [1, A] (* [(1, A); (2, O); (1, A)] *)
| [(1, B); (1, A)] -> [1, A] (* [(1, B); (1, O); (1, A)] *)

| [(1, B)] -> [1, A] (* [(* (0, B); *) (2, A)] *)
| [(1, A); (1, O); (1, A)] -> [1, A] // [(1, B); (1, A)]

//Doesn't yet reduce

| [(n, B)] -> [(n - 1, B); (2, A)]
| [(1, A); (n, O); (1, A)] when n > 2 -> [(1, A); (n - 2, O); (1, B)]

| [(1, A); (n, O); (1, B)] when n > 2 -> [(1, A); (n - 2, O); (1, A); (1, O); (1, A)]
| [(1, A); (2, O); (1, B)]            -> [(2, A); (1, O); (1, A)]
| [(1, A); (1, O); (1, B)]            -> [(1, B); (2, O); (1, A)]
//[(1, A); (0, O); (1, B)] is already another case




// (suspect) | [(2, A); (1, O); (1, A)] -> [(1, B); (1, A)]






// TODO The resulting output pattern is not found earlier, and therefore doesn't reduce
//| [(1, B); (2, O); (1, A)] -> [(1, A); (1, B); (1, A)]




//| [(1, A); (n, O); (1, B)] when n >= 2 -> [(1, A); (n - 2, O); (1, A); (1, O); (1, A)]
//| [(1, B); (1, O); (1, B)] -> [(1, A); (1, O); (2, A)]
//| [(1, A); (1, B); (1, A)] -> [(1, B); (1, O); (1, A)]
//
//| [(1, B); (2, A)] -> [(1, B); (3, O); (1, A)]

// AA0A -> BA
// A0AA -> AB


| unhandled -> unhandled |> sprintf "unhandled %A" |> failwith 

let equivClass value =
  let rec go input =
    if input = 1I then
      input
    elif (input &&& 1I) = 0I then
      go (input >>> 1)
    else
      input
  go value

[<EntryPoint>]
let main argv =
  let hashSet = HashSet()


  let t1 = (decode [(1, A); (1, O); (1, B)])
  printfn "%A" t1

  for i in [t1..t1] do
    let mutable result = i |> equivClass
    let test = (decode (encode3 result))
    if result <> test then
      printfn "%A <> %A" result test
      assert (false)
    if not (hashSet.Contains result) then
      hashSet.Add result |> ignore

      let r1 = encode3 result
      let r2 = encode3 (Collatz1 result |> equivClass)
      
      try       
        let r3 = reduce r1
        if r3 <> [(1, A)] then
          printfn "%A = %A -> %A" result r1 r3
      with
      | _ ->
        printfn "* FAIL - %A → %A" r1 r2

  0 // return an integer exit code
