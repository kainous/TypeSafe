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

let (|A''|) = function
| A'(_, tail) -> tail

type Reduction = {
  Cover : (int * Collat) list
  KnownClosure : (int * Collat) list
}

// Needs something that supports 0 as a pattern
let rec reduce =
  let rec go2 initial = function
  | (0, _)::t -> go2 initial t
  | [1, A] -> { KnownClosure = initial@[1, B]; Cover = initial@[1, B] }
  | unhandled -> unhandled |> sprintf "unhandled %A" |> failwith 

  let rec go1 = function
  | [0, O]          -> { KnownClosure = [1, A]; Cover = [1, A] }
  | (0, _)::t -> go1 t
  | A''[]                 -> { KnownClosure = [1, A]; Cover = [1, A] }
  | A''[1, B]             -> { KnownClosure = [1, A]; Cover = [2, A] }
  | A''[1, O; 1, A]       -> { KnownClosure = [1, A]; Cover = [1, B; 1, A] }
  | A''[2, O; 1, A]       -> { KnownClosure = [1, A]; Cover = [1, A; 1, B] }
  | A''[3, O; 1, A]       -> { KnownClosure = [1, A]; Cover = [1, A; 1, O; 1, B] }
  | A''[4, O; 1, A]       -> { KnownClosure = [1, A]; Cover = [1, A; 2, O; 1, B] }
   
  | A''[1, O; 1, B]       -> { KnownClosure = [1, A]; Cover = [1, B; 2, O; 1, A] }
  | A''[2, O; 1, B]       -> { KnownClosure = [1, A]; Cover = [2, A; 1, O; 1, A] }
  
  | A''[1, B; 1, A]       -> { KnownClosure = [1, A]; Cover = [1, B; 1, O; 1, A] }
  | A''[1, B; 2, A]       -> { KnownClosure = [1, A]; Cover = [1, B; 3, O; 1, A] }
  | A''[1, B; 3, A]       -> { KnownClosure = [1, A]; Cover = [1, B; 5, O; 1, A] }

  | A''[1, B;       1, A] -> { KnownClosure = [1, A]; Cover = [1, B; 1, O; 1, A] }
  | A''[1, B; 1, O; 1, A] -> { KnownClosure = [1, A]; Cover = [1, A; 2, O; 1, A] }
  | A''[1, B; 2, O; 1, A] -> { KnownClosure = [1, A]; Cover = [1, A; 1, B; 1, A] }
  | A''[1, B; 3, O; 1, A] -> { KnownClosure = [1, A]; Cover = [2, A; 1, B] }
  | A''[1, B; 4, O; 1, A] -> { KnownClosure = [1, A]; Cover = [2, A; 1, O; 1, B] }
  | A''[1, B; 5, O; 1, A] -> { KnownClosure = [1, A]; Cover = [2, A; 2, O; 1, B] }

  | A''[2, O; 1, B; 1, A] -> { KnownClosure = [1, A]; Cover = [4, A] }
  
  | A''[1, O; 1, B; 1, O; 1, A] -> { KnownClosure = [1, A]; Cover = [1, B; 4, O; 1, A] }
  | A''[1, O; 1, B; 1, O; 1, A] -> { KnownClosure = [1, A]; Cover = [1, B; 4, O; 1, A] }


  | A''[                        (1, B); (3, O); (1, B); (1, A)] -> { KnownClosure = [1, A]; Cover = [5, A] }
  | A''[        (2, O); (1, B); (1, A); (2, O); (1, B); (1, A)] -> { KnownClosure = [1, A]; Cover = [7, A] }
  | A''[(1, B); (3, O); (1, B); (1, A); (2, O); (1, B); (1, A)] -> { KnownClosure = [1, A]; Cover = [8, A] }

  | [(1, A); (2, O); (1, B); (1, A); (2, O); (1, B); (1, A); (2, O); (1, B); (1, A)]  -> { KnownClosure = [1, A]; Cover = [10, A] }

  | A''[                                (2, O); (1, B); (1, A); (2, O); (1, A)] -> { KnownClosure = [1, A]; Cover = [4, A; 1, B] }
  | A''[                        (1, B); (3, O); (1, B); (1, A); (2, O); (1, A)] -> { KnownClosure = [1, A]; Cover = [5, A; 1, B] }
  | A''[        (2, O); (1, B); (1, A); (2, O); (1, B); (1, A); (2, O); (1, A)] -> { KnownClosure = [1, A]; Cover = [7, A; 1, B] }
  | A''[(1, B); (3, O); (1, B); (1, A); (2, O); (1, B); (1, A); (2, O); (1, A)] -> { KnownClosure = [1, A]; Cover = [8, A; 1, B] }
  // Already handled case: [1, B]

  // Doesn't yet reduce
  | A''[1, O; 2, A] -> { KnownClosure = [2, B; 1, A]; Cover = [2, B; 1, A]}
  | [2, B; 1, A] -> { KnownClosure = [2, B; 1, O; 1, A]; Cover = [2, B; 1, O; 1, A]}
  | [2, B; 1, O; 1, A] -> { KnownClosure = [1, B; 1, A; 2, O; 1, A]; Cover = [1, B; 1, A; 2, O; 1, A]}
  
  | [n, B; m, A] when n > 1 && m > 1         -> { KnownClosure = [n, B; 2 * m - 1, O; 1, A]; Cover = [n, B; 2 * m - 1, O; 1, A]}
  | [n, B; 1, O; m, A] when n >= 1 && m >= 1 -> { KnownClosure = [n - 1, B; 1, A; 2 * m, O; 1, A]; Cover = [n - 1, B; 1, A; 2 * m, O; 1, A]}

  //| A''[2, O; 1, B] -> [1, A; 1, O; 1, A; 1, O; 1, A]
  //| [1, A; 1, O; 1, A; 1, O; 1, A]    -> { KnownClosure = [1, B; 1, A; 1, B]; Cover = [1, B; 1, A; 1, B] }
  //| [1, B; 1, A; 1, B]                -> { KnownClosure = [1, B; 2, O; 2, A]; Cover = [1, B; 2, O; 2, A] }
  //| [1, B; 2, O; 2, A]                -> { KnownClosure = [1, B; 2, O; 2, A]; Cover = [1, A; 2, B; 1, A] }
  //| [1, A; 2, B; 1, A]                -> { KnownClosure = [2, B; 1, O; 1, A]; Cover = [2, B; 1, O; 1, A] }
  //| [2, B; 1, O; 1, A]                -> { KnownClosure = [1, B; 1, A; 2, O; 1, A]; Cover = [1, B; 1, A; 2, O; 1, A] }
  //| [1, B; 1, A; 2, O; 1, A]          -> { KnownClosure = [1, B; 1, O; 1, A; 1, B]; Cover = [1, B; 1, O; 1, A; 1, B] }
  //| [1, B; 1, O; 1, A; 1, B]          -> { KnownClosure = [1, A; 3, O; 2, A]; Cover = [1, A; 3, O; 2, A] }
  //| [1, A; 3, O; 2, A]                -> { KnownClosure = [1, A; 1, O; 2, B]; Cover = [1, A; 1, O; 2, B] }
  //| [1, A; 1, O; 2, B]                -> { KnownClosure = [1, B; 1, O; 1, B; 1, O; 1, A]; Cover = [1, B; 1, O; 1, B; 1, O; 1, A] }
  //| [1, B; 1, O; 1, B; 1, O; 1, A]    -> { KnownClosure = [1, A; 1, O; 1, A; 2, O; 1, A]; Cover = [1, A; 1, O; 1, A; 2, O; 1, A] }
  //| [1, A; 1, O; 1, A; 2, O; 1, A]    -> { KnownClosure = [1, B; 1, A; 1, O; 1, B]; Cover = [1, B; 1, A; 1, O; 1, B] }
  //| [1, B; 1, A; 1, O; 1, B]          -> { KnownClosure = [1, B; 1, O; 1, B; 2, O; 1, A]; Cover = [1, B; 1, O; 1, B; 2, O; 1, A] }
  //| [1, B; 1, O; 1, B; 2, O; 1, A]    -> { KnownClosure = [1, A; 1, O; 1, A; 1, B; 1, A]; Cover = [1, A; 1, O; 1, A; 1, B; 1, A] }
  //| [1, A; 1, O; 1, A; 1, B; 1, A]    -> { KnownClosure = [2, B; 1, O; 2, A]; Cover = [2, B; 1, O; 2, A] }
  //| [(2, B); (1, O); (2, A)]          -> { KnownClosure = [(1, B); (1, A); (4, O); (1, A)]; Cover = [(1, B); (1, A); (4, O); (1, A)] }
  //| [(2, B); (1, O); (2, A)]          -> { KnownClosure = [(1, B); (1, A); (4, O); (1, A)]; Cover = [(1, B); (1, A); (4, O); (1, A)] }
  //| [(1, B); (1, A); (4, O); (1, A)]  -> { KnownClosure = [(1, B); (1, O); (1, A); (2, O); (1, B)]; Cover = [(1, B); (1, O); (1, A); (2, O); (1, B)] }
  //| [(1, B); (1, O); (1, A); (2, O); (1, B)] -> { KnownClosure = [(1, A); (2, O); (2, A); (1, O); (1, A)]; Cover = [(1, A); (2, O); (2, A); (1, O); (1, A)] }
  //| [(1, A); (2, O); (2, A); (1, O); (1, A)] -> { KnownClosure = [(1, A); (2, B); (1, O); (1, B)]; Cover = [(1, A); (2, B); (1, O); (1, B)] }
  //| [(1, A); (2, B); (1, O); (1, B)]  -> { KnownClosure = [(1, B); (1, A); (1, O); (2, A)]; Cover = [(1, B); (1, A); (1, O); (2, A)] }
  //| [(1, B); (1, A); (1, O); (2, A)]  -> { KnownClosure = [(1, B); (1, O); (2, B); (1, A)]; Cover = [(1, B); (1, O); (2, B); (1, A)] }
  //| [(1, B); (1, O); (2, B); (1, A)]  -> { KnownClosure = [(1, A); (1, O); (2, B); (1, O); (1, A)]; Cover = [(1, A); (1, O); (2, B); (1, O); (1, A)] }
  //| [(1, A); (1, O); (2, B); (1, O); (1, A)] -> { KnownClosure = [(1, B); (1, O); (1, B); (3, O); (1, A)]; Cover = [(1, B); (1, O); (1, B); (3, O); (1, A)] }
  //| [(1, B); (1, O); (1, B); (3, O); (1, A)] -> { KnownClosure = [(1, A); (1, O); (2, A); (1, B)]; Cover = [(1, A); (1, O); (2, A); (1, B)] }
  //| [(1, A); (1, O); (2, A); (1, B)]  -> { KnownClosure = [(3, B); (2, O); (1, A)]; Cover = [(3, B); (2, O); (1, A)] }
  //| [(3, B); (2, O); (1, A)] -> { KnownClosure = [(2, B); (1, A); (1, B); (1, A)]; Cover = [(2, B); (1, A); (1, B); (1, A)] }
  //| [(2, B); (1, A); (1, B); (1, A)]  -> { KnownClosure = [(2, B); (2, O); (1, B); (1, O); (1, A)]; Cover = [(2, B); (2, O); (1, B); (1, O); (1, A)] }
  //| [(2, B); (2, O); (1, B); (1, O); (1, A)] -> { KnownClosure = [(1, B); (1, A); (1, B); (4, O); (1, A)]; Cover = [(1, B); (1, A); (1, B); (4, O); (1, A)] }
  //| [(1, B); (1, A); (1, B); (4, O); (1, A)] -> { KnownClosure = [(1, B); (2, O); (2, A); (1, O); (1, B)]; Cover = [(1, B); (2, O); (2, A); (1, O); (1, B)] }
  //| [(1, B); (2, O); (2, A); (1, O); (1, B)] -> { KnownClosure = [(1, A); (2, B); (2, A); (1, O); (1, A)]; Cover = [(1, A); (2, B); (2, A); (1, O); (1, A)] }
  //| [(1, A); (2, B); (2, A); (1, O); (1, A)] -> { KnownClosure = [(2, B); (3, O); (1, B); (1, A)]; Cover = [(2, B); (3, O); (1, B); (1, A)] }
  //| [(2, B); (3, O); (1, B); (1, A)]  -> { KnownClosure = [(1, B); (5, A)]; Cover = [(1, B); (5, A)] }
  //| [(1, B); (5, A)]                  -> { KnownClosure = [(1, B); (9, O); (1, A)]; Cover = [(1, B); (9, O); (1, A)] }
  //| [(1, B); (9, O); (1, A)]          -> { KnownClosure = [(0, B); (2, A); (6, O); (1, B)]; Cover = [(0, B); (2, A); (6, O); (1, B)] }
  //| [(0, B); (2, A); (6, O); (1, B)]  -> { KnownClosure = [(1, B); (9, O); (1, A)]; Cover = [(1, B); (9, O); (1, A)] }
  //| [(1, B); (9, O); (1, A)]          -> { KnownClosure = [(0, B); (2, A); (6, O); (1, B)]; Cover = [(0, B); (2, A); (6, O); (1, B)] }
  //| [(0, B); (2, A); (6, O); (1, B)]  -> { KnownClosure = [(2, A); (6, O); (1, B)]; Cover = [(2, A); (6, O); (1, B)] }
  //| [(2, A); (6, O); (1, B)]          -> { KnownClosure = [(1, A); (4, O); (1, A); (1, O); (1, A)]; Cover = [(1, A); (4, O); (1, A); (1, O); (1, A)] }
  //| [(1, A); (4, O); (1, A); (1, O); (1, A)] -> { KnownClosure = [(1, A); (2, O); (1, B); (1, O); (1, B)]; Cover = [(1, A); (2, O); (1, B); (1, O); (1, B)] }
  //| [(1, A); (2, O); (1, B); (1, O); (1, B)] -> { KnownClosure = [(2, A); (2, O); (2, A)]; Cover = [(2, A); (2, O); (2, A)] }
  //| [(2, A); (2, O); (2, A)]          -> { KnownClosure = [(1, A); (2, B)]; Cover = [(1, A); (2, B)] }
  //| [(1, A); (2, B)]                  -> { KnownClosure = [(1, B); (2, A)]; Cover = [(1, B); (2, A)] }
  //| [(1, B); (2, A)]                  -> { KnownClosure = [(1, B); (3, O); (1, A)]; Cover = [(1, B); (3, O); (1, A)] }



  //| [n, A; 3, O; 1, B] when n > 0 -> { KnownClosure = [1, A; 1, O; 1, A; 1, O; 1, A]; Cover = [1, A; 1, O; 1, A; 1, O; 1, A] }

  // Check again
  //| A' (n, O' (m, [1, A])) when n > 0 -> { KnownClosure = [1, A]; Cover = [1, A; m - 2, O; 1, B] }





  //| [n, B]             -> { KnownClosure = [n - 1, B; 2, A]; Cover = [n - 1, B; 2, A] }
  //| B'(n, A'(m, []))   -> { KnownClosure = [n, B; 2 * m - 1, O; 1, A]; Cover = [n, B; 2 * m - 1, O; 1, A] }
  //| B'(n, O'(m, [k, A])) when n > 0 && m > 3 -> { KnownClosure = [n - 1, B; 2, A; m - 3, O; k, B]; Cover = [n - 1, B; 2, A; m - 3, O; k, B] }
  //
  //| [n, B; 2, O; m, A] when n > 5 && m > 5 -> { KnownClosure = [n - 1, B; 1, A; m, B; 1, A]; Cover = [n - 1, B; 1, A; m, B; 1, A] }

  
  //| [1, A; 1, O; 1, B] -> { KnownClosure = [(1, B); (2, O); (1, A)]; Cover = [(1, B); (2, O); (1, A)] }




  //| A'(1, O'(n, t))      when n > 2 -> go2 [1, A; n - 2, O] t
  
  //| [1, A; n, O; 1, B] when n > 2 -> { KnownClosure = [(1, A); (n - 2, O); (1, A); (1, O); (1, A)] ; Cover = [(1, A); (n - 2, O); (1, A); (1, O); (1, A)] }

  
  | unhandled -> unhandled |> sprintf "unhandled %A" |> failwith 
  
  go1

  
//  function

//Doesn't yet reduce

//| [1, A; n, O; 1, B] when n > 2 -> { KnownClosure = [(1, A); (n - 2, O); (1, A); (1, O); (1, A)] ; Cover = [(1, A); (n - 2, O); (1, A); (1, O); (1, A)] }
//[1, A; 0, O; 1, B] is already another case

//| [1, A; n, O; 1, A; 1, O; 1, A] when n > 2 -> { KnownClosure = [1, A; n - 2, O; 1, B; 1, O; 1, B]; Cover = [1, A; n - 2, O; 1, B; 1, O; 1, B] }
//| [1, A; n, O; 1, B; 1, O; 1, B] when n > 2 -> { KnownClosure = [1, A; n - 2, O; 1, B; 1, O; 1, B]; Cover = [1, A; n - 2, O; 1, A; 2, O; 1, A] }


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

  let rec f i =
    let mutable result = i |> equivClass
    let test = (decode (encode3 result))

    if i % 100000I = 0I then
      printfn "%A" i

    if result <> test then
      printfn "%A <> %A" result test
      assert (false)
    if (hashSet.Contains result) then
      f (i + 1I)
    else
      hashSet.Add result |> ignore

      let mutable r1 = encode3 result
      result <- Collatz1 result |> equivClass
      result <- Collatz1 result |> equivClass
      let mutable r2 = encode3 result

      //match r1, r2 with
      //| _, [1, A]    -> 
      printfn "%A -> %A" r1 r2
      //  ()
      //| _ ->  ()
      
      f (i + 1I)
      
      //try       
      //  let { KnownClosure = r3 } = reduce r1
      //  if r3 <> [(1, A)] then
      //    printfn "%A = %A -> %A" result r1 r3
      //with
      //| _ ->
      //  printfn "  | %A -> { KnownClosure = %A; Cover = %A }" r1 r2 r2
      //
      //  while r1 <> [1, A] do
      //    let r3 =
      //      try
      //        (reduce r1).KnownClosure
      //      with
      //      | _ -> 
      //        result <- Collatz1 result |> equivClass
      //        encode3 result
      //      
      //    
      //    if r3 <> [(1, A)] then
      //      printfn "| %A -> { KnownClosure = %A; Cover = %A }" r1 r3 r3

      //    r1 <- r3
        //printfn "* FAIL - %A -> { KnownClosure = %A; Cover = %A }" r1 r2 r2

  //f 1I

  let mutable result = decode [1, B; 80, O; 1, B; 1, O; 1, B; 1, O; 1, B]
  let mutable r1 = encode3 result
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass

  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass

  
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  

  
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  
  
  result <- Collatz1 result |> equivClass
  result <- Collatz1 result |> equivClass
  
  
  let mutable r2 = encode3 result

  printfn "%A" r1 
  printfn "%A" r2

  Console.ReadLine() |> ignore
  0 // return an integer exit code
