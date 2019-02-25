// Learn more about F# at http://fsharp.org

open System

type Var =
| VZero
| VSucc of Var

type Term<'T> =
  abstract Eval : 'T

let eval (t:Term<'T>) = t.Eval

type Apply<'A, 'B>(f:Term<'A -> 'B>, x:Term<'A>) =
  interface Term<'B> with
    member __.Eval = eval f <| eval x

type Var<'A>(v:'A) =
  interface Term<'A> with
    member __.Eval = v

type Const<'A>(v:'A) =
  interface Term<'A> with
    member __.Eval = v

type Lambda<'A, 'B>(f:Term<'A> -> Term<'B>) =
  interface Term<'A -> 'B> with
    member __.Eval =
      Var >> f >> eval

type Fix<'A>(f:Term<'A -> 'A>) =
  interface Term<'A> with
    member __.Eval =
      let rec y' g = g |> y' |> g
      eval f |> y'

let inline lambda (f:Term<'A> -> Term<'B>) : Term<'A -> 'B> =
  (Lambda f) :> Term<_>

let inline constant a = Const a :> Term<_>

//let inline lam2

let inline ( <*> ) (f:#Term<'A -> 'B>) (x:#Term<'A>) =
  Apply(f, x) :> Term<_>

let rec lookup (var:Var) (x:list<_>) =
  match var, x with
  | VZero, [x] -> x
  | VSucc v, tail -> lookup v tail
  | _ -> failwith "Cannot interpret an open term"

let test1 = Const 33

let inline ID<'a> = lambda <| id

let first = lambda <| fun x -> lambda <| fun _ -> x
let twice = lambda <| fun f -> lambda <| fun x -> lambda <| fun y -> f <*> (f <*> x <*> y) <*> y
let pairs = twice <*> first <*> (constant 33) <*> (constant 22)

let rec fix f x = f (fix f) x
//let rec nat = 


let rec fact n = if n = 0 then 1 else n * fact (n - 1)
let fact' = fix <| fun myfixed n -> if n = 0 then 1 else n * myfixed (n - 1)

let fact'' =  lambda <| fun (myfixed:Term<int -> int>) -> lambda <| fun (n:Term<int>) -> constant <| if n.Eval = 0 then 1 else n.Eval * myfixed.Eval (n.Eval - 1)

//let Bool = Forall<

let rec even = function
| 0 -> true
| n -> odd (n - 1)
and odd = function
| 0 -> false
| n -> even (n - 1)

let rec even' n =
  match n with
  | 0 -> true
  | _ -> match n - 1 with
         | 0 -> false
         | n -> even' (n - 1)

let odd' n = 
  match n - 1 with
  | 0 -> false
  | n -> even' (n - 1)

let even'' = fix <| fun myfixed n -> match n with
                                     | 0 -> true
                                     | _ -> match n - 1 with
                                            | 0 -> false
                                            | n -> myfixed (n - 1)


let bs n = <@ let rec even = function
              | 0 -> true
              | n -> odd (n - 1)
              and odd = function
              | 0 -> false
              | n -> even (n - 1)
              even n @>

let rec evod() =
  let e' = fun n -> n  = 0 || (snd (evod())) (n - 1)
  let o' = fun n -> n <> 0 && (fst (evod())) (n - 1)
  (e', o')

let (even''''', odd''''') = 
  let a' = fix <| fun evod -> fun () ->
    let e' = fun n -> n  = 0 || (snd (evod())) (n - 1)
    let o' = fun n -> n <> 0 && (fst (evod())) (n - 1)
    (e', o')
  a'()

let fix_poly =
  fun l -> fix (fun self l -> List.map (fun li x -> li (self l) x) l) l

let [e;o] =
  fix_poly [fun [even;odd] n -> n  = 0 || odd  (n - 1)
            fun [even;odd] n -> n <> 0 && even (n - 1)]

let Y2 f1 f2 =
  let f1' = fix (fun f1' -> f1 f1' (fix (fun f2' -> f2 f1' f2')))
  let f2' = fix (fun f2' -> f2 (fix (fun f1' -> f1 f1' f2')) f2')
  f1', f2'

let eo = Y2 (fun e o n -> n = 0 || o (n - 1)) (fun e o n -> n <> 0 && e (n - 1))

//let evod = fix <| 

let even'''' = evod() |> fst
let odd''''  = evod() |> snd


[<EntryPoint>]
let main argv =
  0 // return an integer exit code
