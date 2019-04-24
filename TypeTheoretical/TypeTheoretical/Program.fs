open System.Diagnostics
open Microsoft.FSharp.Core.Printf
open System.Text
open System.IO

//type Choice<'A> =
//| Left

type ExprType =
| Star
| Box

type Var = V of string * int

[<DebuggerDisplay("{ToString}")>]
type Expr<'T> =
| Star
| Embed of 'T
| Absurd of 'T
| Var of Var
| Apply  of Expr<'T> * Expr<'T>
| Lambda of string * Expr<'T> * Expr<'T>  // Typing info
| Forall of string * Expr<'T> * Expr<'T>  // Typing info
with
    override this.ToString() =
        let rec go = function
        | Lambda(v, Star, b) -> sprintf "λ%s → %s" v (go b)
        | Lambda(v, a, b) -> sprintf "λ(%s : %s) → %s" v (go a) (go b)
        | Forall(v, a, b) -> sprintf "∀(%s : %s) → %s" v (go a) (go b)
        | Apply(a, b) -> sprintf "(%s %s)" (go a) (go b)
        | Var(a, 0) -> sprintf "%s" a
        | Var(a, n) -> sprintf "%s@%d" a n
        | Embed a -> sprintf "%A" a
        | Star -> sprintf "*"
        | Absurd a -> sprintf "%A -> _|_" a

        go this

type LLL<'T> =
| Cons of 'T * LLL<'T>
| Nil

//let Either = Lambda(Lambda(Forall()

//newtype Either a b = Either {either :: forall u . (a -> u) -> (b -> u) -> u}

//newtype Pair a b = Pair {pair :: forall u . (a -> b -> u) -> u}

type IPair<'A, 'B> =
  abstract unpair : f:('A -> 'B -> 'C) -> 'C

type Pair<'A, 'B>(a, b) =
  interface IPair<'A, 'B> with
    member __.unpair f = f a b

let fst (p:IPair<'A, 'B>) = p.unpair (fun a _ -> a)
let snd (p:IPair<'A, 'B>) = p.unpair (fun _ b -> b)

type IEither<'A, 'B> =
  abstract either : True:('A -> 'R) -> False:('B -> 'R) -> 'R

type Left<'A, 'B>(left:'A) =
  interface IEither<'A, 'B> with
    member __.either f _ = f left

type Right<'A, 'B>(right:'B) =
  interface IEither<'A, 'B> with
    member __.either _ g = g right


//let either (f:'A -> 'R) (g:'B -> 'R) : 'R -> 'R

//let boolT = Forall(Star, Forall(Var))
//let eitherT = Forall(Star, Forall(Star, Forall(Star, Apply(Lambda(Var 3, Var 2))


type List<'A> =
  abstract Uncons : ('A -> 'R -> 'R) -> 'R -> 'R

type Empty<'A> =
  interface List<'A> with
    member __.Uncons _ e = e

type Cons<'A>(x, xs:List<'A>) =
  interface List<'A> with
    member __.Uncons c e = c x (xs.Uncons c e)


type NatNum =
  abstract unnum : (NatNum -> 'r) -> 'r -> 'r

type Z() =
  interface NatNum with
    member this.unnum _ z = z

type S(n) =
  interface NatNum with
    member this.unnum s _ = s n

let unnum (s:NatNum -> 'r) (z:'r) (f : NatNum) : 'r = f.unnum s z

let isZero n = unnum (fun _ -> false) true n
let rec add n m = unnum (fun s -> S(add s m)) m n

type Context<'T> = Context of (string * 'T) list

let fmapContext (f:'A -> 'B) (Context ctx) : Context<'B> =
  ctx |> List.map (fun (s, a) -> (s, f a)) |> Context

let insert k v (Context kvs) = Context((k, v)::kvs)

let lookup k n0 (Context kvs0) =
  let rec go = function
  | ((k', _)::kvs), n when k <> k' -> go (kvs, n)
  | ((_,  _)::kvs), n when n >  0  -> go (kvs, n - 1)
  | ((_,  v)::_  ), n when n =  0  -> Some v
  | _                              -> None

  go (kvs0, n0)



let rec (<*>) (mf:Expr<'A -> 'B>) (mx:Expr<'A>) : Expr<'B> =
  match mf with
  | Star -> Star
  | Var(V(v, n)) -> Var <| V(v, n)
  | Lambda(x, _A, b) -> Lambda(x, _A <*> mx, b <*> mx)
  | Forall(x, _A, _B) -> Forall(x, _A <*> mx, _B <*> mx)
  | Apply(f, a) -> Apply(f <*> mx, a <*> mx)
  | Embed f -> fmap f mx
  | Absurd f -> fmap f mx
and fmap f x = Embed f <*> x

let rec (>>=) (m:Expr<'A>) (f:'A -> Expr<'B>) =
  match m with
  | Star -> Star
  | Var(V(v, n)) -> Var <| V(v, n)
  | Lambda(x, a, b) -> Lambda(x, a >>= f, b >>= f)
  | Forall(x, a, b) -> Forall(x, a >>= f, b >>= f)
  | Apply(x, a) -> Apply(x >>= f, a >>= f)
  | Embed r -> f r
  | Absurd r -> failwith "Absurd"


let shift (d:int) (x0:string) (e0:Expr<_>) =
  let rec go e c =
    match e with
    | Lambda(x, _A, b) -> 
      let c' = if x = x0 then c + 1 else c
      Lambda(x, go _A c, go b c')
    
    | Forall(x, _A, _B) -> 
      let c' = if x = x0 then c + 1 else c
      Forall(x, go _A c, go _B c')

    | Apply(f, a) ->
      Apply(go f c, go a c)

    | Var(V(x, n)) ->
      let n' = if x = x0 && n >= c then n + d else n
      Var <| V(x, n')

    | Embed k -> 
      Embed k
    
    | Star -> 
      Star
  
  go e0 0

let rec subst (x:string) (n:int) (e':Expr<_>) = function
| Star -> 
  Star

| Embed p -> 
  Embed p

| Var (V(x', n')) when x = x' && n = n' ->
  e'

| Var _ as e ->
  e

| Apply(f, a) -> 
  Apply(subst x n e' f, subst x n e' a)

| Absurd _ as a ->
  a

| Forall(x', _A, b) ->
  let n' = if x = x' then n + 1 else n
  let b' = subst x n' (shift 1 x' e') b
  Lambda(x', subst x n e' _A, b')

| Lambda(x', _A, b) ->
  let n' = if x = x' then n + 1 else n
  let b' = subst x n' (shift 1 x' e') b
  Lambda(x', subst x n e' _A, b')

type TypeErrorKind<'T> = 
| UnboundVariable
| NotAFunction
| InvalidInputType of Expr<'T>
| InvalidOutputType of Expr<'T>
| TypeMismatch of Expr<'T> * Expr<'T>
type TypeError<'T> = TypeError of (Context<'T> * Expr<'T> * TypeErrorKind<'T>)

type ExprBuilder() =
  member this.Bind(x, f) =
    f >>= x

  member this.ReturnFrom(v:Expr<_>) =
    v

  member this.Return v =
    Embed v

let expr = ExprBuilder()

type ResultBuilder() =
  member __.Bind(x:Result<'A, _>, f:'A -> Result<'B, _>) =
    match x with
    | Ok x' -> f x'
    | Error err -> Error err

  member __.ReturnFrom(v:Result<_,_>) =
    v

  member __.Return(v:'T) =
    Ok v

let result = ResultBuilder()

let rec whnf = function
| Apply(f, a) ->
  match whnf f with
  | Lambda(x, _A, b) ->
    let a' = shift 1 x a
    let b' = subst x 0 a' b
    whnf (shift -1 x b')
  
  | f' -> 
    Apply(f', a)
| _ as e -> e

let rec freein (V(x, n) as v) =
  let rec go = function
  | Lambda(x', _A, b) ->
    let n' = n + 1
    go _A || if x = x' then freein (V(x, n')) b else go b

  | Forall(x', _A, _B) ->
    let n' = n + 1
    go _A || if x = x' then freein (V(x, n')) _B else go _B

  | Var v' -> v = v'
  | Apply(f, a) -> go f || go a
  | Absurd _
  | Embed _
  | Star -> false
  go

let rec normalize = function
| Lambda(x, _A, b) ->
  let b' = normalize b
  let e' = Lambda(x, normalize _A, b')
  match b' with
  | Apply(f, a) ->
    let a' = whnf a
    let v = V(x, 0)
    match a' with
    | Var v' when v = v' && not (freein v f) -> shift (-1) x f
    | _ -> e'
  | _ -> e'

| Forall(x, _A, _B) ->
  Forall(x, normalize _A, normalize _B)

| Apply(f, a) ->
  match normalize f with
  | Lambda(x, _A, b) ->
    let a' = shift 1 x (normalize a)
    let b' = subst x 0 a' b
    normalize (shift (-1) x b')
  | f' -> Apply(f', normalize a)

| Var _
| Star as e -> e

| Embed p -> Embed p
| Absurd p -> Absurd p

let rec typeWith (ctx:Context<Expr<'T>>) (e:Expr<'T>) : Result<_,_> =
  match e with
  | Embed p -> Ok <| Absurd p
  | Absurd p -> Ok <| Absurd p
  | Star -> Ok <| Star
  | Var(V(x, n)) ->
    match lookup x n ctx with
    | None -> Error(TypeError(ctx, Var(V(x, n)), UnboundVariable))
    | Some a -> Ok a

  | Lambda(x, _A, b) -> result {
      let! _ = typeWith ctx _A
      let ctx' = fmapContext (shift 1 x) (insert x _A ctx)
      let! _B = typeWith ctx' b
      let p = Forall(x, _A, _B)
      let! _t = typeWith ctx p
      return p
    }

  | Forall(x, _A, _B) -> result {
      let! eS = Result.map whnf (typeWith ctx _A)
      //let! s = 
      //  match eS with
      //  | Star -> Ok Star
      //  | _    -> Error(TypeError(ctx, e, InvalidInputType _A))
      let ctx' = fmapContext (shift 1 x) (insert x _A ctx)
      let! eT = Result.map (whnf) (typeWith ctx' _B)
      let! t = 
        match eT with
        | Star -> Ok Star
        | _    -> Error(TypeError(ctx', Forall(x, _A, _B), InvalidOutputType _B))
      return Star
    }

  | Apply(f, a) as e -> result {
      let! e' = Result.map whnf (typeWith ctx f)
      let! (x, _A, _B) =
        match e' with
        | Forall(x, _A, _B) -> Ok (x, _A, _B)
        | _                 -> Error(TypeError(ctx, Apply(f, a), NotAFunction))
      let! _A' = typeWith ctx a
      return!
        if _A = _A'
          then result {
            let a' = shift 1 x a
            let _B' = subst x 0 a' _B
            return (shift (-1) x _B')
          }
          else result {
            let nf_A = normalize _A
            let nf_A' = normalize _A'
            return! Error(TypeError(ctx, e, TypeMismatch(nf_A, nf_A')))
          }
    }

let typeOf = typeWith <| Context []
    

[<EntryPoint>]
let main argv = 
  let pair x y z = z x y
  let fst p = p (fun x -> fun y -> x)
  let snd p = p (fun x -> fun y -> y)

  //Scott encoding???
  let inline left a l r = l a
  let inline right a l r = r a
  let inline test1 f = left 3 f
  let inline test2 f = right "Hello" f
  let inline matchh f g p = p f g
  
  let test3 = matchh (fun x -> x) (fun x -> x + " World") (test1)
  let test4 = matchh (fun x -> x.ToString()) (fun x -> x + " World") (test2)

  //let ID = Forall(Star, Lambda(Var 1, Var 0))
  //let v = Apply(ID, Const (ID))

  let Bool  = Forall("Bool", Star, Forall("True", Var(V("Bool", 0)), Forall("False", Var(V("Bool", 0)), Var(V("Bool", 0)))))
  let True  = Forall("Bool", Star, Lambda("True", Var(V("Bool", 0)), Lambda("False", Var(V("Bool", 0)), Var(V("True", 0)))))
  let False = Forall("Bool", Star, Lambda("True", Var(V("Bool", 0)), Lambda("False", Var(V("Bool", 0)), Var(V("False", 0)))))
  let Maybe = Lambda("a", Star, Forall("Maybe", Star, Forall("Just", Forall("_Just", Var(V("a", 0)), Var (V("Maybe", 0))), Forall("Nothing", Var(V("Maybe", 0)), Var(V("Maybe", 0))))))


  //let List = Lambda("a", Star, Forall("List", Star, Forall("Cons", Forall("head", Var("a", 0), Forall("tail", Var("List", 0), Var("List", 0)), Forall("Nil", Var("List", 0), Var("List", 0))))))

  let s = Apply(Apply(True, Embed 3), Embed 4)
  
  let str = StringBuilder()
  bprintf str "%O\n" Bool
  bprintf str "%O\n" True
  bprintf str "%O\n" False
  bprintf str "%O\n" Maybe

  File.WriteAllText(@"C:\Temp\Test.txt", str.ToString())

  0 // return an integer exit code
