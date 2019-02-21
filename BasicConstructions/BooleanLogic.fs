module BooleanLogic

open Algebras
open Orders
open System.Collections.Generic

type Bool =
| T
| F
with
  member this.Implies other =
    match this, other with
    | F, T -> F
    | _    -> T

  member this.And other =
    match this, other with
    | T, T -> T
    | _    -> F

  member this.Or other =
    match this, other with
    | F, F -> F
    | _    -> T

  interface TotallyOrderedObject<Bool, Bool> with
    member __.CanReach a b = a.Implies b
    member __.Exponential = { new ExponentialMagma<Bool> with member __.Op a b = a.Implies b }

  interface RingObject<Bool, Bool> with
    member __.Coproduct   = { new AbelianGroup<Bool>     with member __.Op a b = a.Or b }
    member __.Product     = { new Monoid<Bool>           with member __.Op a b = a.And b }

type Set<'T when 'T : comparison >(items:IEnumerable<'T>) =
  let _internal = Collections.Set<'T>(items)
  member internal this.Items = _internal

  //member __.SetInclusion =
  //  { new Heyting<Set<'T>, Bool> with
  //      member __.CanReach a b = if a.Items <= b.Items then T else F
  //      member __.Meet a b     = Set.intersect a.Items b.Items |> Set
  //      member __.Join a b     = Set.union a.Items b.Items |> Set
  //      member __.Implies a b  = a.Items - b.Items |> Set }

  //member this.SetMapping =
  //  { new ExponentialRing<Set<'T>> with
  //      member this.Coproduct other =
  //        Set.union 
  //          (this.Items |> Seq.map(fun a -> F, a))
  //          (other.Items |> Seq.map(fun b -> T, b))
  //      member this.Product other =
  //        seq { for a in this.Items ->
  //              for b in other.Items ->
  //              (a, b) } |> Set
  
  //  }

//type MinimalLogic<'TLogic> =
//  abstract member Bottom  : 'TLogic
//  abstract member Implies : 'TLogic -> 'TLogic

//type FuzzyLogic<'TLogic>(tnorm:'TLogic -> 'TLogic -> 'TLogic, negator : 'TLogic -> 'TLogic) =
  