module Integers

open System
open Orders
open Algebras
open BooleanLogic
open NaturalNumbers

type Integer = Integer of Nat * Nat
with
  member this.NormalForm =
    match this with
    | Integer(a, b) when a --> b = T -> Integer(Z, b.Subtract a)
    | Integer(a, b)                  -> Integer(a.Subtract b, Z)

  member this.CanReach other =
    match this, other with
      Integer(a, b), Integer(a', b') -> (a + b') --> (b + a')

  member this.Add other =
    match this, other with
      Integer(a, b), Integer(a', b') -> Integer(a + a', b + b').NormalForm

  member this.Multiply other =
    match this, other with
      Integer(a, b), Integer(a', b') -> Integer(a * a' + b * b', a * b' + a' * b)

  member this.Subtract other =
    match this, other with
      Integer(a, b), Integer(a', b') -> Integer(a + b', a' + b)

  interface RingObject<Integer, Integer> with
    member __.Coproduct = { new AbelianGroup<Integer> with member __.Op a b = a.Add b }
    member __.Product = { new Monoid<Integer> with member __.Op a b = a.Multiply b }

  member this.NaturalOrder =
    { new TotalOrder<Integer, Bool> with
        member __.Coproduct = { new AbelianGroup<Nat> with member __.Op a b = if (a .CanReach b) = T then a else b }
        member __.Product = { new Monoid<Nat> with member __.Op a b = if (a .CanReach b) = T then b else a } 
        member __.CanReach a b = a.CanReach b}