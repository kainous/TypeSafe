module NaturalNumbers

open System
open Orders
open Algebras
open BooleanLogic

//[<CustomComparison; CustomEquality>]
type Nat =
| Z         // Zero
| S of Nat  // Successor or "next value"
with
  
  // Cannot expose this
  member private this.Subtract other =
    match this, other with
    | a, Z -> a
    | Z, _ -> failwith "Cannot encode negative numbers"
    | S a, S b -> a.Subtract b    

  member this.AsBigInt() =
    let rec go output = function
    | Z   -> output
    | S a -> go (Numerics.BigInteger.Add(1I, output)) a
    go 0I this

  member this.CanReach other =
    match this, other with
    | Z, _     -> T
    | S a, S b -> a.CanReach b
    | _        -> F

  member this.Successor = S this

  member this.Add other =
    match this, other with
    | Z, b   -> b
    | S a, b -> S (a.Add b)

  member this.Multiply (other:Nat) =
    match this, other with
    | Z, _   -> Z
    | S a, b -> b.Add (a.Multiply b)

  //member this.Equals other = (this.CanReach other) * (other.CanReach this)

  member this.GreatestCommonDenominator other =
    match this, other with
    | a, b when a.CanReach b = T && b.CanReach a = T -> a
    | a, b when a.CanReach b = T -> b.Subtract a
    | a, b -> a.Subtract b

  interface RingObject<Nat, Nat> with
    member __.Coproduct = { new AbelianGroup<Nat> with member __.Op a b = a.Add b }
    member __.Product   = { new Monoid<Nat> with member __.Op a b = a.Multiply b }

  //interface TotalOrder<Nat, Bool> with
  //  member this.CanReach other = this.CanReach (other :?> Nat)

  //member this.CompareTo other =
  //  match this, other with
  //  | Z, S _ -> -1
  //  | S _, Z -> +1
  //  | Z, Z   ->  0
  //  | S a, S b -> a.CompareTo b

  //member this.Equals other =
  //  match this, other with
  //  | Z, Z     -> true
  //  | S a, S b -> a = b
  //  | _        -> false

  //interface IComparable<Nat> with
  //  member this.CompareTo other = this.CompareTo other

  //interface IComparable with
  //  member this.CompareTo other = this.CompareTo (other :?> Nat)

  //interface IEquatable<Nat> with
  //  member this.Equals other = this.Equals other

  //override this.Equals other = this.Equals (other :?> Nat)

  //override this.GetHashCode() = this.AsBigInt().GetHashCode()

  //static member ( <= ) (a : Nat, b : Nat) =
  //  match a, b with
  //  | Z, _       -> true
  //  | S a', S b' -> a' <= b'
  //  | _          -> false

let test = S (S Z) + S Z

  