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
  member this.Subtract other =
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

  member this.EqualsTo other = (this.CanReach other) * (other.CanReach this)

  member this.Successor = S this

  member this.Add other =
    match this, other with
    | Z, b   -> b
    | S a, b -> S (a.Add b)

  member this.Multiply (other:Nat) =
    match this, other with
    | Z, _   -> Z
    | S a, b -> b.Add (a.Multiply b)

  member this.Divide (other:Nat) =
    let rec go quot = function
    | _, Z -> failwith "Cannot divide by 0"
    | Z, _ -> quot, Z
    | (a:Nat), (b:Nat) when a.CanReach b = T && b.CanReach a = T -> (S quot), a
    | (a:Nat), (b:Nat) when b.CanReach a = T -> quot, a
    | (a:Nat), (b:Nat) -> go (S quot) (a.Subtract b, b)
    go Z (this, other)

  member this.Exponentiate (other:Nat) =
    match this, other with
    | Z, Z   -> failwith "No clear definition for raising 0 to the power of 0"
    | Z, S Z -> Z
    | _, Z   -> S Z
    | a, S b -> (a.Exponentiate b).Multiply a

  //member this.Equals other = (this.CanReach other) * (other.CanReach this)

  member this.GreatestCommonDenominator other =
    match this, other with
    | a, b when a.CanReach b = T && b.CanReach a = T -> a
    | a, b when a.CanReach b = T -> b.Subtract a
    | a, b -> a.Subtract b

  member this.LeastCommonMultiple other = (this.Multiply other).Divide (this.GreatestCommonDenominator other) |> fst

  interface ExponentialRingObject<Nat, Nat> with
    member __.Coproduct   = { new AbelianGroup<Nat>     with member __.Op a b = a.Add b }
    member __.Product     = { new Monoid<Nat>           with member __.Op a b = a.Multiply b }
    member __.Exponential = { new ExponentialMagma<Nat> with member __.Op a b = a.Exponentiate b }

  member __.NaturalOrder =
    { new TotallyOrderedObject<Nat, Bool> with
        member __.Coproduct = { new AbelianGroup<Nat> with member __.Op a b = if (a .CanReach b) = T then a else b }
        member __.Product = { new Monoid<Nat> with member __.Op a b = if (a .CanReach b) = T then b else a }
        member __.Exponentiate = { new ExponentialMagma<Nat> with member __.Op a b = a.Subtract b }
        member __.CanReach a b = a.CanReach b }

  member __.Divisibility =
    { new Lattice<Nat, Bool> with
        member __.Coproduct = { new AbelianGroup<Nat> with member __.Op a b = a.GreatestCommonDenominator b }
        member __.Product   = { new Monoid<Nat> with member __.Op a b = a.LeastCommonMultiple b }
        member __.CanReach a b = ((a.Divide b) |> snd) .EqualsTo Z }

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

let test1 = S (S Z) * S (S Z)

let test2 = (S (S (S Z))) --> (S (S Z))