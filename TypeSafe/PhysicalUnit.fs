namespace TypeSafe

open NaturalNumbers

module PhysicalUnit =
  type Dimension = interface end

  type Distance<'T when 'T :> Nat> = Length of 'T with interface Dimension
  type Time<'T when 'T :> Nat>   = Time   of 'T with interface Dimension
  //type Temperature<'T when 'T :> Int
  
  type DimensionalItems<'L, 'T when 'L :> Nat and 'T :> Nat> = DI of value:float * Distance<'L> * Time<'T> with
    static member inline (+)(DI(v, Length a, Time b), DI(v', Length a', Time b')) =
      Eq(a, a') |> ignore
      Eq(b, b') |> ignore
      DI(v + v', Length a, Time b)

  let s = DI(2.0, Length Z3_, Time Z2_)
  let s' = DI(1.0, Length Z3_, Time Z2_)
  let c = s + s'