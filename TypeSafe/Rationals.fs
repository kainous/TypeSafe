namespace TypeSafe

open TypeSafe
open NaturalNumbers
open Integers

module Rationals =
  type Fraction<'Num1, 'Num2, 'Den when 'Num1 :> Nat and 'Num2 :> Nat and 'Den :> Nat> = Fraction of Int<'Num1, 'Num2> * 'Den with
    static member inline (+) (Fraction(a, b), Fraction(a', b')) =
      Fraction(a + a', b + b')

    static member inline (-) (Fraction(a, b), Fraction(a', b')) =
      Fraction(a - a', b - b')


  let R0 = Fraction(P0, Z0_)
  let R1_1 = Fraction(P1, Z1_)
  let R2_1 = Fraction(P2, Z1_)
  let R1_2 = Fraction