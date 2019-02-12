namespace TypeSafe

open NaturalNumbers

module Integers =
  type Refl<'T> = Eq of 'T * 'T with
    static member inline ( <-- )(_, _) = True

  let isEq (Eq(_, _)) = True

  type Int<'Pos, 'Neg when 'Pos :> Nat and 'Neg :> Nat > = Int of 'Pos * 'Neg with
    static member inline (!!) (Int(a, b)) = (a <<= b) <?> (Int(Z, b - a), Int(a - b, Z))
    static member inline (!++)(Int(a, b)) = Int(S a, b)
    static member inline ( ~-)(Int(a, b)) = Int(b, a)
    static member inline ( + )(Int(a, b), Int(c, d)) = !!Int(a + c, b + d)
    static member inline ( - )(Int(a, b), Int(c, d)) = !!Int(a + d, b + c)
    static member inline ( * )(Int(a, b), Int(c, d)) = !!Int(a * c + b * d, a * d + b * c)
    static member inline (===)(Int(a, b), Int(c, d)) = isEq(Eq(a + d, b + c))
    static member inline ( !%%% )(S a) = Int(S a, Z)  //Factorial
    static member inline ( !%%% )(Z) = Int(Z, Z)      //Factorial

  //let inline (~-) (Int(a, b)) = Int(b, a)  

  let P0 = Int(Z, Z)
  let P1 = !++ P0
  let P2 = !++ P1
  let N1 = P0 - P1
  let c = (Int(Z1_, Z2_)) + P1
  let a1 = !!((Int(Z, Z)))
  let a2 = !!((Int(S Z, Z)))
  let a3 = !!((Int(Z, S Z)))
  let a4 = !!((Int(S Z, S Z)))
  let a5 = !!((Int(Z2_, Z3_)))

  let d2 = !! Int(S Z, S (S Z))

  let d = c === P0  //A witness that proves reflexivity (equality)

  //let x = -Int(Z1_, Z3_)



  //let test = ()