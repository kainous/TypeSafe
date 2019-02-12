namespace TypeSafe

[<AutoOpen>]
module Functional =
  type Func = interface end  

  type Id = Id with
    interface Func
    static member inline ( <-- )(Id, x) = x

  type Const<'T> = Const of 'T with
    interface Func
    static member inline ( <-- )(Const k, _) = k

  type Fst = Fst with
    interface Func
    static member inline ( <-- )(Fst, (x, _)) = x

  type Snd = Snd with
    interface Func
    static member inline ( <-- )(Snd, (_, y)) = y
  
  type Comp<'A, 'B when 'A :> Func and 'B :> Func > = Comp of 'A * 'B with
    interface Func
    static member inline ( <-- )(Comp(f, g), x) =
      f <-- (g <-- x)

  type Fork<'A, 'B> = Fork of 'A * 'B with
    interface Func
    static member inline ( <-- )(Fork(a, b), x) =
      (a <-- x, b <-- x)

  let inline ( --> ) x f = f <-- x
  let inline ( <-< ) f g = Comp(f, g)
  let inline ( >-> ) g f = Comp(f, g)
  let inline ( <*> ) a b = Fork(a, b)
  let inline ( <&> ) a b = (Fst >-> a) <*> (Snd >-> b)