namespace TypeSafe

open NaturalNumbers

module Language =
  type Const = interface end
  
  type Star = Star with
    interface Const
  
  type Box = Box with
    interface Const

  type Lang = interface end

  type Embed<'T> = Embed of 'T with
    interface Lang
    static member inline ( <&&&> ) (Embed a, _) = Embed a
    static member inline (!!) (Embed a) = Embed a

  type Var<'T when 'T :> Nat > = Var of 'T with
    interface Lang
    static member inline ( <&&&> ) (Var Z, Cons(head, _)) = head
    static member inline ( <&&&> ) ((Var (S a)), Cons(_, tail)) = (Var a) <&&&> tail
    static member inline (!!) ((Var _) as a) = a

  type Lambda<'TDef when 'TDef :> Lang> = Lambda of 'TDef with
    interface Lang
    static member inline ( <&&&> ) (Lambda(a), b) = a <&&&> Cons(Lambda(a), b)
    //static member inline ( <&&&> ) (Lambda(Var Z), Cons(head, _)) = head
    static member inline (!!) (Lambda a) = a <&&&> Empty

  type Apply<'E, 'V> = Apply of 'E * 'V with
    interface Lang
    static member inline ( <&&&> ) (Apply(a, b), tail) = a <&&&> Cons(b, tail)
    static member inline (!!) ((Apply _) as a) = a <&&&> Empty

  let T = fun x -> fun y -> x
  let T' = !!Apply(Lambda(Lambda(Var(S Z))), Embed ())

  let Id() = !!(Apply(Lambda(Lambda(Var Z)), Embed Z2_))
  let first = Lambda(Lambda(Var Z))
  let second = Lambda(Lambda(Var(S Z)))

  let test1 = !!Apply(Id, Const 3)

