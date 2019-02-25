namespace TypeSafe

open NaturalNumbers

module Language =
  type Const = interface end
  
  type Star = Star with
    interface Const
  
  type Box = Box with
    interface Const

  type Lang = interface end

  type Const<'T> = Const of 'T with
    interface Lang
    static member inline (!!) ((Const _) as a) = a

  type Var<'T when 'T :> Nat > = Var of 'T with
    interface Lang
    static member inline (!!) ((Var _) as a) = a

  type Lambda<'TDef when 'TDef :> Lang> = Lambda of 'TDef with
    interface Lang
    static member inline (!!) (Lambda a) = Lambda(!!a)

  type Apply<'TApp> = Apply of 'TApp with
    interface Lang
    static member inline (!!) (Apply(Lambda(Var Z), a)) = a

  let Id = Lambda(Var Z)
  let first = Lambda(Lambda(Var Z))
  let second = Lambda(Lambda(Var(S Z)))

  let test1 = !!Apply(Id, Const 3)

