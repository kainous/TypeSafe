namespace TypeSafe

open Functional

module Option =
  type Option = interface end

  type None = None with
    interface Option
    static member inline ( >>= )(None, f) =
      None


  type Some<'T> = Some of 'T with
    interface Option
    static member inline ( >>= )(Some x, f) =
      f x |> Some


module List =
  type List = interface end

  type Nil = Nil with
    interface List
    static member inline ( >>= )(Nil, f) =
      Nil
  
  type Cons<'TItem, 'TList> = Cons of 'TItem * 'TList with
    interface List
    static member inline ( >>= )(Cons(a, b), f) =
      Cons(f a, b >>= f)

  type Map<'f> = Map of 'f with
    interface Func
    static member inline ( <-- )(Map f, xs) =
      xs >>= f

  type Filter<'f> = Filter of 'f with
    interface Func
    static member inline ( <-- )(Filter f, Cons(a, b)) =
      (f <-- a) <?> (Cons(a, Filter(f, b)), Filter(f, b))

    static member inline ( <-- )(Filter _, Nil) =
      Nil

  type Join = Join with
    interface Func
    static member inline ( <-- )(Join, a) =
      a >>= Id

  type IdMonad = IdMonad with
    static member inline ( >>= )(x, f) =
      f x


  //type Fold<'f, 'x> = Fold of 'f * 'x with
  //  interface Func
  //  static member inline ( <-- )(Fold f, )

  //type Kleisi<'M, 'f, 'g> = Kleisi of 'M * 'f * 'g with
  //  interface Func
  //  static member inline ( <-- )(Kleisi(m, f, g), (a, b)) =
  //    <-- 