module Equality
  type Eq<'A, 'B> = private Refl of ('A -> 'B) * ('B -> 'A)

  let refl<'A>() : Eq<'A, 'A> = Refl(id, id)
  let sym (Refl(f, g) : Eq<'A, 'B>) : Eq<'B, 'A> = Refl(g, f)
  let trans (Refl(f, g) : Eq<'A, 'B>) (Refl(h, k) : Eq<'B, 'C>) = Refl(f >> h, k >> g)
  let cast (Refl(f, _) : Eq<'A, 'B>) = f

  type EqImpl<'A, 'B> =
    abstract Refl  : Eq<'A, 'A>
    abstract Sym   : Eq<'A, 'B> -> Eq<'B, 'A>
    abstract Trans : Eq<'A, 'B> -> Eq<'B, 'C> -> Eq<'A, 'C>

  type Nat = interface end

  type Succ<'T when 'T :> Nat > = Succ of 'T with
    interface Nat

  type Zero = Zero with
    interface Nat

  type Integer1 = Integer1 of Nat * Nat

  type Integer2 = Integer2 of bool * Nat

  type List<'T> = interface end

  type Cons<'Head, 'Tail when 'Tail :> List<'Head> > = Cons of ('Head * 'Tail) with
    interface List<'Head>
    static member inline ( ^+ ) (a : 'Head, b : #List<'Head> ) = Cons(a, b)
    static member inline ( ++ ) (Cons(ah:'Head, at), (b : #List<'Head>)) = Cons(ah, at ++ b)

  type Nil<'T> = private Nil of unit with
    interface List<'T>
    static member inline ( ^+ ) (a : 'T, _ : Nil<'T>) = Cons(a, Nil())
    static member inline ( ++ ) (a : 'Head when 'Head :> #List<'T>, _:Nil<'T>) = a
    static member inline ( ++ ) (_:Nil<'T>, b : 'Head when 'Head :> #List<'T>) = b

  //type EqVectorSize<'T>() =
  //  interface Vect<'T> with
  //    interface EqImpl<'T> with
  //      member __.Refl = refl 
  
  let nil = Nil()

  let a = 1 ^+ 2 ^+ nil
  let b = 3 ^+ 4 ^+ nil
  let c = a ++ b

  //let v1() = Cons <| fun () -> 1, Cons <| fun () -> 2, (Nil() : Nil<int>)
  //let v2 = Cons<| fun () -> (2 , v1)