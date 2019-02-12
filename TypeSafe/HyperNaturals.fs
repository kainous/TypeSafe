namespace TypeSafe

module HyperNaturals =
  type HyperNat = interface end

  type Inf = Inf with
    interface HyperNat
    static member inline (!++) (Inf) = Inf
    static member inline (!--) (Inf) = Inf
    static member inline ( + ) (Inf, x : #HyperNat) = Inf
    static member inline ( * ) (Inf, x : #HyperNat) = Inf
    static member inline ( - ) (Inf, Z)   = Inf
    static member inline ( - ) (Inf, S _) = Inf
    static member inline (<<=) (Inf, Z)     = False
    static member inline (<<=) (Inf, (S _)) = False

  and Zero = Z with
    interface HyperNat

    static member inline (!++) (Z) = S Z
    static member inline ( + ) (Z, x : #HyperNat ) = x
    static member inline ( * ) (Z, x : #HyperNat ) = Z
    static member inline ( !&) (Z) = S Z
    static member inline (<<=) (Z, _ : #HyperNat) = True    
    static member inline ( - ) (x : #HyperNat, Z) = x
    static member inline (===) (Z, Z) = True
    static member inline (===) (Z, S _) = False
    static member inline (<%>) (_ : #HyperNat, Z) = Z
    static member inline (<&&&>) (Z, x) = x

  and Succ< 'T when 'T :> HyperNat > = private S of 'T with
    interface HyperNat

    static member inline (!++) (S _ as x) = S x    
    static member inline ( + ) (S x, y) = S (x + y)
    static member inline ( * ) (S x, y) = y + (x * y)
    static member inline ( !&) (S x) = (S x) * x  //Factorial    
    static member inline (!--) (S x) = x
    static member inline ( - ) (S x, S y) = x - y
    static member inline (<%>) (x, y : #HyperNat) = (x <<= y) <?> (x, x - y)
    static member inline (<<=) (S _, Z) = False
    static member inline (<<=) (S x, S y) = x <<= y
    static member inline (<<=) (S _, Inf) = True
    static member inline (===) (S _, Z) = False
    static member inline (===) (S x, S y) = (x === y)

  type Z0 = Zero
  type Z1 = Succ<Z0>
  type Z2 = Succ<Z1>
  type Z3 = Succ<Z2>
  type Z4 = Succ<Z3>

  //                   : State (by type)
  let Z0_ = Z       // : Zero
  let Z1_ = !++ Z0_ // : Succ<Zero>
  let Z2_ = !++ Z1_ // : Succ<Succ<Zero>>
  let Z3_ = !++ Z2_ // : Succ<Succ<Succ<Zero>>>
  let Z4_ = !++ Z3_ // : Succ<Succ<Succ<Succ<Zero>>>>

  let Inf0 = Inf     // : Inf
  let Inf1 = !++ Inf // : Inf

  let inline LeftIdentity (a) = a + Z0_ //Eq(a + Z0_, Z0_ + a)
  //let LeftIdentity1 = LeftIdentity Z1_

  //let s = Z3_ <&&&> Z1_ //Type inference too complicated
  
  

  type List = interface end

  type Nil = Nil with
    interface List
    static member inline ( >>= ) (Nil, _) =
      Nil

    static member inline ( <*>>= ) (Nil, (Nil, f)) =
      Nil

  and Cons<'TItem, 'TList when 'TList :> List > = Cons of 'TItem * 'TList with
    interface List
    static member inline ( >>= ) (Cons(item, list), f) =
      Cons(f item, list >>= f)  

    static member inline ( <*>>= ) (Cons(x, xs), (Cons(y, ys), f)) =
      Cons(f x y, xs <*>>= (ys, f))

  let vect2 a b = Cons(a, Cons(b, Nil))
  let vect3 a b c = Cons(a, vect2 b c)

  let x = vect2 2 3
  let y = vect3 5 2 1

  //let z = x <*>>= (y, (+))