namespace TypeSafe

module NaturalNumbers =
  type Nat = interface end

  and Zero = Z with
    interface Nat    
    static member inline (!++) (Z) = S Z
    static member inline ( + ) (Z, x : #Nat ) = x
    static member inline ( * ) (Z, x : #Nat ) = Z
    static member inline ( !&) (Z) = S Z
    static member inline (<<=) (Z, _ : #Nat) = True    
    static member inline ( - ) (x : #Nat, Z) = x
    static member inline (===) (Z, Z) = True
    static member inline (===) (Z, S _) = False
    static member inline (<%>) (_ : #Nat, Z) = Z
    static member inline (<&&&>) (Z, x) = x

  and Succ< 'T when 'T :> Nat > = S of 'T with
    interface Nat
    static member inline (!++) (S _ as x) = S x    
    static member inline ( + ) (S x, y) = S (x + y)
    static member inline ( * ) (S x, y) = y + (x * y)
    static member inline ( !&) (S x) = (S x) * x  //Factorial    
    static member inline (!--) (S x) = x
    static member inline ( - ) (S x, S y) = x - y
    static member inline (<%>) (x, y : #Nat) = (x <<= y) <?> (x, x - y)
    static member inline (<<=) (S _, Z) = False
    static member inline (<<=) (S x, S y) = x <<= y
    static member inline (===) (S _, Z) = False
    static member inline (===) (S x, S y) = (x === y)

  type Z0 = Zero
  type Z1 = Succ<Z0>
  type Z2 = Succ<Z1>
  type Z3 = Succ<Z2>
  type Z4 = Succ<Z3>  

  let Z0_ = Z
  let Z1_ = S Z0_
  let Z2_ = S Z1_
  let Z3_ = S Z2_
  let Z4_ = S Z3_

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
    interface List<'TItem>
    static member inline ( >>= ) (Cons(item, list), f) =
      Cons(f item, list >>= f)  

    static member inline ( <*>>= ) (Cons(x, xs), (Cons(y, ys), f)) =
      Cons(f x y, xs <*>>= (ys, f))

  let vect2 a b = Cons(a, Cons(b, Nil))
  let vect3 a b c = Cons(a, vect2 b c)

  let x = vect2 2 3
  let y = vect3 5 2 1

  //let z = x <*>>= (y, (+))