module Algebras

type Magma<'T> =
  abstract Op : 'T -> 'T -> 'T

type MagmaObject<'TMagma, 'T when 'TMagma :> MagmaObject<'TMagma, 'T> > =
  inherit Magma<'TMagma>

type ExponentialMagma<'T> =
  inherit Magma<'T>

type ExponentialMagmaObject<'TWrapper, 'T when 'TWrapper :> ExponentialMagmaObject<'TWrapper, 'T> > =
  inherit ExponentialMagma<'T>

// Closed Monoidal Category
type Monoid<'T> =
  inherit Magma<'T>

type MonoidObject<'TMonoid, 'T when 'TMonoid :> MonoidObject<'TMonoid, 'T> > =
  inherit Monoid<'TMonoid>

type AbelianGroup<'T> =
  inherit Monoid<'T>

type Ring<'T> =
  abstract Coproduct : AbelianGroup<'T>
  abstract Product   : Monoid<'T>

type RingObject<'TRing, 'T when 'TRing :> RingObject<'TRing, 'T> > =
  inherit Ring<'T>

//Field
type ExponentialRing<'T> =
  inherit Ring<'T>
  abstract Exponential : ExponentialMagma<'T>

type ExponentialRingObject<'TWrapper, 'T when 'TWrapper :> ExponentialRingObject<'TWrapper, 'T> > =
  inherit ExponentialRing<'T>
  inherit RingObject<'TWrapper, 'T>

let ( <*> ) (a : 'TMagma when 'TMagma :> MagmaObject<'TMagma, 'T>) b = a.Op a b
let (  +  ) (a : 'TRing when 'TRing :> RingObject<'TRing, 'T> ) b = a.Coproduct.Op a b
let (  *  ) (a : 'TRing when 'TRing :> RingObject<'TRing, 'T> ) b = a.Product.Op a b
let (  ** ) (a : 'TRing when 'TRing :> ExponentialRingObject<'TRing, 'T> ) b = a.Exponential.Op a b