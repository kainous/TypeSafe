module Algebras

type BinaryOperation<'T> =
  abstract Glom : 'T -> 'T -> 'T

// Left-Identity element != Right-Identity Element - is partial due to 0^0
type Exponential<'T> =
  inherit BinaryOperation<'T>

// Implies associativity of the Binary Operation
type AssociativeOperation<'T> =
  inherit BinaryOperation<'T>

type CommutativeOperation<'T> =
  inherit BinaryOperation<'T>


// a.k.a. Closed Monoidal Category
// Property that 
type Monoid<'T> =
  inherit BinaryOperation<'T>

type Magma<'TObject, 'TOperation when 'TOperation :> BinaryOperation<'TObject> > = Magma of Op:'TOperation

// This fails to make Op show as exponential
type ExponentialMagma< 'TObject, 'TOperation when 'TOperation :> Exponential<'TObject> > =
  ExponentialMagma of Op:'TOperation



type MonoidObject<'TMonoid, 'T when 'TMonoid :> MonoidObject<'TMonoid, 'T> > =
  inherit Monoid<'TMonoid>

type CommutativeMonoid<'T> =
  inherit Monoid<'T>

type CommutativeMonoidObject<'TWrapper, 'T when 'TWrapper :> CommutativeMonoidObject<'TWrapper, 'T> > =
  inherit CommutativeMonoid<'TWrapper>

type CommutativeGroup<'T> =
  inherit Monoid<'T>

type FieldLike<'TCoproduct, 'TProduct> =
  abstract Coproduct : 'TCoproduct
  abstract Product   : 'TProduct

type Semiring<'T> =
  inherit FieldLike<AbelianMonoid<'T>, Monoid<'T>>

type Ring<'T> =
  inherit FieldLike<AbelianGroup<'T>, Monoid<'T>>

type Field<'T> =
  inherit FieldLike<AbelianGroup<'T>, Group<'T>>

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

let ( <*> ) (a : 'TMagma when 'TMagma :> Magma<'TMagma, 'T>) b = a.Op a b
let (  +  ) (a : 'TRing when 'TRing :> RingObject<'TRing, 'T> ) b = a.Coproduct.Op a b
let (  *  ) (a : 'TRing when 'TRing :> RingObject<'TRing, 'T> ) b = a.Product.Op a b
let (  ** ) (a : 'TRing when 'TRing :> ExponentialRingObject<'TRing, 'T> ) b = a.Exponential.Op a b