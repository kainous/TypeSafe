module Orders

open Algebras

// Preorder = category and nothing else...Can Reach describing any binary relation
type Preorder<'TValue, 'TLogic> =
  abstract CanReach : 'TValue -> 'TValue -> 'TLogic

type PreorderedObject<'TValue, 'TLogic when 'TValue :> PreorderedObject<'TValue, 'TLogic> > =
  inherit Preorder<'TValue, 'TLogic>

type PartialOrder<'TValue, 'TLogic> =
  inherit Preorder<'TValue, 'TLogic>

type PartiallyOrderedObject<'TValue, 'TLogic when 'TValue :> PartiallyOrderedObject<'TValue, 'TLogic>> =
  inherit PartialOrder<'TValue, 'TLogic>

type Lattice<'TValue, 'TLogic> =
  inherit PartialOrder<'TValue, 'TLogic>
  inherit Ring<'TValue>

type LatticeNode<'TValue, 'TLogic when 'TValue :> LatticeNode<'TValue, 'TLogic> > =
  inherit PartialOrder<'TValue, 'TLogic>
  inherit RingObject<'TValue, 'TValue>

type Heyting<'TValue, 'TLogic> =
  inherit Lattice<'TValue, 'TLogic>
  inherit ExponentialRing<'TValue>

type HeytingNode<'TValue, 'TLogic when 'TValue :> Heyting<'TValue, 'TLogic> and 'TValue :> ExponentialRingObject<'TValue, 'TValue> > =
  inherit Heyting<'TValue, 'TLogic>
  inherit ExponentialRingObject<'TValue, 'TValue>

type TotalOrder<'TValue, 'TLogic> =
  inherit Heyting<'TValue, 'TLogic>

type TotallyOrderedObject<'TValue, 'TLogic when 'TValue :> Heyting<'TValue, 'TLogic> and 'TValue :> ExponentialRingObject<'TValue, 'TValue> > =
  inherit TotalOrder<'TValue, 'TLogic>
  inherit HeytingNode<'TValue, 'TLogic>

let ( --> ) (a:'TValue when 'TValue :> Preorder<_,_>) (b:'TValue) =
  a.CanReach a b