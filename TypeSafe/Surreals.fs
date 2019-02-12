namespace Surreals

open TypeSafe.NaturalNumbers



module Surreals =
  type Surreal<'TLeft, 'TRight when 'TLeft :> List and 'TRight :> List > = Surreal of 'TLeft * 'TRight with
    