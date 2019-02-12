namespace TypeSafe

open System

module Ordering =
  type Space = interface end

  type Apart = inherit Space      // Incomparable, but not equal
  type IsLessThan = inherit Apart     
  type IsGreaterThan = inherit Apart
  