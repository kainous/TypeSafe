namespace TypeSafe

open System.Runtime.CompilerServices

module Semigroupoid =
  type Semigroupoid<'Source, 'Target> = 
    abstract ComposeWith : other:Semigroupoid<'Target, 'Result> -> Semigroupoid<'Source, 'Result>

  

  [<Extension>]
  type Semigroupoids() =
    [<Extension>]
    static member ( *>> ) (x:Semigroupoid<'A, 'B>, y:Semigroupoid<'B, 'C>) = x.ComposeWith y
  
    [<Extension>]
    static member ( <<* ) (x:Semigroupoid<'B, 'C>, y:Semigroupoid<'A, 'B>) = y.ComposeWith x
  
    [<Extension>]
    static member ( <<* ) (x:Semigroupoid<'C, 'B>, y:Semigroupoid<'B, 'A>) = y.ComposeWith x

  type Endo<'C, 'A> = Endo of 'C * 'A * 'A

  //type Relation<'Source, 'Target> = 


    //Test : source:'Source -> target:'Target -> bool
    
    
  //type Endorelation<'A> =
  //  inherit Relation<'A, 'A>
  //  abstract Refl : item:'A -> bool

  
  
  ////type ISemigroupoid<'Source, 'Target> =
  ////  abstract member ComposeWith : b:ISemigroupoid<'Target, 'Result> -> ISemigroupoid<'Source, 'Result>
  
  //type Semigroupoid<'Source, 'Target> = Semigroupoid of 'Source * 'Target with    
  //  static member inline ( >-> ) (Semigroupoid(a, _), Semigroupoid(_, c)) = Semigroupoid(a, c)
  //  static member inline ( ----> ) (Semigroupoid(a : 'T, b), v : 'T) = b


  //let a = Semigroupoid(3, "a")
  //let b = Semigroupoid("bob", 2.6)
  //let c = a >-> b
  //let d = c ----> 5

  //type Category<'Source, 'Target> = Category of 'Source * 'Target with


  // a type must be enumerable to have a relation as a category, instead of a semigroupoid and only endorelations have identity