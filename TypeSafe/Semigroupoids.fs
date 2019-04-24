namespace TypeSafe

open System.Runtime.CompilerServices

module Semigroupoid =
  type Semigroupoid<'Container1, 'Source, 'Target when 'Container1 :> Semigroupoid<'Container1, 'Source, 'Target> > = 
    abstract ComposeWith<'Container2, 'Container3, 'Result 
      when 'Container2 :> Semigroupoid<'Container2, 'Target, 'Result>
      and  'Container3 :> Semigroupoid<'Container3, 'Source, 'Result> > : other:Semigroupoid<'Container2, 'Target, 'Result> -> Semigroupoid<'Container3, 'Source, 'Result>

  //type Relation<'A, 'B> =
  //  inherit Semigroupoid<'A, 'B>


  [<Extension>]
  type Semigroupoids() =
    [<Extension>]
    static member ( *>> ) (x:Semigroupoid<'Container1, 'A, 'B>, y:Semigroupoid<'Container2, 'B, 'C>) = x.ComposeWith y
  
    [<Extension>]
    static member ( <<* ) (x:Semigroupoid<'Container1, 'B, 'C>, y:Semigroupoid<'Container2, 'A, 'B>) = y.ComposeWith x  

  type Endo<'C, 'A> = Endo of 'C * 'A * 'A

  type FinitePureMultifunction<'Source, 'Target when 'Source : comparison and 'Target : comparison >(multifunc : Map<'Source, Set<'Target>>) =
    member private __.Items = 
      multifunc
    
    member __.Apply (source : 'Source) =
      multifunc.[source]

    interface Semigroupoid<FinitePureMultifunction<'Source, 'Target>, 'Source, 'Target> with
      member this.ComposeWith other =
        seq {
          for (key, items) in this.Items |> Map.toSeq do
          for i in items do
          yield key, other.Items |> Map.find i
        } |> Map.ofSeq

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