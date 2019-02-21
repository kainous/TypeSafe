module Arrows

//type CategoricalObject< 'TWrapper, 'TObject when 'TWrapper :> CategoricalObject<'TWrapper, 'TObject> > =
//  inherit Category

//and Category =
//  abstract Identity : CategoricalObject<'MA, 'A> -> CategoricalObject<'MA, 'A>
//  abstract ComposeWith : CategoricalObject<'MA, 'A> -> CategoricalObject<'MB, 'B> -> CategoricalObject<'MC, 'C>

//type ArrowObject< 'TWrapper, 'TObject when 'TWrapper :> ArrowObject<'TWrapper, 'TObject> > =
//  inherit Arrow

//and Arrow =
//  inherit Category
//  abstract WrapFirst : f:('A -> 'B) -> ArrowObject< 'MAB, 'A -> 'B >
//  abstract MapFirst  : ArrowObject<'MAB, 'A -> 'B> -> ArrowObject<'MABC, ('A -> 'B) * ('C -> 'C)>


//type Parser<'S, 'A, 'B> = P of StaticParser<'S> * DynamicParser<'S>
//and 