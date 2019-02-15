module ParaconsistentLogic
type Paraconsistent =
| Truth
| Pro  of string
| Neg' of Paraconsistent
| Con' of Paraconsistent * Paraconsistent
| Eq   of Paraconsistent * Paraconsistent
| Eq'  of Paraconsistent * Paraconsistent

let Falsity  = Neg' Truth
let Dis' p q = Neg'(Con'((Neg' p), (Neg' q)))
let Imp p q  = Eq(p, Con'(p, q))
let Imp' p q = Eq'(p, Con'(p, q))
let Box p = Eq(p, Truth)
let Neg p = Box(Neg' p)
let Con p q = Box(Con'(p, q))
let Dis p q = Box(Dis' p  q)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code


