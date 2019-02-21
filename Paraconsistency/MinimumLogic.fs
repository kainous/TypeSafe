module MinimumLogic

type Minimum =
| Embed of string
| Falsity
| Imply of (Minimum * Minimum)

let (-->) a b = Imply(a, b)
let ( ! ) p = p --> Falsity

let ( && ) p q = !(p --> !q)
let ( || ) p q = (p --> q) --> q
let ( <--> ) p q = (p --> q) && (q --> p)

//let (|AND|) p q =
//  match p, q with
//  | Imply(p, Imply(q, False))

let evalIPC = function
| Imply(p, Imply(_, r)) -> Imply(p, r)

// Schema 1 introduces q out of nowhere
| Imply(p, Imply(q, r)) -> (p --> q) --> (p --> r) // Schema 2
| Imply(p, Imply(_, r)) when p = r -> p // Pierce's Law - Schema 3

let evalMinimal = function
| Imply(Falsity, Imply(b, Falsity)) -> Imply(b, Falsity)

let evalIntuitionistic = function
| Imply(Falsity, a) -> a

let evalClassical = function
| Imply(Imply(a, Falsity), Falsity) -> a