namespace TypeSafe

[<AutoOpen>]
module Boolean =
  type Bool = interface end

  type False = False with
    interface Bool
    static member inline ( <?> ) (False, (_, y)) = y

  type True = True with
    interface Bool
    static member inline ( <?> ) (True, (x, _)) = x

  type If<'p, 'a, 'b> = If of 'p * 'a * 'b with
    interface Func
    static member inline ( <-- ) (If(p, a, b), v) =
      v --> ((v --> p) <?> (a, b))

  type If<'p, 'a> = If0 of 'p * 'a with
    interface Func
    static member inline ( <-- ) (If0(p, a), v) =
      v --> ((v --> p) <?> (a, Id))

  type Not = Not with
    interface Func
    static member inline ( <-- ) (Not, True)  = False
    static member inline ( <-- ) (Not, False) = True

  type And = And with
    interface Func
    static member inline ( <-- ) (And, (True, True))   = True
    static member inline ( <-- ) (And, (False, True))  = False
    static member inline ( <-- ) (And, (True, False))  = False
    static member inline ( <-- ) (And, (False, False)) = False

  type Or = Or with
    interface Func
    static member inline ( <-- ) (And, (True, True))   = True
    static member inline ( <-- ) (And, (False, True))  = True
    static member inline ( <-- ) (And, (True, False))  = True
    static member inline ( <-- ) (And, (False, False)) = False

  type While<'p, 'f> = While of 'p * 'f with
    interface Func
    static member inline ( <-- ) (While(p, f), v) =
      ((p <-- v) <?> (While(p, f) <-< f, Id)) <-- v


  type Eq<'T> = Eq of 'T * 'T with
    interface Func
    static member inline ( <-- ) (Eq(_, _), (_, _)) = True