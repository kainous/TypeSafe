module FreeMonad

open System

type Slot = {
  Date      : DateTimeOffset
  SeatsLeft : int
}

type Reservation = {
  Date     : DateTimeOffset
  Name     : string
  Email    : string
  Quantity : int
}

type ReservationsApiInstruction<'a> =
| GetSlots of (DateTimeOffset * (Slot list -> 'a))
| PostReservation of Reservation * 'a

let private mapI f = function
| GetSlots(x, next) -> GetSlots(x, next >> f)
| PostReservation(x, next) -> PostReservation(x, next |> f)

type ReservationsApiProgram<'a> =
| Free of ReservationsApiInstruction<ReservationsApiProgram<'a>>
| Pure of 'a

let rec bind f = function
| Free instruction -> instruction |> mapI (bind f) |> Free
| Pure x -> f x

let map f = bind (f >> Pure)

let getSlots date = Free (GetSlots (date, Pure))
let postReservation r = Free (PostReservation(r, Pure()))

type ReservationsApiBuilder() =
  member __.Bind(x, f) = bind f x
  member __.Return x = Pure x
  member __.ReturnFrom x = x
  member __.Zero() = Pure ()

let api = ReservationsApiBuilder()

let a = api {
  let! a = getSlots DateTimeOffset.Now
  let! b = postReservation {
    Date = DateTimeOffset.Now
    Name = "Nick Cooper"
    Email = "nick.cooper@halliburton.com"
    Quantity = 3
  }
  return a
}