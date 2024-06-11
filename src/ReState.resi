type t<'a>

let make: 'a => t<'a>

let computed: (~equalityCheck: ('a, 'a) => bool=?, unit => 'a) => t<'a>

@send
external get: t<'a> => 'a = "g"

@send
external set: (t<'a>, 'a) => unit = "s"

type unsubscribe = unit => unit

let subscribe: (t<'a>, 'a => unit) => unsubscribe

type notify = (. unit) => unit

let notify: notify

let wrapNotify: (notify => unit) => unit
