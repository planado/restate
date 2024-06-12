module Array = Js.Array2

module Promise = {
  type t<+'a> = promise<'a>
  @send
  external thenResolveUnit: (t<unit>, unit => 'b) => t<'b> = "then"
  @val @scope("Promise")
  external resolve: unit => t<'a> = "resolve"
}

module Fn = {
  let callWithFinally: (unit => 'a, unit => unit) => 'a = %raw(`(fn, finallyCb) => {
    try {
      return fn()
    } finally {
      finallyCb()
    }
  }`)

  @inline
  let call1 = (fn: 'arg1 => 'return, arg1: 'arg1): 'return => {
    // Obj.magic(fn)(. arg1)

    fn(arg1)
  }

  @inline
  let call2 = (fn: ('arg1, 'arg2) => 'return, arg1: 'arg1, arg2: 'arg2): 'return => {
    // Obj.magic(fn)(. arg1, arg2)

    fn(arg1, arg2)
  }
}

type notify = unit => unit

type effect = unit => unit

type unsubscribe = unit => unit

type queue = array<array<effect>>

type t<'a> = {
  @as("a")
  state: 'a,
  @as("g")
  get: unit => 'a,
  @as("s") @dead
  set: 'a => unit,
}

type pub<'a> = {
  @as("a")
  act: t<'a>,
  @as("s")
  stateSnapshot: 'a,
}

type valueAct<'a> = {
  @as("a")
  mutable state: 'a,
  @as("v")
  mutable version: float,
  @as("e")
  mutable effects: array<effect>,
  @as("g") @dead
  mutable get: unit => 'a,
  @as("s") @dead
  mutable set: 'a => unit,
}

type computedAct<'a> = {
  @as("a")
  mutable state: 'a,
  @as("v")
  mutable version: float,
  @as("p")
  mutable pubs: array<pub<unknown>>,
  @as("g") @dead
  mutable get: unit => 'a,
  @as("s") @dead
  mutable set: 'a => unit,
}

type rec context = {
  // a subscriber - source of truth
  @as("r")
  mutable maybeRoot: option<effect>,
  // a subscriber to unsubscribe
  @as("u")
  mutable maybeUnroot: option<effect>,
  // effects queue for a batch, also used as a cache key of a transaction
  @as("q")
  mutable queue: queue,
  // global `dirty` flag used to cache visited nodes during it invalidation by a subscriber
  @as("v")
  mutable version: float,
  // list of a publishers from a computed in prev stack step
  @as("p")
  mutable maybePubs: option<array<pub<unknown>>>,
  @as("n")
  mutable notify: notify,
}

type subscribtionContext = {
  @as("q")
  mutable lastQueue: queue,
  @as("s")
  mutable lastState: unknown,
}

let castUnknownToAny: unknown => 'a = Obj.magic(_)

let castAnyToUnknown: 'a => unknown = Obj.magic(_)

let castAnyActToUnknownAct: t<'a> => t<unknown> = Obj.magic(_)

let castUnknownActToAnyAct: t<unknown> => t<'a> = Obj.magic(_)

let castValueActToGeneric: valueAct<'a> => t<'a> = Obj.magic(_)

let castComputedActToGeneric: computedAct<'a> => t<'a> = Obj.magic(_)

let initialActVersion = -1.

let context = {
  maybeRoot: None,
  maybeUnroot: None,
  queue: [],
  maybePubs: None,
  version: 0.,
  notify: %raw("undefined"),
}

context.notify = () => {
  let iterator = context.queue

  context.queue = []

  for effectsIdx in 0 to iterator->Array.length - 1 {
    let effects = iterator->Array.unsafe_get(effectsIdx)
    for effectIdx in 0 to effects->Array.length - 1 {
      let effect = effects->Array.unsafe_get(effectIdx)
      effect()
    }
  }
}

let notify = () => context.notify()

let wrapNotify = fn => {
  let prevNotify = context.notify
  context.notify = () => fn->Fn.call1(prevNotify)
}

@inline
let addPublisher = (act: t<'a>) => {
  switch context.maybePubs {
  | Some(pubs) =>
    pubs
    ->Array.push({
      act,
      stateSnapshot: act.state,
    })
    ->ignore

  | None => ()
  }
}

@inline
let syncEffects = (valueAct: valueAct<'a>) => {
  if valueAct.version !== context.version {
    valueAct.version = context.version
    switch (context.maybeUnroot, context.maybeRoot) {
    | (Some(unroot), _) =>
      valueAct.effects
      ->Array.removeCountInPlace(~pos=valueAct.effects->Array.indexOf(unroot), ~count=1)
      ->ignore
    | (_, Some(root)) => valueAct.effects->Array.push(root)->ignore
    | _ => ()
    }
  }
}

let make = initial => {
  let valueAct = {
    state: initial,
    version: initialActVersion,
    effects: [],
    get: %raw("undefined"),
    set: %raw("undefined"),
  }

  {
    let valueAct = valueAct->(Obj.magic(_): valueAct<'a> => valueAct<unknown>)
    valueAct.get = () => {
      valueAct->syncEffects
      valueAct->castValueActToGeneric->addPublisher
      valueAct.state->castUnknownToAny
    }
    valueAct.set = state => {
      let state = state->castAnyToUnknown
      valueAct.state = state

      if context.queue->Array.push(valueAct.effects) === 1 {
        Promise.resolve()->Promise.thenResolveUnit(notify)->ignore
      }

      valueAct.effects = []

      valueAct->syncEffects
      valueAct->castValueActToGeneric->addPublisher
    }
  }

  valueAct->castValueActToGeneric
}

let computed = (~equalityCheck as maybeEqualityCheck=?, fn) => {
  let computedAct = {
    state: %raw("undefined"),
    version: initialActVersion,
    pubs: [],
    get: %raw("undefined"),
    set: ignore,
  }

  {
    let computedAct = computedAct->(Obj.magic(_): computedAct<'a> => computedAct<unknown>)
    computedAct.get = () => {
      if computedAct.version !== context.version || context.maybeRoot === None {
        let computedPubs = computedAct.pubs
        let prevPubs = context.maybePubs
        context.maybePubs = None

        let isEmptyComputedPubs = computedPubs->Array.length === 0
        if (
          isEmptyComputedPubs ||
          computedPubs->Array.some(el =>
            (el.act->castUnknownActToAnyAct).get->Fn.call1() !== el.stateSnapshot
          )
        ) {
          let newPubs = isEmptyComputedPubs ? computedPubs : []
          context.maybePubs = Some(newPubs)
          computedAct.pubs = newPubs

          let newState = fn->Fn.call1()->castAnyToUnknown
          if (
            computedAct.version === initialActVersion ||
              switch maybeEqualityCheck {
              | Some(equalityCheck) =>
                equalityCheck
                ->Fn.call2(computedAct.state->castUnknownToAny, newState->castUnknownToAny)
                ->not
              | None => true
              }
          ) {
            computedAct.state = newState
          }
        }

        context.maybePubs = prevPubs

        computedAct.version = context.version
      }

      computedAct->castComputedActToGeneric->addPublisher
      computedAct.state->castUnknownToAny
    }
  }

  computedAct->castComputedActToGeneric
}

@send
external get: t<'a> => 'a = "g"

@send
external set: (t<'a>, 'a) => unit = "s"

let subscribe = (act, cb) => {
  let act = act->castAnyActToUnknownAct

  let subscribtionContext = {
    lastQueue: %raw("cb"),
    lastState: %raw("cb"),
  }

  let rec effect = () => {
    if subscribtionContext.lastQueue !== context.queue {
      subscribtionContext.lastQueue = context.queue

      context.version = context.version +. 1.

      let prevRoot = context.maybeRoot
      context.maybeRoot = Some(effect)

      Fn.callWithFinally(
        () => {
          let calculatedState = act->get
          if subscribtionContext.lastState !== calculatedState {
            subscribtionContext.lastState = calculatedState
            cb->Fn.call1(calculatedState->castUnknownToAny)
          }
        },
        () => {
          context.maybeRoot = prevRoot
        },
      )
    }
  }

  effect()

  () => {
    context.version = context.version +. 1.
    context.maybeUnroot = Some(effect)
    act->get->ignore
    context.maybeUnroot = None
  }
}
