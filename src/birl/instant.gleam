//// An instant given by a non-decreasing clock.

import gleam/order
import birl/duration

pub opaque type Instant {
  Instant(Int)
}

pub fn now() -> Instant {
  ffi_now()
  |> Instant
}

pub fn compare(a: Instant, b: Instant) {
  let Instant(ia) = a
  let Instant(ib) = b
  case ia == ib {
    True -> order.Eq
    False ->
      case ia < ib {
        True -> order.Lt
        False -> order.Gt
      }
  }
}

pub fn difference(a: Instant, b: Instant) {
  let Instant(ia) = a
  let Instant(ib) = b
  duration.Duration(ib - ia)
}

pub fn add(value: Instant, duration: duration.Duration) -> Instant {
  let Instant(instant) = value
  let duration.Duration(duration) = duration
  Instant(instant + duration)
}

pub fn subtract(value: Instant, duration: duration.Duration) -> Instant {
  let Instant(instant) = value
  let duration.Duration(duration) = duration
  Instant(instant - duration)
}

if erlang {
  external fn ffi_now() -> Int =
    "birl_ffi" "monotonic_now"
}

if javascript {
  external fn ffi_now() -> Int =
    "../birl_ffi.mjs" "monotonic_now"
}
