import gleam/list
import gleam/option
import gleam/order

import birl
import birl/duration

pub opaque type Interval {
  Interval(start: birl.Time, end: birl.Time)
}

pub fn from_start_and_end(start: birl.Time, end: birl.Time) {
  case birl.compare(start, end) {
    order.Eq -> Error(Nil)
    order.Lt -> Ok(Interval(start, end))
    order.Gt -> Ok(Interval(end, start))
  }
}

pub fn from_start_and_duration(start: birl.Time, duration: duration.Duration) {
  from_start_and_end(start, birl.add(start, duration))
}

pub fn shift(interval: Interval, duration: duration.Duration) {
  let Interval(start, end) = interval
  Interval(birl.add(start, duration), birl.add(end, duration))
}

pub fn scale_up(interval: Interval, factor: Int) {
  let Interval(start, end) = interval
  let assert Ok(scaled) =
    birl.difference(end, start)
    |> duration.scale_up(factor)
    |> from_start_and_duration(start, _)
  scaled
}

pub fn scale_down(interval: Interval, factor: Int) {
  let Interval(start, end) = interval
  let assert Ok(scaled) =
    birl.difference(end, start)
    |> duration.scale_down(factor)
    |> from_start_and_duration(start, _)
  scaled
}

pub fn intersection(a: Interval, b: Interval) -> option.Option(Interval) {
  case contains(a, b), contains(b, a) {
    True, False -> option.Some(b)
    False, True -> option.Some(a)
    _, _ -> {
      let Interval(a_start, a_end) = a
      let Interval(b_start, b_end) = b

      case includes(a, b_start), includes(b, a_start) {
        True, False -> option.Some(Interval(b_start, a_end))
        False, True -> option.Some(Interval(a_start, b_end))
        _, _ -> option.None
      }
    }
  }
}

pub fn includes(interval: Interval, time: birl.Time) {
  let Interval(start, end) = interval
  list.contains([order.Eq, order.Lt], birl.compare(start, time))
  && list.contains([order.Eq, order.Gt], birl.compare(end, time))
}

pub fn contains(a: Interval, b: Interval) {
  let Interval(start, end) = b
  includes(a, start) && includes(a, end)
}

pub fn get_bounds(interval: Interval) -> #(birl.Time, birl.Time) {
  let Interval(start, end) = interval
  #(start, end)
}
