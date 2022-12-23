import gleam/int
import gleam/list

pub type Duration {
  Duration(Int)
}

pub type Unit {
  MilliSecond
  Second
  Minute
  Hour
  Day
  Week
  Month
  Year
}

const second = 1_000

const minute = 60_000

const hour = 3_600_000

const day = 86_400_000

const week = 604_800_000

const month = 2_592_000_000

const year = 31_536_000_000

const accurate_year = 31_556_952_000

const accurate_month = 26_297_460_00

/// Use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(
    0,
    fn(total, current) {
      let #(amount, unit) = current
      case unit {
        MilliSecond -> total + amount
        Second -> total + amount * second
        Minute -> total + amount * minute
        Hour -> total + amount * hour
        Day -> total + amount * day
        Week -> total + amount * week
        Month -> total + amount * month
        Year -> total + amount * year
      }
    },
  )
  |> Duration
}

/// Use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_new(values: List(#(Int, Unit))) -> Duration {
  values
  |> list.fold(
    0,
    fn(total, current) {
      let #(amount, unit) = current
      case unit {
        MilliSecond -> total + amount
        Second -> total + amount * second
        Minute -> total + amount * minute
        Hour -> total + amount * hour
        Day -> total + amount * day
        Week -> total + amount * week
        Month -> total + amount * accurate_month
        Year -> total + amount * accurate_year
      }
    },
  )
  |> Duration
}

/// Use this if you need short durations where a year just means 365 days and a month just means 30 days
pub fn decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let #(years, remaining) = extract(value, year)
  let #(months, remaining) = extract(remaining, month)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)

  [
    #(years, Year),
    #(months, Month),
    #(days, Day),
    #(hours, Hour),
    #(minutes, Minute),
    #(seconds, Second),
    #(remaining, MilliSecond),
  ]
  |> list.filter(fn(item) { item.0 > 0 })
}

/// Use this if you need very long durations where small inaccuracies could lead to large errors
pub fn accurate_decompose(duration: Duration) -> List(#(Int, Unit)) {
  let Duration(value) = duration
  let #(years, remaining) = extract(value, accurate_year)
  let #(months, remaining) = extract(remaining, accurate_month)
  let #(days, remaining) = extract(remaining, day)
  let #(hours, remaining) = extract(remaining, hour)
  let #(minutes, remaining) = extract(remaining, minute)
  let #(seconds, remaining) = extract(remaining, second)

  [
    #(years, Year),
    #(months, Month),
    #(days, Day),
    #(hours, Hour),
    #(minutes, Minute),
    #(seconds, Second),
    #(remaining, MilliSecond),
  ]
  |> list.filter(fn(item) { item.0 > 0 })
}

fn extract(duration: Int, unit: Int) -> #(Int, Int) {
  assert Ok(amount) = int.divide(duration, unit)
  assert Ok(remaining) = int.modulo(duration, unit)
  #(amount, remaining)
}
