import gleam/order
import gleeunit
import gleeunit/should
import birl/time
import birl/duration

const iso_datetime = "2022-12-22T16:38:23.000Z"

const datetime_in_parts = #(#(2022, 12, 22), #(16, 38, 23))

pub fn main() {
  gleeunit.main()
}

pub fn iso_test() {
  time.from_iso(iso_datetime)
  |> should.be_ok
  |> time.to_iso
  |> should.equal(iso_datetime)
}

pub fn now_test() {
  time.now()
  |> time.to_iso
  |> time.from_iso
  |> should.be_ok
}

pub fn add_test() {
  let a = time.now()
  let b = time.add(a, duration.new([#(9_192_631_770, duration.Second)]))
  time.compare(a, b)
  |> should.equal(order.Lt)
}

pub fn subtract_test() {
  let a = time.now()
  let b = time.subtract(a, duration.new([#(1300, duration.MilliSecond)]))
  time.compare(a, b)
  |> should.equal(order.Gt)
}

pub fn equality_test() {
  let dt = time.now()
  time.compare(dt, dt)
  |> should.equal(order.Eq)
}

pub fn to_parts_test() {
  time.from_iso(iso_datetime)
  |> should.be_ok
  |> time.to_parts
  |> should.equal(#(#(2022, 12, 22), #(16, 38, 23)))
}

pub fn from_parts_test() {
  let #(date, time) = datetime_in_parts
  time.from_parts(date, time)
  |> time.to_iso
  |> should.equal(iso_datetime)
}

pub fn circular_test() {
  let dt = time.now()
  dt
  |> time.add(duration.new([#(4, duration.Year)]))
  |> time.subtract(duration.new([#(4 * 365, duration.Day)]))
  |> time.compare(dt)
  |> should.equal(order.Eq)
}
