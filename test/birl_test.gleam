import gleam/order
import gleeunit
import gleeunit/should
import birl/datetime

const iso_datetime = "2022-12-22T16:38:23.000Z"

const datetime_in_parts = #(
  datetime.Date(2022, 12, 22),
  datetime.Time(16, 38, 23),
)

pub fn main() {
  gleeunit.main()
}

pub fn iso_test() {
  datetime.from_iso(iso_datetime)
  |> should.be_ok
  |> datetime.to_iso
  |> should.equal(iso_datetime)
}

pub fn new_test() {
  datetime.new()
  |> datetime.to_iso
  |> datetime.from_iso
  |> should.be_ok
}

pub fn add_test() {
  let a = datetime.new()
  let b = datetime.add(a, 9_192_631_770, datetime.Second)
  datetime.compare(a, b)
  |> should.equal(order.Lt)
}

pub fn subtract_test() {
  let a = datetime.new()
  let b = datetime.subtract(a, 1300, datetime.MilliSecond)
  datetime.compare(a, b)
  |> should.equal(order.Gt)
}

pub fn equality_test() {
  let dt = datetime.new()
  datetime.compare(dt, dt)
  |> should.equal(order.Eq)
}

pub fn to_parts_test() {
  datetime.from_iso(iso_datetime)
  |> should.be_ok
  |> datetime.to_parts
  |> should.equal(#(datetime.Date(2022, 12, 22), datetime.Time(16, 38, 23)))
}

pub fn from_parts_test() {
  let #(date, time) = datetime_in_parts
  datetime.from_parts(date, time)
  |> datetime.to_iso
  |> should.equal(iso_datetime)
}

pub fn circular_test() {
  let dt = datetime.new()
  dt
  |> datetime.add(22, datetime.Minute)
  |> datetime.subtract(22 * 60, datetime.Second)
  |> datetime.compare(dt)
  |> should.equal(order.Eq)
}
