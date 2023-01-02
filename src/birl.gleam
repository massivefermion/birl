import gleam/io
import birl/time

// import birl/duration

pub fn main() {
  // duration.new([#(8, duration.Hour), #(30, duration.Minute)])
  // |> io.debug
  // |> duration.accurate_decompose
  // |> io.debug
  time.generate_offset(-30_600_000_000)
  |> io.debug
}
