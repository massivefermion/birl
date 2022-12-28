export function now() {
  return Date.now() * 1000
}

export function monotonic_now() {
  return Math.floor(globalThis.performance.now() * 1000)
}

export function to_parts(timestamp) {
  const date = new Date(timestamp / 1000)
  return [
    [date.getFullYear(), date.getUTCMonth() + 1, date.getUTCDate()],
    [date.getUTCHours(), date.getUTCMinutes(), date.getUTCSeconds()],
  ]
}

export function to_iso(timestamp) {
  const date = new Date(timestamp / 1000)
  return date.toISOString()
}

export function from_iso(iso_date) {
  const date = new Date(iso_date)
  return date.getTime() * 1000
}

export function get_weekday(timestamp) {
  const date = new Date(timestamp / 1000)
  return date.getDay()
}
