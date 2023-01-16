export function now() {
  return Date.now() * 1000
}

export function local_offset() {
  const date = new Date()
  return -1 * date.getTimezoneOffset()
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

export function from_parts(parts, offset) {
  const date = new Date(
    parts[0][0],
    parts[0][1] - 1,
    parts[0][2],
    parts[1][0],
    parts[1][1],
    parts[1][2]
  )
  return date.getTime() * 1000 + offset
}

export function to_iso(timestamp) {
  const date = new Date(timestamp / 1000)
  return date.toISOString()
}

export function from_iso(iso_date) {
  const date = new Date(iso_date)
  return date.getTime() * 1000
}

export function weekday(timestamp) {
  const date = new Date(timestamp / 1000)
  return date.getDay()
}
