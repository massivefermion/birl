export function now() {
  return Date.now()
}

export function to_parts(timestamp) {
  const date = new Date(timestamp)
  return [
    [date.getFullYear(), date.getUTCMonth() + 1, date.getUTCDate()],
    [date.getUTCHours(), date.getUTCMinutes(), date.getUTCSeconds()],
  ]
}

export function to_iso(timestamp) {
  const date = new Date(timestamp)
  return date.toISOString()
}

export function from_iso(iso_date) {
  const date = new Date(iso_date)
  return date.getTime()
}
