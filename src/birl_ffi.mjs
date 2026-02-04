import { Some } from '../gleam_stdlib/gleam/option.mjs';

// Only export functions that gleam_time doesn't provide

export function local_timezone() {
  return new Some(Intl.DateTimeFormat().resolvedOptions().timeZone);
}

export function monotonic_now() {
  return Math.floor(globalThis.performance.now() * 1000);
}

export function weekday(timestamp, offset) {
  const date = new Date((timestamp + offset) / 1000);
  return date.getUTCDay();
}
