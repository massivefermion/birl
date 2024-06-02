import { Some } from '../gleam_stdlib/gleam/option.mjs';

export function now() {
  return Date.now() * 1000;
}

export function local_offset() {
  const date = new Date();
  return -date.getTimezoneOffset();
}

export function local_timezone() {
  return new Some(Intl.DateTimeFormat().resolvedOptions().timeZone);
}

export function monotonic_now() {
  return Math.floor(globalThis.performance.now() * 1000);
}

export function to_parts(timestamp, offset) {
  const date = new Date((timestamp + offset) / 1000);
  return [
    [date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate()],
    [
      date.getUTCHours(),
      date.getUTCMinutes(),
      date.getUTCSeconds(),
      date.getUTCMilliseconds(),
    ],
  ];
}

export function from_parts(parts, offset) {
  const date = new Date(
    Date.UTC(
      parts[0][0],
      parts[0][1] - 1,
      parts[0][2],
      parts[1][0],
      parts[1][1],
      parts[1][2],
      parts[1][3]
    )
  );

  return date.getTime() * 1000 - offset;
}

export function weekday(timestamp, offset) {
  const date = new Date((timestamp + offset) / 1000);
  return date.getUTCDay();
}
