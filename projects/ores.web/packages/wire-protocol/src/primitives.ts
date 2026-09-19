/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

/**
 * Primitive types shared across the protocol layer.
 *
 * The server serialises UUIDs as lowercase hyphenated strings and timestamps
 * as `YYYY-MM-DD HH:MM:SSZ` (space separator, UTC, second resolution). See
 * `ores.utility/rfl/reflectors.hpp` and `ores.platform/time/datetime.cpp` in
 * the C++ checkout. Those two spellings are the wire truth; everything above
 * this layer uses the branded aliases below.
 */

declare const uuidBrand: unique symbol;
declare const timestampBrand: unique symbol;

/** A UUID in the canonical lowercase hyphenated form. */
export type Uuid = string & { readonly [uuidBrand]: 'Uuid' };

/** An instant formatted for the wire as `YYYY-MM-DD HH:MM:SSZ`. */
export type WireTimestamp = string & { readonly [timestampBrand]: 'WireTimestamp' };

const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
const WIRE_TIMESTAMP_PATTERN = /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})Z$/;

/**
 * Validates and brands a UUID string.
 *
 * @throws {TypeError} when the input is not a canonical lowercase UUID.
 */
export function uuid(value: string): Uuid {
  if (!UUID_PATTERN.test(value)) {
    throw new TypeError(`Not a canonical UUID: ${JSON.stringify(value)}`);
  }
  return value as Uuid;
}

/** Narrows an arbitrary string to {@link Uuid} without throwing. */
export function isUuid(value: string): value is Uuid {
  return UUID_PATTERN.test(value);
}

/**
 * Validates and brands a wire timestamp.
 *
 * The pattern is checked structurally and the date components are checked
 * against the calendar so `2026-02-30 00:00:00Z` is rejected rather than
 * silently normalised.
 *
 * @throws {TypeError} when the input is not a well-formed wire timestamp.
 */
export function wireTimestamp(value: string): WireTimestamp {
  const match = WIRE_TIMESTAMP_PATTERN.exec(value);
  if (match === null) {
    throw new TypeError(`Not a wire timestamp: ${JSON.stringify(value)}`);
  }
  const [, year, month, day, hour, minute, second] = match;
  const parts = [year, month, day, hour, minute, second].map(Number);
  const [y, m, d, hh, mm, ss] = parts as [number, number, number, number, number, number];
  const roundTrips =
    Date.UTC(y, m - 1, d, hh, mm, ss) === Date.UTC(y, m - 1, d, hh, mm, ss) &&
    new Date(Date.UTC(y, m - 1, d, hh, mm, ss)).getUTCDate() === d &&
    new Date(Date.UTC(y, m - 1, d, hh, mm, ss)).getUTCMonth() === m - 1 &&
    hh <= 23 &&
    mm <= 59 &&
    ss <= 59;
  if (!roundTrips) {
    throw new TypeError(`Impossible calendar value: ${JSON.stringify(value)}`);
  }
  return value as WireTimestamp;
}

/** Narrows an arbitrary string to {@link WireTimestamp} without throwing. */
export function isWireTimestamp(value: string): value is WireTimestamp {
  try {
    wireTimestamp(value);
    return true;
  } catch {
    return false;
  }
}

/**
 * Converts an instant to the wire spelling the server parses.
 *
 * Sub-second precision is dropped because the server's formatter writes whole
 * seconds only.
 */
export function toWireTimestamp(instant: Date): WireTimestamp {
  const iso = instant.toISOString();
  return wireTimestamp(`${iso.slice(0, 10)} ${iso.slice(11, 19)}Z`);
}

/** Converts a wire timestamp to a `Date` for presentation. */
export function fromWireTimestamp(value: WireTimestamp): Date {
  const match = WIRE_TIMESTAMP_PATTERN.exec(value);
  if (match === null) {
    throw new TypeError(`Not a wire timestamp: ${JSON.stringify(value)}`);
  }
  const [, year, month, day, hour, minute, second] = match;
  return new Date(
    Date.UTC(
      Number(year),
      Number(month) - 1,
      Number(day),
      Number(hour),
      Number(minute),
      Number(second),
    ),
  );
}

/** The system tenant, {@code ffffffff-ffff-ffff-ffff-ffffffffffff}. */
export const SYSTEM_TENANT_ID = uuid('ffffffff-ffff-ffff-ffff-ffffffffffff');

/** The Live workspace, {@code aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa}. */
export const LIVE_WORKSPACE_ID = uuid('aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa');
