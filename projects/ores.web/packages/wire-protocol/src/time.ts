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
 * The one owner of the wire timestamp.
 *
 * The server owns the definition. `to_iso8601_utc` in
 * `ores.platform/time/datetime.cpp` writes an instant as
 * `YYYY-MM-DD HH:MM:SS.ffffffZ`, and `parse_utc_timestamp` there reads
 * `YYYY-MM-DD HH:MM:SS[.ffffff]Z`: a space separator, an optional fraction of
 * one to six digits, and the UTC `Z` marker. The shape is stated once in
 * `doc/knowledge/architecture/time_architecture.org`.
 *
 * This module mirrors that rule field for field rather than defining a second
 * one. Where the two disagree, the server is right and this module is the bug;
 * a fixture that only round-trips this module's own output would prove nothing,
 * which is why the golden test feeds it bytes captured from the running fleet.
 *
 * No other module may parse, format, brand, or convert a wire timestamp. An
 * absent instant is represented by `null`, never by an empty string that
 * happens to satisfy the type.
 */

import { z } from 'zod';

declare const timestampBrand: unique symbol;

/** An instant formatted for the wire as `YYYY-MM-DD HH:MM:SS[.ffffff]Z`. */
export type WireTimestamp = string & { readonly [timestampBrand]: 'WireTimestamp' };

/**
 * Exactly what the server's reader consumes and its writer produces.
 *
 * The fraction is optional because `to_iso8601_utc` always states six digits
 * while `parse_utc_timestamp` also accepts one to five (and none, which
 * PostgreSQL states when the microsecond is zero). Nothing outside that range
 * is a wire timestamp, and the pattern is never widened to admit one.
 */
const WIRE_TIMESTAMP_PATTERN =
    /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2})(?:\.(\d{1,6}))?Z$/;

/**
 * Validates and brands a wire timestamp.
 *
 * The calendar fields are checked against a real date, so `2026-02-30` is
 * rejected instead of being silently normalised to March.
 *
 * @throws {TypeError} when the input is not a well-formed wire timestamp.
 */
export function wireTimestamp(value: string): WireTimestamp {
    const match = WIRE_TIMESTAMP_PATTERN.exec(value);
    if (match === null) {
        throw new TypeError(`Not a wire timestamp: ${JSON.stringify(value)}`);
    }
    const [, year, month, day, hour, minute, second] = match;
    const [y, m, d, hh, mm, ss] = [year, month, day, hour, minute, second].map(Number) as [
        number,
        number,
        number,
        number,
        number,
        number,
    ];
    if (m < 1 || m > 12 || d < 1 || d > 31 || hh > 23 || mm > 59 || ss > 59) {
        throw new TypeError(`Impossible calendar value: ${JSON.stringify(value)}`);
    }
    const calendar = new Date(0);
    calendar.setUTCFullYear(y, m - 1, d);
    calendar.setUTCHours(hh, mm, ss, 0);
    if (
        calendar.getUTCFullYear() !== y ||
        calendar.getUTCMonth() !== m - 1 ||
        calendar.getUTCDate() !== d
    ) {
        throw new TypeError(`Impossible calendar value: ${JSON.stringify(value)}`);
    }
    return value as WireTimestamp;
}

/** Narrows an arbitrary value to {@link WireTimestamp} without throwing. */
export function isWireTimestamp(value: unknown): value is WireTimestamp {
    return typeof value === 'string' && WIRE_TIMESTAMP_PATTERN.test(value) && isValid(value);
}

/** Whether a structurally matching value is also a real calendar date. */
function isValid(value: string): boolean {
    try {
        wireTimestamp(value);
        return true;
    } catch {
        return false;
    }
}

/**
 * Converts a `Date` to the wire spelling the server reads.
 *
 * Six digits of microseconds are written, exactly as `to_iso8601_utc` writes
 * them. A `Date` holds milliseconds, so the last three digits are zero; the
 * shape is the same one the server emits, which is what lets a client request
 * and a server reply be compared byte for byte.
 */
export function toWireTimestamp(instant: Date): WireTimestamp {
    const iso = instant.toISOString();
    return wireTimestamp(`${iso.slice(0, 10)} ${iso.slice(11, 23)}000Z`);
}

/**
 * Converts a wire timestamp to a `Date`.
 *
 * A `Date` keeps milliseconds, so `652076` microseconds become `652`
 * milliseconds. The instant is built in UTC from its parts, never through the
 * host timezone, so the same bytes give the same `Date` everywhere.
 */
export function fromWireTimestamp(value: WireTimestamp): Date {
    const match = WIRE_TIMESTAMP_PATTERN.exec(value);
    if (match === null) {
        throw new TypeError(`Not a wire timestamp: ${JSON.stringify(value)}`);
    }
    const [, year, month, day, hour, minute, second, fraction] = match;
    const milliseconds = Number((fraction ?? '').padEnd(3, '0').slice(0, 3));
    const instant = new Date(0);
    instant.setUTCFullYear(Number(year), Number(month) - 1, Number(day));
    instant.setUTCHours(Number(hour), Number(minute), Number(second), milliseconds);
    return instant;
}

/** Parses the wire shape and brands it, for use in a wire-protocol schema. */
export const wireTimestampSchema = z.string().transform((value, ctx) => {
    try {
        return wireTimestamp(value);
    } catch (cause) {
        ctx.addIssue({ code: 'custom', message: 'Not a wire timestamp', cause });
        return z.NEVER;
    }
});

/**
 * An instant the server may state as absent.
 *
 * The server writes `null` for a nullable column and an empty string for a
 * field its protocol models as `std::string` with no value, such as a session
 * that has not ended. Both mean "no instant", and both become `null` here so a
 * consumer must handle the absence instead of comparing against `''`.
 */
export const wireTimestampOrAbsentSchema = z
    .union([wireTimestampSchema, z.literal(''), z.null()])
    .transform((value): WireTimestamp | null => (value === '' || value === null ? null : value));

/** An instant the server states as `null`, with no empty-string spelling. */
export const nullableWireTimestampSchema = wireTimestampSchema.nullable();
