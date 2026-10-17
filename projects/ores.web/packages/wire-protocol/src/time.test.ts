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

import { execFileSync } from 'node:child_process';
import { describe, expect, it } from 'vitest';
import {
    fromWireTimestamp,
    isWireTimestamp,
    nullableWireTimestampSchema,
    toWireTimestamp,
    wireTimestamp,
    wireTimestampOrAbsentSchema,
    wireTimestampSchema,
} from './time.js';

describe('the wire timestamp the server writes', () => {
    it('accepts six digits of microseconds, which to_iso8601_utc always writes', () => {
        expect(isWireTimestamp('2026-10-17 13:00:09.000000Z')).toBe(true);
        expect(isWireTimestamp('2026-04-08 10:30:45.652076Z')).toBe(true);
    });

    it('accepts one digit and anything up to six, as parse_utc_timestamp reads them', () => {
        expect(isWireTimestamp('2026-04-08 10:30:45.5Z')).toBe(true);
        expect(isWireTimestamp('2026-04-08 10:30:45.65207Z')).toBe(true);
    });

    it('accepts no fraction, which PostgreSQL states when the microsecond is zero', () => {
        expect(isWireTimestamp('2026-09-18 15:50:44Z')).toBe(true);
    });

    it('rejects a fraction of more than six digits or an empty one', () => {
        expect(isWireTimestamp('2026-04-08 10:30:45.6520761Z')).toBe(false);
        expect(isWireTimestamp('2026-04-08 10:30:45.Z')).toBe(false);
    });

    it('rejects the ISO spelling with a T separator', () => {
        expect(isWireTimestamp('2026-09-18T15:50:44Z')).toBe(false);
    });

    it('rejects a value with no UTC marker', () => {
        expect(isWireTimestamp('2026-09-18 15:50:44+00')).toBe(false);
        expect(isWireTimestamp('2026-09-18 15:50:44')).toBe(false);
    });

    it('rejects an impossible calendar date', () => {
        expect(isWireTimestamp('2026-02-30 00:00:00Z')).toBe(false);
        expect(isWireTimestamp('2025-02-29 00:00:00Z')).toBe(false);
        expect(isWireTimestamp('2024-02-29 00:00:00Z')).toBe(true);
    });

    it('rejects an out-of-range time', () => {
        expect(isWireTimestamp('2026-02-01 24:00:00Z')).toBe(false);
        expect(isWireTimestamp('2026-02-01 23:60:00Z')).toBe(false);
    });

    it('rejects a malformed value', () => {
        expect(isWireTimestamp('not a timestamp')).toBe(false);
        expect(isWireTimestamp('2026-4-8 10:30:45Z')).toBe(false);
        expect(isWireTimestamp('')).toBe(false);
        expect(isWireTimestamp(undefined)).toBe(false);
        expect(isWireTimestamp(null)).toBe(false);
        expect(isWireTimestamp(20261017)).toBe(false);
    });

    it('brands only a well-formed value', () => {
        expect(() => wireTimestamp('2026-04-08 10:30:45.652076Z')).not.toThrow();
        expect(() => wireTimestamp('2026-04-08T10:30:45.652076Z')).toThrow(TypeError);
    });
});

describe('the wire timestamp schema', () => {
    it('parses a value into the brand', () => {
        expect(wireTimestampSchema.parse('2026-04-08 10:30:45.652076Z')).toBe(
            '2026-04-08 10:30:45.652076Z',
        );
    });

    it('refuses a value that is not the wire shape', () => {
        expect(wireTimestampSchema.safeParse('2026-04-08T10:30:45.652076Z').success).toBe(false);
    });

    it('treats an absent instant as null, never as an empty string', () => {
        expect(wireTimestampOrAbsentSchema.parse('')).toBeNull();
        expect(wireTimestampOrAbsentSchema.parse(null)).toBeNull();
        expect(wireTimestampOrAbsentSchema.parse('2026-04-08 10:30:45.652076Z')).toBe(
            '2026-04-08 10:30:45.652076Z',
        );
        expect(nullableWireTimestampSchema.parse(null)).toBeNull();
        expect(nullableWireTimestampSchema.safeParse('').success).toBe(false);
    });
});

describe('converting a wire timestamp and a Date', () => {
    it('writes six digits of microseconds, as the server writes them', () => {
        const instant = new Date(Date.UTC(2026, 8, 18, 15, 50, 44, 512));
        expect(toWireTimestamp(instant)).toBe('2026-09-18 15:50:44.512000Z');
    });

    it('keeps the millisecond a Date can hold when parsing back', () => {
        const value = wireTimestamp('2026-04-08 10:30:45.652076Z');
        expect(fromWireTimestamp(value).toISOString()).toBe('2026-04-08T10:30:45.652Z');
    });

    it('round-trips a Date through the wire spelling', () => {
        const instant = new Date(Date.UTC(2026, 9, 17, 13, 0, 9, 0));
        expect(fromWireTimestamp(toWireTimestamp(instant)).toISOString()).toBe(
            instant.toISOString(),
        );
    });
});

/**
 * Runs one conversion in a fresh process whose TZ is the given zone.
 *
 * A `TZ` changes how the host renders a local time, never how these functions
 * read or write UTC. A child process is the only way to set it before the
 * conversion, because Node reads the zone when the process starts.
 */
function bytesIn(timezone: string): string {
    const script =
        "import('./time.ts').then((m) => process.stdout.write(m.toWireTimestamp(new Date(Date.UTC(2026, 9, 17, 13, 0, 9, 512)))))";
    return execFileSync(process.execPath, ['--import', 'tsx', '-e', script], {
        cwd: import.meta.dirname,
        env: { ...process.env, TZ: timezone },
        encoding: 'utf8',
    });
}

function instantIn(timezone: string): string {
    const script =
        "import('./time.ts').then((m) => process.stdout.write(m.fromWireTimestamp(m.wireTimestamp('2026-10-17 13:00:09.512000Z')).toISOString()))";
    return execFileSync(process.execPath, ['--import', 'tsx', '-e', script], {
        cwd: import.meta.dirname,
        env: { ...process.env, TZ: timezone },
        encoding: 'utf8',
    });
}

describe('timezone independence', () => {
    it('writes identical bytes with the process in UTC and in a non-UTC zone', () => {
        const utc = bytesIn('UTC');
        expect(utc).toBe('2026-10-17 13:00:09.512000Z');
        expect(bytesIn('Europe/London')).toBe(utc);
        expect(bytesIn('America/New_York')).toBe(utc);
    });

    it('reads the same instant with the process in UTC and in a non-UTC zone', () => {
        const utc = instantIn('UTC');
        expect(utc).toBe('2026-10-17T13:00:09.512Z');
        expect(instantIn('Europe/London')).toBe(utc);
        expect(instantIn('America/New_York')).toBe(utc);
    });
});
