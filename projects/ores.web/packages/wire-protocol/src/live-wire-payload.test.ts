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
 * The web parses bytes captured from the live server, byte for byte.
 *
 * The fixtures are raw replies written by `scripts/capture-wire-fixtures.ts`
 * against a running fleet, not values this repository made up. A change to the
 * server's serializer changes the captured bytes; recapturing them makes the
 * assertions below fail until the web parser is shown to agree with the new
 * bytes. A test that only round-trips the web's own output would prove only
 * that the web agrees with itself, which is the drift this story exists to
 * stop.
 */

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { accountPageSchema } from './operations.js';
import { isWireTimestamp } from './time.js';

function fixture(name: string): Record<string, unknown> {
    return JSON.parse(readFileSync(resolve(import.meta.dirname, 'fixtures', name), 'utf8')) as Record<
        string,
        unknown
    >;
}

/** Every value the reply holds under a timestamp-shaped key, as it arrived. */
function timesIn(value: unknown): string[] {
    const found: string[] = [];
    if (Array.isArray(value)) {
        for (const item of value) found.push(...timesIn(item));
    } else if (value !== null && typeof value === 'object') {
        for (const [key, item] of Object.entries(value)) {
            if (/(_at|_time)$/.test(key) && typeof item === 'string') {
                found.push(item);
            } else {
                found.push(...timesIn(item));
            }
        }
    }
    return found;
}

describe('a live accounts reply', () => {
    const raw = fixture('live-accounts-page.json') as {
        accounts: {
            recorded_at: string;
            [key: string]: unknown;
        }[];
    };

    it('carries the microsecond in the project wire shape, byte for byte', () => {
        expect(raw.accounts.length).toBeGreaterThan(0);
        expect(raw.accounts[0]?.recorded_at).toBe('2026-10-17 13:00:10.000000Z');
        expect(raw.accounts[1]?.recorded_at).toBe('2026-10-17 13:00:09.000000Z');
    });

    it('parses through the account page schema', () => {
        const page = accountPageSchema.parse(raw);
        expect(page.accounts.length).toBe(raw.accounts.length);
        expect(page.accounts[0]?.recordedAt).toBe(raw.accounts[0]?.recorded_at);
    });

    it('states every timestamp in the wire shape, not only the validated one', () => {
        for (const time of timesIn(raw)) {
            expect(isWireTimestamp(time), time).toBe(true);
        }
    });
});

describe('the shape a raw database rendering takes', () => {
    it('is not a wire timestamp, so the web refuses it instead of reading it as UTC', () => {
        expect(isWireTimestamp('2026-10-17 15:27:23.511414+01')).toBe(false);
        expect(isWireTimestamp('2026-10-17 14:39:57.246326+00')).toBe(false);
    });
});
