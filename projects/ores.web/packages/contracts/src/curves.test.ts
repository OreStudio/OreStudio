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

import { describe, expect, it } from 'vitest';
import { curveInstantSchema, curveSnapshotSchema } from './curves.js';

const snapshot = (overrides: Record<string, unknown> = {}) => ({
    as_of: '2026-10-10 09:00:00Z',
    coordinate_fields: ['tenor'],
    nodes: [],
    values: [],
    recorded_at: ['2026-10-10 09:00:00.652076Z'],
    provenance: [],
    oldest_age_seconds: 0,
    spread_seconds: 0,
    warning: false,
    ...overrides,
});

describe('a curve instant', () => {
    it('reads the wire shape the server writes', () => {
        const parsed = curveInstantSchema.parse({
            as_of: '2026-10-10 09:00:00.652076Z',
            values: ['1.25'],
        });
        expect(parsed.as_of).toBe('2026-10-10 09:00:00.652076Z');
    });

    it('refuses the T separator and any other spelling', () => {
        expect(
            curveInstantSchema.safeParse({ as_of: '2026-10-10T09:00:00Z', values: [] }).success,
        ).toBe(false);
        expect(
            curveInstantSchema.safeParse({ as_of: '2026-10-10 09:00:00+00', values: [] }).success,
        ).toBe(false);
        expect(curveInstantSchema.safeParse({ as_of: '', values: [] }).success).toBe(false);
    });

    it('refuses an impossible calendar day rather than shifting it', () => {
        expect(
            curveInstantSchema.safeParse({ as_of: '2026-02-30 09:00:00Z', values: [] }).success,
        ).toBe(false);
    });
});

describe('a curve snapshot', () => {
    it('reads every instant the snapshot carries, the list included', () => {
        const parsed = curveSnapshotSchema.parse(snapshot());
        expect(parsed.as_of).toBe('2026-10-10 09:00:00Z');
        expect(parsed.recorded_at).toEqual(['2026-10-10 09:00:00.652076Z']);
    });

    it('refuses a record time that is not the wire shape', () => {
        expect(
            curveSnapshotSchema.safeParse(snapshot({ recorded_at: ['2026-10-10'] })).success,
        ).toBe(false);
    });
});
