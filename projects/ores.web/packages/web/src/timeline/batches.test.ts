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
import type { TimelineEvent } from '@ores/wire-protocol/browser';
import { BATCH_GAP_MS, BATCH_MINIMUM, batchesOf } from './batches.js';

function event(over: Partial<TimelineEvent>): TimelineEvent {
    return {
        entityType: 'ores.refdata.party_currency',
        entityId: 'p/GBP',
        kind: 'raised',
        at: '2026-10-12 02:07:59Z',
        actor: 'ores_refdata_service',
        version: 1,
        reasonCode: 'system.external_data_import',
        commentary: 'Imported from DQ dataset: Party Currencies',
        fields: [],
        ...over,
    };
}

/** n entries of one act, one second apart, newest first, as the stream orders them. */
function act(n: number, over: Partial<TimelineEvent> = {}): TimelineEvent[] {
    return Array.from({ length: n }, (_, i) =>
        event({
            entityId: `p/C${String(i)}`,
            at: `2026-10-12 02:07:${String(59 - i).padStart(2, '0')}Z`,
            ...over,
        }),
    );
}

describe('entries that are one act', () => {
    it('are drawn as one row that holds them all, in the order they arrived', () => {
        const rows = batchesOf(act(5));

        expect(rows).toHaveLength(1);
        expect(rows[0]?.kind).toBe('batch');
        expect(rows[0]?.kind === 'batch' && rows[0].events.map((e) => e.entityId)).toEqual([
            'p/C0',
            'p/C1',
            'p/C2',
            'p/C3',
            'p/C4',
        ]);
    });

    it(`stay as they are below ${String(BATCH_MINIMUM)} entries`, () => {
        expect(batchesOf(act(BATCH_MINIMUM - 1)).map((r) => r.kind)).toEqual(['entry', 'entry']);
    });

    it('do not batch a change, which carries a difference to read', () => {
        expect(batchesOf(act(6, { kind: 'changed' })).every((r) => r.kind === 'entry')).toBe(true);
    });

    it('batch a closing as they batch an opening', () => {
        expect(batchesOf(act(4, { kind: 'closed' })).map((r) => r.kind)).toEqual(['batch']);
    });

    it('do not batch across authors, kinds of record, reasons or comments', () => {
        const mixed = [
            ...act(2),
            ...act(2, { actor: 'priya' }),
            ...act(2, { entityType: 'ores.refdata.party_country' }),
            ...act(2, { reasonCode: 'common.rectification' }),
            ...act(2, { commentary: 'Imported from DQ dataset: Party Countries' }),
        ];

        expect(batchesOf(mixed).every((r) => r.kind === 'entry')).toBe(true);
    });

    it('split when there is a pause longer than the gap between two entries', () => {
        const before = act(3).map((e) => ({ ...e, at: e.at.replace('02:07', '02:00') }));
        const after = act(3);

        expect(batchesOf([...after, ...before]).map((r) => r.kind)).toEqual(['batch', 'batch']);
        expect(BATCH_GAP_MS).toBeGreaterThan(1000);
    });

    it('keep the entries of other acts where they were, around the batch', () => {
        const edit = event({
            kind: 'changed',
            actor: 'priya',
            entityType: 'ores.refdata.party',
            entityId: 'p',
            at: '2026-10-12 02:09:00Z',
        });
        const rows = batchesOf([edit, ...act(4)]);

        expect(rows.map((r) => r.kind)).toEqual(['entry', 'batch']);
    });

    it('leave an entry with a time it cannot read as an entry, and batch the rest', () => {
        const rows = batchesOf(act(4).map((e, i) => (i === 0 ? { ...e, at: 'never' } : e)));

        expect(rows.map((r) => r.kind)).toEqual(['entry', 'batch']);
        expect(rows[1]?.kind === 'batch' && rows[1].events).toHaveLength(3);
    });
});
