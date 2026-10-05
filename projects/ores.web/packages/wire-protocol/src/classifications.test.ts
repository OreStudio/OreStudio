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
import type { AuthenticatedCaller } from './account-operations.js';
import {
    CLASSIFICATION_LISTS,
    classificationCatalogue,
    classificationList,
    countClassificationRows,
    readClassificationLabels,
    readLabelCatalogue,
    setClassificationLabel,
    listClassificationRows,
    readEntityHistory,
    removeClassificationRow,
    saveClassificationRow,
    saveClassificationRows,
} from './classifications.js';

interface Call {
    readonly subject: string;
    readonly body: unknown;
}

/** A caller that answers from canned replies and records what it was asked. */
function fakeCaller(replies: Readonly<Record<string, unknown>>): {
    readonly caller: AuthenticatedCaller;
    readonly calls: Call[];
} {
    const calls: Call[] = [];
    const caller = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return schema.parse(replies[subject]);
        },
    } as unknown as AuthenticatedCaller;
    return { caller, calls };
}

const ok = { outcome: 'ok', code: '', message: '' };

function list(key: string): NonNullable<ReturnType<typeof classificationList>> {
    const found = classificationList(key);
    if (found === undefined) {
        throw new Error(`No list ${key}`);
    }
    return found;
}

describe('the classification catalogue', () => {
    it('names 28 lists, each once', () => {
        const keys = CLASSIFICATION_LISTS.map((entry) => entry.key);
        expect(keys).toHaveLength(28);
        expect(new Set(keys).size).toBe(28);
    });

    it('marks exactly the ORE spellings read-only', () => {
        const readOnly = classificationCatalogue()
            .filter((entry) => !entry.editable)
            .map((entry) => entry.key)
            .sort();
        expect(readOnly).toEqual([
            'business-day-convention-type',
            'calendar-name',
            'day-counter',
            'floating-index-type',
            'leg-type',
        ]);
    });

    it('gives every list a refdata subject for each verb', () => {
        for (const entry of CLASSIFICATION_LISTS) {
            expect(entry.entityType).toMatch(/^ores\.refdata\./);
            for (const subject of Object.values(entry.subjects)) {
                expect(subject).toMatch(/^refdata\.v1\./);
            }
        }
    });

    it('answers nothing for a list it does not name', () => {
        expect(classificationList('currencies')).toBeUndefined();
    });
});

describe('listClassificationRows', () => {
    it('reads the rows from the field the list names', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_pair_classifications.list': {
                result: ok,
                classifications: [
                    {
                        version: 2,
                        code: 'major',
                        name: 'Major',
                        description: 'Most traded',
                        display_order: 10,
                    },
                ],
            },
        });
        const rows = await listClassificationRows(caller, list('currency-pair-classification'));
        expect(rows).toEqual([
            {
                code: 'major',
                name: 'Major',
                description: 'Most traded',
                displayOrder: 10,
                version: 2,
                modifiedBy: '',
                recordedAt: '',
                reasonCode: '',
                commentary: '',
                labelCode: null,
            },
        ]);
        expect(calls[0]?.body).toMatchObject({ order: { field: '' } });
    });

    it('orders a plain list by its key, which has no display order', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.day_counters.list': {
                result: ok,
                day_counters: [{ version: 1, code: 'A360', description: 'Actual/360' }],
            },
        });
        const rows = await listClassificationRows(caller, list('day-counter'));
        expect(rows[0]).toMatchObject({ code: 'A360', name: '', displayOrder: null });
        expect(calls[0]?.body).toMatchObject({ order: { field: '' } });
    });

    it('states no point in time to a list whose request carries one', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.book_statuses.list': { result: ok, statuses: [] },
            'refdata.v1.rounding_types.list': { result: ok, types: [] },
        });
        await listClassificationRows(caller, list('book-status'));
        await listClassificationRows(caller, list('rounding-type'));
        expect(calls[0]?.body).toMatchObject({ as_of: null });
        expect(calls[1]?.body).not.toHaveProperty('as_of');
    });

    it('answers the rows in display order, whatever order they were read in', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.rounding_types.list': {
                result: ok,
                types: [
                    { version: 1, code: 'B', name: 'B', display_order: 20 },
                    { version: 1, code: 'A', name: 'A', display_order: 10 },
                ],
            },
        });
        const rows = await listClassificationRows(caller, list('rounding-type'));
        expect(rows.map((row) => row.code)).toEqual(['A', 'B']);
    });

    it('fails when the server refuses the read', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.rounding_types.list': {
                result: { outcome: 'denied', code: 'denied', message: 'No.' },
            },
        });
        await expect(listClassificationRows(caller, list('rounding-type'))).rejects.toThrow('No.');
    });
});

describe('the writes', () => {
    it('creates a new row with must_not_exist and the reason given', async () => {
        const { caller, calls } = fakeCaller({ 'refdata.v1.rounding_types.put': { result: ok } });
        const outcome = await saveClassificationRow(
            caller,
            list('rounding-type'),
            {
                code: 'Up',
                name: 'Up',
                description: 'Away from zero',
                displayOrder: 20,
                version: null,
            },
            { reasonCode: 'common.rectification', commentary: 'Added' },
        );
        expect(outcome).toEqual({ done: true });
        expect(calls[0]?.body).toEqual({
            change: {
                write: { code: 'Up', name: 'Up', description: 'Away from zero', display_order: 20 },
                precondition: { kind: 'must_not_exist', version: null },
            },
            intent: { reason_code: 'common.rectification', commentary: 'Added' },
        });
    });

    it('writes only the columns an ordered list carries, against the version read', async () => {
        const { caller, calls } = fakeCaller({ 'refdata.v1.tenor_anchors.put': { result: ok } });
        await saveClassificationRow(
            caller,
            list('tenor-anchor'),
            { code: 'SPOT', name: 'ignored', description: 'Spot', displayOrder: 5, version: 3 },
            { reasonCode: 'common.rectification', commentary: '' },
        );
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { code: 'SPOT', description: 'Spot', display_order: 5 },
                precondition: { kind: 'must_match_version', version: 3 },
            },
        });
    });

    it('answers the outcome of a refused write', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.rounding_types.put': {
                result: { outcome: 'conflict', code: 'version_conflict', message: 'Stale.' },
            },
        });
        const outcome = await saveClassificationRow(
            caller,
            list('rounding-type'),
            { code: 'Up', name: 'Up', description: '', displayOrder: 20, version: 1 },
            { reasonCode: 'common.rectification', commentary: '' },
        );
        expect(outcome).toEqual({ done: false, outcome: 'conflict', message: 'Stale.' });
    });

    it('writes several rows in one call', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.rounding_types.put_many': { result: ok },
        });
        await saveClassificationRows(
            caller,
            list('rounding-type'),
            [
                { code: 'Up', name: 'Up', description: '', displayOrder: 10, version: 1 },
                { code: 'Down', name: 'Down', description: '', displayOrder: 20, version: 4 },
            ],
            { reasonCode: 'common.rectification', commentary: 'Reordered' },
        );
        expect((calls[0]?.body as { changes: unknown[] }).changes).toHaveLength(2);
    });

    it('removes a row by its code', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.rounding_types.delete': { result: ok },
        });
        await removeClassificationRow(caller, list('rounding-type'), 'Up', {
            reasonCode: 'common.rectification',
            commentary: 'Unused',
        });
        expect(calls[0]?.body).toEqual({
            removal: { key: { code: 'Up' }, precondition: { kind: 'any', version: null } },
            intent: { reason_code: 'common.rectification', commentary: 'Unused' },
        });
    });
});

describe('readEntityHistory', () => {
    it('asks the owning component and answers newest first with the changes', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.history.get': {
                success: true,
                versions: [
                    {
                        version: 1,
                        modified_by: 'system',
                        recorded_at: 't1',
                        fields: [],
                        changes: { entries: [] },
                    },
                    {
                        version: 2,
                        modified_by: 'alice',
                        recorded_at: 't2',
                        fields: [{ name: 'Name', value: 'Up' }],
                        changes: {
                            entries: [{ field_name: 'Name', old_value: 'UP', new_value: 'Up' }],
                        },
                    },
                ],
            },
        });
        const versions = await readEntityHistory(caller, 'ores.refdata.rounding_type', 'Up');
        expect(calls[0]?.body).toEqual({
            entity_type: 'ores.refdata.rounding_type',
            entity_id: 'Up',
        });
        expect(versions.map((version) => version.version)).toEqual([2, 1]);
        expect(versions[0]?.changes).toEqual([{ field: 'Name', before: 'UP', after: 'Up' }]);
    });

    it('fails when the server cannot answer', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.history.get': { success: false, message: 'Unknown entity type.' },
        });
        await expect(readEntityHistory(caller, 'ores.refdata.rounding_type', 'Up')).rejects.toThrow(
            'Unknown entity type.',
        );
    });
});

describe('the catalogue permissions', () => {
    it('names the permissions the server checks for each list', () => {
        const entry = classificationCatalogue().find((list) => list.key === 'rounding-type');
        expect(entry).toMatchObject({
            writePermission: 'refdata::rounding_types:write',
            deletePermission: 'refdata::rounding_types:delete',
        });
    });
});

describe('countClassificationRows', () => {
    it('reads the total without the rows', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.day_counters.list': {
                result: ok,
                day_counters: [{ code: 'A360' }],
                total: 71,
            },
        });
        expect(await countClassificationRows(caller, list('day-counter'))).toBe(71);
        expect(calls[0]?.body).toMatchObject({ limit: 1 });
    });
});

describe('the labels', () => {
    it("reads a list's labels from its entity name as the code domain", async () => {
        const { caller, calls } = fakeCaller({
            'dq.v1.badge_mappings.list_by_code_domain_code': {
                result: ok,
                badge_mappings: [
                    {
                        code_domain_code: 'rounding_type',
                        entity_code: 'Up',
                        badge_code: 'rounding_type_up',
                    },
                ],
            },
        });
        expect(await readClassificationLabels(caller, list('rounding-type'))).toEqual({
            Up: 'rounding_type_up',
        });
        expect(calls[0]?.body).toMatchObject({ code_domain_code: 'rounding_type' });
    });

    it('answers every label and the labels each domain uses, once each', async () => {
        const { caller } = fakeCaller({
            'dq.v1.badge_definitions.list': {
                definitions: [
                    {
                        code: 'rounding_type_up',
                        name: 'Up',
                        background_colour: '#8b5cf6',
                        text_colour: '#fff',
                    },
                    {
                        code: 'active',
                        name: 'Active',
                        background_colour: '#22c55e',
                        text_colour: '#fff',
                    },
                ],
            },
            'dq.v1.badge_mappings.list': {
                result: ok,
                badge_mappings: [
                    {
                        code_domain_code: 'rounding_type',
                        entity_code: 'Up',
                        badge_code: 'rounding_type_up',
                    },
                    {
                        code_domain_code: 'rounding_type',
                        entity_code: 'Up',
                        badge_code: 'rounding_type_up',
                    },
                    {
                        code_domain_code: 'party_status',
                        entity_code: 'Active',
                        badge_code: 'active',
                    },
                ],
            },
        });
        const catalogue = await readLabelCatalogue(caller);
        expect(catalogue.labels.map((label) => label.code)).toEqual(['rounding_type_up', 'active']);
        expect(catalogue.domains).toEqual({
            rounding_type: ['rounding_type_up'],
            party_status: ['active'],
        });
    });

    it('gives a row a label by writing its mapping', async () => {
        const { caller, calls } = fakeCaller({ 'dq.v1.badge_mappings.put': { result: ok } });
        const outcome = await setClassificationLabel(
            caller,
            list('rounding-type'),
            'Up',
            'rounding_type_up',
            {
                reasonCode: 'common.non_material_update',
                commentary: '',
            },
        );
        expect(outcome).toEqual({ done: true });
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: {
                    code_domain_code: 'rounding_type',
                    entity_code: 'Up',
                    badge_code: 'rounding_type_up',
                },
            },
        });
    });

    it('takes a label away by removing its mapping', async () => {
        const { caller, calls } = fakeCaller({ 'dq.v1.badge_mappings.delete': { result: ok } });
        await setClassificationLabel(caller, list('rounding-type'), 'Up', null, {
            reasonCode: 'common.rectification',
            commentary: '',
        });
        expect(calls[0]?.body).toMatchObject({
            removal: { key: { code_domain_code: 'rounding_type', entity_code: 'Up' } },
        });
    });
});
