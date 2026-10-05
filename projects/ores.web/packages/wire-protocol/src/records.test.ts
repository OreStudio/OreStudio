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
    REFDATA_RECORDS,
    listRecords,
    recordResource,
    removeRecord,
    resourceName,
    saveRecord,
} from './records.js';

/** A caller that answers from canned replies and records what it was asked. */
function fakeCaller(replies: Readonly<Record<string, unknown>>): {
    readonly caller: AuthenticatedCaller;
    readonly calls: { subject: string; body: unknown }[];
} {
    const calls: { subject: string; body: unknown }[] = [];
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

function resource(key: string): NonNullable<ReturnType<typeof recordResource>> {
    const found = recordResource(key);
    if (found === undefined) {
        throw new Error(`No resource ${key}`);
    }
    return found;
}

describe('the record registry', () => {
    it('names each resource once, each with refdata subjects', () => {
        const keys = REFDATA_RECORDS.map((entry) => entry.key);
        expect(new Set(keys).size).toBe(keys.length);
        for (const entry of REFDATA_RECORDS) {
            expect(entry.subjects.list).toMatch(/^refdata\.v1\./);
            expect(entry.subjects.put).toMatch(/^refdata\.v1\./);
        }
    });

    it('reads a junction by its parent, and only a junction', () => {
        const junctions = REFDATA_RECORDS.filter((entry) => entry.listBy !== undefined).map(
            (entry) => entry.key,
        );
        expect(junctions.sort()).toEqual([
            'currency-calendars',
            'currency-countries',
            'currency-memberships',
            'pair-calendars',
        ]);
        expect(
            REFDATA_RECORDS.filter((entry) => !entry.versioned)
                .map((entry) => entry.key)
                .sort(),
        ).toEqual(junctions.sort());
    });

    it('names the resource its permissions use', () => {
        expect(resourceName(resource('currency-pairs'))).toBe('currency_pairs');
    });
});

describe('listRecords', () => {
    it('reads the rows from the field the resource names, with the point in time it requires', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currencies.list': {
                result: ok,
                currencies: [{ iso_code: 'EUR', version: 3 }],
            },
        });
        const rows = await listRecords(caller, resource('currencies'));
        expect(rows).toEqual([{ iso_code: 'EUR', version: 3 }]);
        expect(calls[0]?.body).toMatchObject({ as_of: null });
    });

    it('states no point in time to a resource that has none', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_pairs.list': { result: ok, pairs: [] },
        });
        await listRecords(caller, resource('currency-pairs'));
        expect(calls[0]?.body).not.toHaveProperty('as_of');
    });

    it('reads one parent of a junction through its list-by subject', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_countries.list_by_currency_iso_code': {
                result: ok,
                currency_countries: [
                    { currency_iso_code: 'EUR', country_alpha2_code: 'DE', version: 1 },
                ],
            },
        });
        const rows = await listRecords(caller, resource('currency-countries'), 'EUR');
        expect(rows).toHaveLength(1);
        expect(calls[0]?.body).toMatchObject({ currency_iso_code: 'EUR', scope: 'direct' });
    });

    it('fails when the server refuses the read', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.currency_groups.list': {
                result: { outcome: 'denied', code: 'denied', message: 'No.' },
            },
        });
        await expect(listRecords(caller, resource('currency-groups'))).rejects.toThrow('No.');
    });
});

describe('the record writes', () => {
    it('creates with must_not_exist and corrects against the version read', async () => {
        const { caller, calls } = fakeCaller({ 'refdata.v1.currency_groups.put': { result: ok } });
        const intent = { reasonCode: 'common.rectification', commentary: '' };
        await saveRecord(caller, resource('currency-groups'), { code: 'G11' }, null, intent);
        await saveRecord(caller, resource('currency-groups'), { code: 'G11' }, 4, intent);
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_not_exist' } },
        });
        expect(calls[1]?.body).toMatchObject({
            change: {
                write: { code: 'G11' },
                precondition: { kind: 'must_match_version', version: 4 },
            },
        });
    });

    it('removes a junction row by both of its keys', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_countries.delete': { result: ok },
        });
        const outcome = await removeRecord(
            caller,
            resource('currency-countries'),
            { currency_iso_code: 'EUR', country_alpha2_code: 'DE' },
            { reasonCode: 'common.rectification', commentary: '' },
        );
        expect(outcome).toEqual({ done: true });
        expect(calls[0]?.body).toMatchObject({
            removal: { key: { currency_iso_code: 'EUR', country_alpha2_code: 'DE' } },
        });
    });
});
