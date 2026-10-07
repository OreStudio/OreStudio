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
    listRecordPage,
    listRecords,
    readRecord,
    readCalendarYear,
    rebuildCalendar,
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

    it('reads every junction by its parent, and keeps no versions only for a junction', () => {
        const junctions = REFDATA_RECORDS.filter((entry) => !entry.versioned).map(
            (entry) => entry.key,
        );
        expect(junctions.sort()).toEqual([
            'currency-calendars',
            'currency-countries',
            'currency-memberships',
            'pair-calendars',
        ]);
        for (const key of junctions) {
            expect(resource(key).listBy).toBeDefined();
        }
    });

    it('reads the parts of a calendar by the calendar', () => {
        for (const key of ['calendar-rules', 'calendar-exceptions', 'calendar-events']) {
            expect(resource(key)).toMatchObject({
                listBy: 'calendar_code',
                keyFields: ['id'],
                versioned: true,
            });
        }
    });

    it('names the resource its permissions use, for every resource', () => {
        expect(
            Object.fromEntries(REFDATA_RECORDS.map((entry) => [entry.key, resourceName(entry)])),
        ).toEqual({
            currencies: 'currencies',
            countries: 'countries',
            'business-centres': 'business_centres',
            calendars: 'calendars',
            'calendar-rules': 'calendar_rules',
            'calendar-exceptions': 'calendar_exceptions',
            'calendar-events': 'calendar_events',
            'currency-groups': 'currency_groups',
            'currency-countries': 'currency_countries',
            'currency-calendars': 'currency_calendars',
            'currency-memberships': 'currency_currency_groups',
            'currency-pairs': 'currency_pairs',
            'currency-pair-conventions': 'currency_pair_conventions',
            'pair-calendars': 'currency_pair_convention_calendars',
        });
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

    it('reads page after page until a page comes back short', async () => {
        const all = Array.from({ length: 1500 }, (_, index) => ({
            code: `C${String(index)}`,
            version: 1,
        }));
        const offsets: number[] = [];
        const caller = {
            async callAuthenticated(
                _subject: string,
                body: { offset: number; limit: number },
                schema: { parse: (value: unknown) => unknown },
            ): Promise<unknown> {
                offsets.push(body.offset);
                return schema.parse({
                    result: ok,
                    groups: all.slice(body.offset, body.offset + body.limit),
                });
            },
        } as unknown as AuthenticatedCaller;
        expect(await listRecords(caller, resource('currency-groups'))).toHaveLength(1500);
        expect(offsets).toEqual([0, 1000]);
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
            null,
            { reasonCode: 'common.rectification', commentary: '' },
        );
        expect(outcome).toEqual({ done: true });
        expect(calls[0]?.body).toMatchObject({
            removal: {
                key: { currency_iso_code: 'EUR', country_alpha2_code: 'DE' },
                precondition: { kind: 'any' },
            },
        });
    });

    it('removes a versioned row against the version read', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_groups.delete': { result: ok },
        });
        await removeRecord(caller, resource('currency-groups'), { code: 'G11' }, 3, {
            reasonCode: 'common.rectification',
            commentary: '',
        });
        expect(calls[0]?.body).toMatchObject({
            removal: { precondition: { kind: 'must_match_version', version: 3 } },
        });
    });
});

describe('listRecordPage', () => {
    it('asks for one page with the search and the order, and answers the total', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currencies.list': {
                result: ok,
                currencies: [{ iso_code: 'EUR', version: 1 }],
                total: 168,
            },
        });
        const page = await listRecordPage(caller, resource('currencies'), {
            offset: 100,
            limit: 50,
            search: 'eu',
            sort: 'name',
            descending: true,
        });
        expect(page).toEqual({ rows: [{ iso_code: 'EUR', version: 1 }], total: 168 });
        expect(calls[0]?.body).toEqual({
            offset: 100,
            limit: 50,
            order: { field: 'name', descending: true },
            filter: { iso_code_one_of: null, search: 'eu' },
            as_of: null,
        });
    });

    it('sends no filter when there is no search', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_groups.list': { result: ok, groups: [], total: 0 },
        });
        await listRecordPage(caller, resource('currency-groups'), {
            offset: 0,
            limit: 100,
            search: '',
            sort: '',
            descending: false,
        });
        expect(calls[0]?.body).toMatchObject({ filter: null, order: { field: '' } });
    });

    it('declares search and sorting only where the model has them', () => {
        expect(resource('currencies')).toMatchObject({ search: true });
        expect(resource('countries')).toMatchObject({ search: false, sortable: [] });
    });
});

describe('readRecord', () => {
    it("reads one record through the key's one-of filter", async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.currency_pairs.list': {
                result: ok,
                pairs: [{ pair_code: 'EUR/USD', version: 3 }],
                total: 1,
            },
        });
        expect(await readRecord(caller, resource('currency-pairs'), 'EUR/USD')).toEqual({
            pair_code: 'EUR/USD',
            version: 3,
        });
        expect(calls[0]?.body).toMatchObject({
            limit: 1,
            filter: { pair_code_one_of: ['EUR/USD'] },
        });
    });

    it('answers undefined for a key with no record', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.currency_groups.list': { result: ok, groups: [], total: 0 },
        });
        expect(await readRecord(caller, resource('currency-groups'), 'NONE')).toBeUndefined();
    });
});

/** Consecutive days from a start date, as the server materialises them. */
function days(
    from: string,
    count: number,
): { date: string; is_business_day: boolean; source: string }[] {
    const start = Date.parse(`${from}T00:00:00Z`);
    return Array.from({ length: count }, (_, index) => ({
        date: new Date(start + index * 86_400_000).toISOString().slice(0, 10),
        is_business_day: index % 7 < 5,
        source: 'user_defined',
    }));
}

describe('readCalendarYear', () => {
    it('pages through the days until it passes the year, and keeps only that year', async () => {
        const all = days('2024-01-01', 2500);
        const calls: number[] = [];
        const caller = {
            async callAuthenticated(
                _subject: string,
                body: { offset: number; limit: number },
                schema: { parse: (value: unknown) => unknown },
            ): Promise<unknown> {
                calls.push(body.offset);
                return schema.parse({
                    result: ok,
                    calendar_dates: all.slice(body.offset, body.offset + body.limit),
                });
            },
        } as unknown as AuthenticatedCaller;
        const year = await readCalendarYear(caller, 'X', 2025);
        expect(year).toHaveLength(365);
        expect(year[0]?.date).toBe('2025-01-01');
        expect(year.at(-1)?.date).toBe('2025-12-31');
        expect(calls).toEqual([0]);
        expect(await readCalendarYear(caller, 'X', 2027)).toHaveLength(365);
        expect(calls).toEqual([0, 0, 1000]);
    });

    it('answers no days for a year the calendar has not built', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.calendar_dates.list_by_calendar_code': {
                result: ok,
                calendar_dates: days('2024-01-01', 366),
            },
        });
        expect(await readCalendarYear(caller, 'X', 2030)).toEqual([]);
    });
});

describe('rebuildCalendar', () => {
    it('asks for one calendar up to a year, and answers the days written', async () => {
        const { caller, calls } = fakeCaller({
            'refdata.v1.ops.regenerate_calendar_dates': {
                success: true,
                message: '',
                rows_written: 365,
            },
        });
        expect(await rebuildCalendar(caller, 'X', 2030)).toBe(365);
        expect(calls[0]?.body).toEqual({ calendar_code: 'X', end_year: 2030 });
    });

    it('fails with the server words when the rebuild is refused', async () => {
        const { caller } = fakeCaller({
            'refdata.v1.ops.regenerate_calendar_dates': {
                success: false,
                message: 'Permission denied',
            },
        });
        await expect(rebuildCalendar(caller, 'X', 2030)).rejects.toThrow('Permission denied');
    });
});
