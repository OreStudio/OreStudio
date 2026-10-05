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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { CalendarDay, RecordResourceView, RecordRow } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { CalendarPage, CalendarsPage } from './calendars.js';
import { FIRST_PAGE, pageKey } from './RecordList.js';
import { invalidFields, writeOf, type FieldSpec } from './records.js';

function resource(key: string): RecordResourceView {
    const name = key.replaceAll('-', '_');
    return {
        key,
        entityType: `ores.refdata.${name}`,
        keyFields: key === 'calendars' ? ['code'] : ['id'],
        versioned: true,
        writable: true,
        writePermission: `refdata::${name}:write`,
        deletePermission: `refdata::${name}:delete`,
    };
}

const REGISTRY = ['calendars', 'calendar-rules', 'calendar-exceptions', 'calendar-events'].map(
    resource,
);

const everything: AccountAccess = {
    roles: [
        {
            roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
            name: 'TenantAdmin',
            description: '',
            permissionCodes: ['*'],
            givenBy: 'system',
            givenAt: '2026-10-04',
            reasonCode: 'system.new_record',
            commentary: '',
        },
    ],
};

function calendar(code: string, fields: Partial<RecordRow>): RecordRow {
    return {
        version: 1,
        code,
        name: `${code} calendar`,
        calendar_type: 'public_holiday',
        country_code: 'GB',
        image_id: null,
        source: 'user',
        is_editable: true,
        base_calendar_code: null,
        ...fields,
    };
}

const target = calendar('TARGET', { source: 'quantlib', is_editable: false });
const office = calendar('OFFICE', {});
const desk = calendar('DESK', { base_calendar_code: 'TARGET' });

function render(
    path: string,
    parts: {
        readonly rules?: RecordRow[];
        readonly exceptions?: RecordRow[];
        readonly days?: CalendarDay[];
    } = {},
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['refdata-registry'], REGISTRY);
    client.setQueryData(['my-access'], everything);
    client.setQueryData(['labels'], { labels: [], domains: {} });
    client.setQueryData(['records', 'calendars'], [target, office, desk]);
    client.setQueryData(pageKey({ key: 'calendars' }, FIRST_PAGE), {
        rows: [target, office, desk],
        total: 61,
    });
    client.setQueryData(['image-map'], {
        currencies: {},
        countries: { GB: 'gb-flag' },
        calendars: { TARGET: 'eu-flag' },
        businessCentres: {},
        noFlag: null,
    });
    for (const row of [target, office, desk]) {
        client.setQueryData(['records', 'calendars', 'key', row['code']], row);
    }
    for (const code of ['TARGET', 'OFFICE', 'DESK']) {
        client.setQueryData(['records', 'calendar-rules', code], parts.rules ?? []);
        client.setQueryData(['records', 'calendar-exceptions', code], parts.exceptions ?? []);
        client.setQueryData(['records', 'calendar-events', code], []);
        client.setQueryData(['calendar-days', code, new Date().getUTCFullYear()], parts.days ?? []);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/refdata/calendars" element={<CalendarsPage />} />
                        <Route path="/refdata/calendars/:code" element={<CalendarPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function rule(id: string, fields: Partial<RecordRow>): RecordRow {
    return {
        version: 1,
        id,
        calendar_code: 'OFFICE',
        kind: 'fixed_date',
        month: null,
        day: null,
        weekday: null,
        occurrence: null,
        day_offset: null,
        shift: 'none',
        effective_from: null,
        effective_to: null,
        ...fields,
    };
}

describe('the calendar page', () => {
    it('shows a QuantLib calendar read only, with no rules, exceptions or days', () => {
        const html = render('/refdata/calendars/TARGET');
        expect(html).toContain('Derive a calendar');
        expect(html).toContain('QuantLib supplies the holidays');
        expect(html).not.toContain('>Edit<');
        expect(html).not.toContain('Delete');
        expect(html).not.toContain('>Rules<');
        expect(html).not.toContain('>Exceptions<');
        expect(html).not.toContain('>Business days<');
    });

    it('shows a derived calendar its exceptions but no rules', () => {
        const html = render('/refdata/calendars/DESK');
        expect(html).toContain('>Exceptions<');
        expect(html).toContain('>Business days<');
        expect(html).not.toContain('>Rules<');
        expect(html).toContain('TARGET, with exceptions');
    });

    it('describes each rule of a bespoke calendar by its shape, in date order', () => {
        const html = render('/refdata/calendars/OFFICE?tab=rules', {
            rules: [
                rule('b', { kind: 'fixed_date', month: 12, day: 25, shift: 'nearest_weekday' }),
                rule('a', { kind: 'nth_weekday_of_month', month: 1, weekday: 1, occurrence: 3 }),
                rule('c', { kind: 'easter_offset', day_offset: 1, effective_to: 2030 }),
                rule('d', { kind: 'last_weekday_of_month', month: 5, weekday: 1 }),
            ],
        });
        expect(html).toContain('3rd Monday of January');
        expect(html).toContain('25 December');
        expect(html).toContain('Last Monday of May');
        expect(html).toContain('+1 days from Easter');
        expect(html).toContain('Nearest weekday');
        expect(html).toContain('… – 2030');
        expect(html.indexOf('+1 days from Easter')).toBeLessThan(html.indexOf('3rd Monday'));
        expect(html.indexOf('3rd Monday')).toBeLessThan(html.indexOf('25 December'));
    });

    it('lists the weekday holidays of the year with what produced them', () => {
        const year = new Date().getUTCFullYear();
        const days: CalendarDay[] = [];
        for (let day = 0; day < 365; day += 1) {
            const date = new Date(Date.UTC(year, 0, 1 + day)).toISOString().slice(0, 10);
            const weekend = [0, 6].includes(new Date(`${date}T00:00:00Z`).getUTCDay());
            days.push({ date, businessDay: !weekend, source: 'user_adjustment' });
        }
        const firstWeekday = days.find((day) => day.businessDay);
        if (firstWeekday === undefined) {
            throw new Error('A year has weekdays');
        }
        firstWeekday.businessDay = false;
        const html = render('/refdata/calendars/DESK?tab=days', {
            days,
            exceptions: [
                {
                    version: 1,
                    id: 'e',
                    calendar_code: 'DESK',
                    exception_date: firstWeekday.date,
                    is_business_day: false,
                    description: 'Office move',
                },
                {
                    version: 1,
                    id: 'f',
                    calendar_code: 'DESK',
                    exception_date: `${String(year)}-12-31`,
                    is_business_day: !days.at(-1)?.businessDay,
                    description: 'Late addition',
                },
            ],
        });
        expect(html).toContain('Office move');
        expect(html).toContain('Late addition: not in the built days');
        expect(html).toContain('1 weekday holidays');
        expect(html).toContain('TARGET takes its holidays from QuantLib');
        expect(html).toContain('Rebuild business days');
    });

    it('says a year is not built when the calendar holds no days for it', () => {
        const html = render('/refdata/calendars/OFFICE?tab=days');
        expect(html).toContain('No business days are built for');
    });
});

describe('a field asked for only in some shapes', () => {
    const specs: FieldSpec[] = [
        { field: 'kind', history: 'Kind', kind: { kind: 'choice', options: ['a', 'b'] } },
        {
            field: 'month',
            history: 'Month',
            kind: { kind: 'choice', options: ['1', '12'], numeric: true },
            when: (values) => values['kind'] === 'a',
        },
        {
            field: 'day',
            history: 'Day',
            kind: { kind: 'int', min: 1, max: 31 },
            when: (values) => values['kind'] === 'a',
        },
    ];

    it('writes a number when asked, and null when not, even with a stale value', () => {
        expect(writeOf(specs, { kind: 'a', month: '12', day: '25' })).toEqual({
            kind: 'a',
            month: 12,
            day: 25,
        });
        expect(writeOf(specs, { kind: 'b', month: '12', day: '25' })).toEqual({
            kind: 'b',
            month: null,
            day: null,
        });
    });

    it('refuses an empty or out-of-range value only while the field is asked for', () => {
        expect(invalidFields(specs, { kind: 'a', month: '', day: '32' })).toEqual(['month', 'day']);
        expect(invalidFields(specs, { kind: 'b', month: '', day: '32' })).toEqual([]);
    });
});

describe('the calendar list', () => {
    it('draws one page of calendars with the server total, each with its flag', () => {
        const html = render('/refdata/calendars');
        expect(html).toContain('1–3 of 61');
        expect(html).toContain('/api/images/eu-flag');
        expect(html).toContain('/api/images/gb-flag');
        expect(html).toContain('TARGET, with exceptions');
    });
});
