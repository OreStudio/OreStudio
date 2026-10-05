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
import type { AccountAccess, HistoryVersion } from '@ores/wire-protocol/browser';
import type { RecordResourceView, RecordRow } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { CURRENCY_FIELDS, CurrenciesPage, CurrencyPage } from './currencies.js';
import { CurrencyPairPage } from './currencyPairs.js';
import { DeskGroupPage } from './deskGroups.js';
import { valuesFromHistory, writeOf, type FieldSpec } from './records.js';
import { reasonsFor } from './shared.js';
import { parseTimestamp, relativeTime } from '../ui/Time.js';

/**
 * The record screens, rendered from a seeded query cache, and the field table
 * that turns a form into a write and a history version back into a form.
 */

function resource(key: string, keyFields: string[], versioned = true): RecordResourceView {
    const name = key.replaceAll('-', '_');
    return {
        key,
        entityType: `ores.refdata.${name}`,
        keyFields,
        versioned,
        writable: true,
        search: key === 'currencies',
        sortable: key === 'currencies' ? ['iso_code', 'name'] : [],
        writePermission: `refdata::${name}:write`,
        deletePermission: `refdata::${name}:delete`,
    };
}

const REGISTRY: RecordResourceView[] = [
    resource('currencies', ['iso_code']),
    resource('currency-groups', ['code']),
    resource('currency-countries', ['currency_iso_code', 'country_alpha2_code'], false),
    resource('currency-memberships', ['currency_iso_code', 'currency_group_code'], false),
    resource('currency-pairs', ['pair_code']),
    resource('pair-calendars', ['pair_code', 'calendar_code'], false),
];

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Operations',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

const euro: RecordRow = {
    version: 2,
    iso_code: 'EUR',
    name: 'Euro',
    numeric_code: '978',
    symbol: 'E',
    fraction_symbol: 'c',
    fractions_per_unit: 100,
    rounding_type: 'Closest',
    rounding_precision: 2,
    format: '#,##0.00',
    monetary_nature: 'fiat',
    market_tier: 'g10',
    ore_currency_type: null,
    image_id: null,
    spot_days: 2,
    day_basis: 'ACT/360',
    base_precedence: 1,
};

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['refdata-registry'], REGISTRY);
    client.setQueryData(['labels'], { labels: [], domains: {} });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/refdata/currencies" element={<CurrenciesPage />} />
                        <Route path="/refdata/currencies/:code" element={<CurrencyPage />} />
                        <Route path="/refdata/desk-groups/:code" element={<DeskGroupPage />} />
                        <Route
                            path="/refdata/currency-pairs/:code"
                            element={<CurrencyPairPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the field table', () => {
    const specs: FieldSpec[] = [
        { field: 'name', history: 'Name', kind: { kind: 'text' } },
        { field: 'spot_days', history: 'Spot Days', kind: { kind: 'int' } },
        { field: 'pip_factor', history: 'Pip Factor', kind: { kind: 'decimal' } },
        { field: 'end_of_month', history: 'End Of Month', kind: { kind: 'bool' }, optional: true },
        {
            field: 'ore_currency_type',
            history: 'Ore Currency Type',
            kind: { kind: 'text' },
            optional: true,
        },
        { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
    ];

    it('types each field as the server expects, writing null for an empty optional and empty for a blank', () => {
        expect(
            writeOf(specs, {
                name: ' Euro ',
                spot_days: '2',
                pip_factor: '0.0001',
                end_of_month: 'false',
                ore_currency_type: '',
                description: '',
            }),
        ).toEqual({
            name: 'Euro',
            spot_days: 2,
            pip_factor: 0.0001,
            end_of_month: false,
            ore_currency_type: null,
            description: '',
        });
    });

    it('reads an older version back by the names the history gives the fields', () => {
        const version: HistoryVersion = {
            version: 1,
            modifiedBy: 'priya',
            recordedAt: 't1',
            fields: [
                { name: 'Name', value: 'EURO' },
                { name: 'Spot Days', value: '3' },
            ],
            changes: [],
        };
        expect(valuesFromHistory(specs.slice(0, 2), version)).toEqual({
            name: 'EURO',
            spot_days: '3',
        });
    });

    it('covers every currency column the server writes but the image', () => {
        expect(CURRENCY_FIELDS.map((spec) => spec.field).sort()).toEqual(
            Object.keys(euro)
                .filter((field) => field !== 'version' && field !== 'image_id')
                .sort(),
        );
    });
});

const firstPage = { offset: 0, limit: 100, search: '', sort: '', descending: false };

describe('the currency screens', () => {
    it('lists one page of currencies with the server total, Refresh, Add and the pager', () => {
        const html = render('/refdata/currencies', (client) => {
            client.setQueryData(['records', 'currencies', 'page', firstPage], {
                rows: [euro],
                total: 168,
            });
            client.setQueryData(['my-access'], access(['refdata::currencies:write']));
        });
        expect(html).toContain('Add currency');
        expect(html).toContain('Refresh');
        expect(html).toContain('Euro');
        expect(html).toContain('1–1 of 168');
        expect(html).toContain('Page size');
        expect(html).toContain('Load all');
        expect(html).toContain('Search…');
    });

    it('orders by a sortable column from the address, and marks it', () => {
        const html = render('/refdata/currencies?sort=name&desc=1', (client) => {
            client.setQueryData(
                ['records', 'currencies', 'page', { ...firstPage, sort: 'name', descending: true }],
                { rows: [euro], total: 1 },
            );
        });
        expect(html).toContain('aria-sort="descending"');
        expect(html).toContain('Name ↓');
    });

    it('says the list is empty, and offers Add to a writer', () => {
        const html = render('/refdata/currencies', (client) => {
            client.setQueryData(['records', 'currencies', 'page', firstPage], {
                rows: [],
                total: 0,
            });
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('There are no currencies yet.');
    });

    it('shows a currency with its fields, the export note, Edit and Delete, and its links as tabs', () => {
        const html = render('/refdata/currencies/EUR', (client) => {
            client.setQueryData(['records', 'currencies', 'key', 'EUR'], euro);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('ACT/360');
        expect(html).toContain('>Countries<');
        expect(html).toContain('>Desk groups<');
        expect(html).toContain('needs a server operation that does not exist yet');
        expect(html).toContain('Edit');
        expect(html).toContain('Delete');
        expect(html).not.toContain('>Remove<');
    });

    it('lists the countries on their own tab, each removable as a link', () => {
        const html = render('/refdata/currencies/EUR?tab=countries', (client) => {
            client.setQueryData(['records', 'currencies', 'key', 'EUR'], euro);
            client.setQueryData(
                ['records', 'countries'],
                [{ version: 1, alpha2_code: 'DE', name: 'Germany' }],
            );
            client.setQueryData(
                ['records', 'currency-countries', 'EUR'],
                [{ version: 1, currency_iso_code: 'EUR', country_alpha2_code: 'DE' }],
            );
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('Germany');
        expect(html).toContain('Remove');
    });

    it('ends the details with who last changed the record', () => {
        const html = render('/refdata/currencies/EUR', (client) => {
            client.setQueryData(['records', 'currencies', 'key', 'EUR'], {
                ...euro,
                modified_by: 'svc',
                performed_by: 'priya',
                recorded_at: '2026-10-04 22:10:00Z',
                change_reason_code: 'common.rectification',
                change_commentary: 'Fixed the symbol',
            });
        });
        expect(html).toContain('Last changed:');
        expect(html).toContain('svc for priya');
        expect(html).toContain('common.rectification');
        expect(html).toContain('Fixed the symbol');
    });

    it('offers a reader no change', () => {
        const html = render('/refdata/currencies/EUR', (client) => {
            client.setQueryData(['records', 'currencies', 'key', 'EUR'], euro);
            client.setQueryData(['my-access'], access(['refdata::currencies:read']));
        });
        expect(html).not.toContain('Edit');
        expect(html).not.toContain('Delete');
    });
});

describe('the desk group screen', () => {
    it("lists the group's members on their own tab", () => {
        const html = render('/refdata/desk-groups/G11?tab=members', (client) => {
            client.setQueryData(['records', 'currency-groups', 'key', 'G11'], {
                version: 1,
                code: 'G11',
                name: 'Group of eleven',
                description: '',
                display_order: 10,
            });
            client.setQueryData(['records', 'currencies'], [euro]);
            client.setQueryData(
                ['records', 'currency-memberships'],
                [
                    { version: 1, currency_iso_code: 'EUR', currency_group_code: 'G11' },
                    { version: 1, currency_iso_code: 'NOK', currency_group_code: 'SCANDIES' },
                ],
            );
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('Group of eleven');
        expect(html).toContain('>Euro<');
        expect(html).not.toContain('NOK');
    });
});

describe('the currency pair screen', () => {
    const pair = {
        version: 1,
        pair_code: 'EUR/USD',
        base_currency: 'EUR',
        quote_currency: 'USD',
        classification: 'major',
    };

    it('shows the convention with a sample rate at its precision', () => {
        const html = render('/refdata/currency-pairs/EUR%2FUSD', (client) => {
            client.setQueryData(['records', 'currency-pairs', 'key', 'EUR/USD'], pair);
            client.setQueryData(['records', 'currency-pair-conventions', 'key', 'EUR/USD'], {
                version: 1,
                pair_code: 'EUR/USD',
                pip_factor: 0.0001,
                tick_size: 1,
                decimal_places: 4,
                spot_relative: true,
                end_of_month: false,
            });
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('EUR/USD');
        expect(html).toContain('1.0842');
        expect(html).toContain('href="/refdata/currencies/USD"');
        expect(html).toContain('>Calendars<');
    });

    it('says when a pair has no convention', () => {
        const html = render('/refdata/currency-pairs/EUR%2FUSD', (client) => {
            client.setQueryData(['records', 'currency-pairs', 'key', 'EUR/USD'], pair);
            client.setQueryData(['records', 'currency-pair-conventions', 'key', 'EUR/USD'], null);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('This pair has no convention');
    });
});

describe('the reasons a write offers', () => {
    const reasons = [
        { code: 'common.non_material_update' },
        { code: 'common.rectification' },
        { code: 'common.data_correction' },
    ];

    it('refuses a touch for a change, and anything but a touch for no change', () => {
        expect(reasonsFor(reasons, 'amend', true).map((reason) => reason.code)).toEqual([
            'common.rectification',
            'common.data_correction',
        ]);
        expect(reasonsFor(reasons, 'amend', false).map((reason) => reason.code)).toEqual([
            'common.non_material_update',
        ]);
        expect(reasonsFor(reasons, 'delete', true)).toHaveLength(3);
    });
});

describe('a relative time', () => {
    it("says how long ago, in the person's language", () => {
        const now = new Date('2026-10-05T12:00:00Z');
        expect(relativeTime(parseTimestamp('2026-10-05 10:00:00Z'), now, 'en')).toBe('2 hours ago');
        expect(relativeTime(parseTimestamp('2026-10-04 12:00:00Z'), now, 'en')).toBe('yesterday');
    });
});
