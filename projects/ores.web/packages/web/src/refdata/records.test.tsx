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

describe('the currency screens', () => {
    it('lists the currencies with an add for a writer', () => {
        const html = render('/refdata/currencies', (client) => {
            client.setQueryData(['records', 'currencies'], [euro]);
            client.setQueryData(['my-access'], access(['refdata::currencies:write']));
        });
        expect(html).toContain('Add currency');
        expect(html).toContain('Euro');
    });

    it('shows a currency with its fields, its link panels and the export note', () => {
        const html = render('/refdata/currencies/EUR', (client) => {
            client.setQueryData(['records', 'currencies'], [euro]);
            client.setQueryData(
                ['records', 'countries'],
                [{ version: 1, alpha2_code: 'DE', name: 'Germany' }],
            );
            client.setQueryData(
                ['records', 'currency-countries', 'EUR'],
                [{ version: 1, currency_iso_code: 'EUR', country_alpha2_code: 'DE' }],
            );
            client.setQueryData(['records', 'currency-calendars', 'EUR'], []);
            client.setQueryData(['records', 'currency-memberships', 'EUR'], []);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('ACT/360');
        expect(html).toContain('Germany');
        expect(html).toContain('Desk groups');
        expect(html).toContain('needs a server operation that does not exist yet');
        expect(html).toContain('>Edit<');
    });

    it('offers a reader no change', () => {
        const html = render('/refdata/currencies/EUR', (client) => {
            client.setQueryData(['records', 'currencies'], [euro]);
            client.setQueryData(['my-access'], access(['refdata::currencies:read']));
        });
        expect(html).not.toContain('>Edit<');
        expect(html).not.toContain('Choose one to add');
    });
});

describe('the desk group screen', () => {
    it("lists the group's members from the whole junction", () => {
        const html = render('/refdata/desk-groups/G11', (client) => {
            client.setQueryData(
                ['records', 'currency-groups'],
                [
                    {
                        version: 1,
                        code: 'G11',
                        name: 'Group of eleven',
                        description: '',
                        display_order: 10,
                    },
                ],
            );
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
            client.setQueryData(['records', 'currency-pairs'], [pair]);
            client.setQueryData(
                ['records', 'currency-pair-conventions'],
                [
                    {
                        version: 1,
                        pair_code: 'EUR/USD',
                        pip_factor: 0.0001,
                        tick_size: 1,
                        decimal_places: 4,
                        spot_relative: true,
                        end_of_month: false,
                    },
                ],
            );
            client.setQueryData(['records', 'pair-calendars', 'EUR/USD'], []);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('EUR/USD');
        expect(html).toContain('1.0842');
        expect(html).toContain('href="/refdata/currencies/USD"');
    });

    it('says when a pair has no convention', () => {
        const html = render('/refdata/currency-pairs/EUR%2FUSD', (client) => {
            client.setQueryData(['records', 'currency-pairs'], [pair]);
            client.setQueryData(['records', 'currency-pair-conventions'], []);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('This pair has no convention');
    });
});
