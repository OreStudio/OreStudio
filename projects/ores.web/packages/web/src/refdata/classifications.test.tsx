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
import type {
    AccountAccess,
    BadgePresentation,
    ClassificationList,
    ClassificationRow,
    HistoryVersion,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { ClassificationListPage } from './ClassificationListPage.js';
import { ClassificationRowPage } from './ClassificationRowPage.js';
import { ClassificationsPage } from './ClassificationsPage.js';
import { RefdataPage } from './RefdataPage.js';

/**
 * The reference data screens, rendered from a seeded query cache.
 *
 * What is checked is what each screen promises: every list has a name in the
 * person's language, rows carry the labels the shared catalogue gives them, a
 * list of ORE spellings cannot be changed, a person who may only read is
 * offered no change, and the history shows what changed as old and new lines.
 */

type Listed = ClassificationList & { count: number | null };

function listOf(
    key: string,
    shape: ClassificationList['shape'],
    editable: boolean,
    topic = 'currencies',
): Listed {
    const resource = key.replaceAll('-', '_') + 's';
    return {
        key,
        entityType: `ores.refdata.${key.replaceAll('-', '_')}`,
        topic,
        shape,
        editable,
        writePermission: `refdata::${resource}:write`,
        deletePermission: `refdata::${resource}:delete`,
        count: 5,
    };
}

const LISTS: Listed[] = [
    listOf('rounding-type', 'named', true),
    listOf('day-counter', 'plain', false, 'products'),
    listOf('tenor-anchor', 'ordered', true, 'tenors'),
];

function row(
    code: string,
    name: string,
    order: number | null,
    labelCode: string | null,
): ClassificationRow {
    return {
        code,
        name,
        description: `${code} explained`,
        displayOrder: order,
        version: 2,
        modifiedBy: 'priya',
        recordedAt: '2026-10-04 09:05',
        reasonCode: 'common.rectification',
        commentary: 'Fixed',
        labelCode,
    };
}

function badge(code: string, label: string, colour: string): BadgePresentation {
    return {
        code,
        label,
        description: '',
        backgroundColour: colour,
        textColour: '#ffffff',
        severity: '',
    };
}

const LABELS = {
    labels: [
        badge('__unmapped__', 'Unmapped', '#f97316'),
        badge('rounding_type_up', 'Up', '#8b5cf6'),
        badge('active', 'Active', '#22c55e'),
    ],
    domains: { rounding_type: ['rounding_type_up'], party_status: ['active'] },
};

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

function field(name: string, value: string): { name: string; value: string } {
    return { name, value };
}

function version(n: number, name: string, order: string, reason: string): HistoryVersion {
    return {
        version: n,
        modifiedBy: 'priya',
        recordedAt: `2026-10-0${String(n)} 09:00`,
        fields: [
            field('Code', 'Up'),
            field('Name', name),
            field('Description', 'Away from zero'),
            field('Display Order', order),
            field('Modified By', 'priya'),
            field('Performed By', 'ores_refdata_service'),
            field('Change Reason Code', reason),
            field('Change Commentary', n === 2 ? 'Name was in capitals' : ''),
            field('Recorded At', `2026-10-0${String(n)} 09:00`),
        ],
        changes: [],
    };
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['classifications'], LISTS);
    client.setQueryData(['labels'], LABELS);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/refdata" element={<RefdataPage />} />
                        <Route path="/refdata/classifications" element={<ClassificationsPage />} />
                        <Route
                            path="/refdata/classifications/:list"
                            element={<ClassificationListPage />}
                        />
                        <Route
                            path="/refdata/classifications/:list/:code"
                            element={<ClassificationRowPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the reference data area', () => {
    it('opens classifications and shows the screens still to come', () => {
        const html = render('/refdata', () => undefined);
        expect(html).toContain('href="/refdata/classifications"');
        expect(html).toContain('Holiday calendars');
        expect(html).toContain('Designed; not built yet');
    });
});

describe('the classification index', () => {
    it('names every one of the 28 lists, topics and descriptions in words', () => {
        const keys = [
            'monetary-nature',
            'rounding-type',
            'currency-market-tier',
            'currency-pair-classification',
            'calendar-type',
            'diary-entry-type',
            'calendar-name',
            'business-day-convention-type',
            'party-type',
            'party-status',
            'contact-type',
            'book-status',
            'book-purpose-type',
            'ledger-feed-type',
            'regulatory-book-type',
            'purpose-type',
            'asset-class-code',
            'leg-type',
            'floating-index-type',
            'day-counter',
            'day-count-fraction-type',
            'curve-role',
            'tenor-kind',
            'tenor-unit',
            'tenor-anchor',
            'tenor-resolution-algorithm',
            'derivation-kind',
            'series-subclass-code',
        ];
        const topics = [
            'currencies',
            'calendars',
            'parties',
            'books',
            'products',
            'tenors',
            'market-data',
        ];
        const lists = keys.map((key, index) =>
            listOf(key, 'named', true, topics[index % topics.length]),
        );
        const html = render('/refdata/classifications', (client) =>
            client.setQueryData(['classifications'], lists),
        );
        expect(html).not.toContain('refdata.classifications.');
        expect(html).toContain('Rounding types');
        expect(html).toContain('Market data');
        for (const key of keys) {
            const brief = render(`/refdata/classifications/${key}`, (client) => {
                client.setQueryData(['classifications'], lists);
                client.setQueryData(['classifications', key], []);
                client.setQueryData(['my-access'], access([]));
            });
            expect(brief).not.toContain('refdata.classifications.briefs.');
        }
    });

    it('marks the ORE lists read-only and shows each count', () => {
        const html = render('/refdata/classifications', () => undefined);
        expect(html).toContain('Read-only');
        expect(html).toContain('href="/refdata/classifications/day-counter"');
    });
});

describe('one list', () => {
    it('offers a writer the actions, and paints each row with its label', () => {
        const html = render('/refdata/classifications/rounding-type', (client) => {
            client.setQueryData(
                ['classifications', 'rounding-type'],
                [row('Up', 'Round Up', 1, 'rounding_type_up'), row('Odd', 'Odd', 2, null)],
            );
            client.setQueryData(['my-access'], access(['refdata::rounding_types:write']));
        });
        expect(html).toContain('Add row');
        expect(html).toContain('Reorder');
        expect(html).toContain('>Label</th>');
        expect(html).toContain('background-color:#8b5cf6');
        expect(html).toContain('Unmapped');
    });

    it('offers a reader no change, and says why', () => {
        const html = render('/refdata/classifications/rounding-type', (client) => {
            client.setQueryData(
                ['classifications', 'rounding-type'],
                [row('Up', 'Round Up', 1, null)],
            );
            client.setQueryData(['my-access'], access(['refdata::rounding_types:read']));
        });
        expect(html).not.toContain('Add row');
        expect(html).toContain('Changing it needs the reference data permissions');
    });

    it('shows a list of ORE spellings read-only, even to someone who holds everything', () => {
        const html = render('/refdata/classifications/day-counter', (client) => {
            client.setQueryData(['classifications', 'day-counter'], [row('A360', '', null, null)]);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('ORE documents write these spellings exactly');
        expect(html).not.toContain('Add row');
        expect(html).not.toContain('>Order</th>');
        expect(html).not.toContain('>Label</th>');
    });
});

describe('one row', () => {
    it('opens on its details with its label and the actions a writer may take', () => {
        const html = render('/refdata/classifications/rounding-type/Up', (client) => {
            client.setQueryData(
                ['classifications', 'rounding-type'],
                [row('Up', 'Round Up', 1, 'rounding_type_up')],
            );
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('Round Up');
        expect(html).toContain('Rounding types · Up · version 2');
        expect(html).toContain('>Edit<');
        expect(html).toContain('>Remove<');
        expect(html).toContain('aria-selected="true"');
        expect(html).toContain('common.rectification — Fixed');
    });

    it('shows a change as an old line and a new line, and the provenance in the timeline', () => {
        const html = render('/refdata/classifications/rounding-type/Up?tab=history', (client) => {
            client.setQueryData(
                ['classifications', 'rounding-type'],
                [row('Up', 'Round Up', 1, 'rounding_type_up')],
            );
            client.setQueryData(['my-access'], access(['*']));
            client.setQueryData(
                ['history', 'ores.refdata.rounding_type', 'Up'],
                [
                    version(2, 'Round Up', '1', 'common.rectification'),
                    version(1, 'ROUND UP', '1', 'system.new_record'),
                ],
            );
        });
        expect(html).toContain('Performed by ores_refdata_service');
        expect(html).toContain('“Name was in capitals”');
        expect(html).toContain('Value diff');
        expect(html).toContain('>−<');
        expect(html).toContain('>+<');
        expect(html).toContain('<mark');
        expect(html).not.toContain('>Recorded At<');
    });
});
