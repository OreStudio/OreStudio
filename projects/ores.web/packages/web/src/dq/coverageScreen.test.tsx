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
 */

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { BundleRosterBundle } from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { TranslationProvider } from '../i18n/Provider.js';
import { CoveragePage } from './CoveragePage.js';

/**
 * The coverage list, called the way a person reaches it: the roster in the query
 * cache, drawn as the bundles of each level beside the datasets of the bundle in
 * view, each dataset with what it has staged and the reason it cannot publish.
 */

function member(code: string, extra: Record<string, unknown> = {}) {
    return {
        display_order: 1,
        dataset_name: `${code} dataset`,
        artefact_type: 'currency',
        artefact_table: 'dq_currencies_artefact_tbl',
        target_subject: 'refdata_currencies',
        has_dataset: true,
        has_artefact_type: true,
        staged_count: 160,
        is_publishable: true,
        optional: false,
        dataset_code: code,
        ...extra,
    };
}

const WORLD: readonly BundleRosterBundle[] = [
    {
        code: 'world',
        name: 'World reference data',
        description: '',
        level: 'world',
        dataset_count: 3,
        members: [
            member('currencies'),
            member('calendars', { has_dataset: false }),
            member('lei_entities', { staged_count: 0, is_publishable: false }),
        ],
    },
];

function render(roster: readonly BundleRosterBundle[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['dq-roster', ''], roster);
    client.setQueryData(['dq-roster', roster[0]?.code ?? ''], roster);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/data-quality/coverage']}>
                    <CoveragePage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the coverage screen', () => {
    it('draws the bundles of each level beside the table of one bundle', () => {
        const html = render(WORLD);

        expect(html).toContain('Reference data coverage');
        expect(html).toContain('world');
        expect(html).toContain('World reference data');
        expect(html).toContain('data-quality');
        expect(html).toContain('world · 3 datasets · world level');
    });

    it('draws each dataset with what it staged and why it cannot publish', () => {
        const html = render(WORLD);

        expect(html).toContain('currencies');
        expect(html).toContain('160');
        expect(html).toContain('Metadata complete');
        expect(html).toContain('No dataset row');
        expect(html).toContain('No staged rows');
    });

    it('filter by state and by words, so a person reads the rows that need work', () => {
        const html = render(WORLD);

        expect(html).toContain('Filter by dataset or artefact type');
        expect(html).toContain('Every state');
        expect(html).toContain('Not ready');
    });

    it('draws a fresh database as a sentence and not as a failure', () => {
        const html = render([]);

        expect(html).toContain('This database holds no bundle yet');
        expect(html).not.toContain('Staged');
    });
});
