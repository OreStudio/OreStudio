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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { ConventionDocument, ConventionTerm } from '../api/conventions.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ConventionRecordPage, KindCard } from './ConventionRecordPage.js';

const BLANK: ConventionTerm = {
    entity: 'swap_convention',
    column: '',
    ore: '',
    xpath: '',
    value: null,
    documented: false,
    required: false,
    label: '',
    help: '',
    description: '',
    defaultText: '',
    format: '',
    introducedIn: '',
    removedIn: '',
    sourceKind: '',
    choices: [],
};

const DOCUMENT: ConventionDocument = {
    family: 'swap',
    id: 'EUR-6M-SWAP',
    shared: false,
    kind: {
        code: 'SWAP',
        name: 'Swap',
        description: 'Vanilla interest rate swap convention',
        assetClass: 'interest-rates',
        oreElement: 'Swap',
    },
    version: 3,
    modifiedBy: 'tenant_admin',
    recordedAt: '2026-10-26 00:00:00',
    terms: [
        {
            ...BLANK,
            column: 'fixed_frequency',
            ore: 'FixedFrequency',
            xpath: '/Conventions/Swap/FixedFrequency',
            value: 'A',
            documented: true,
            required: true,
            label: 'Fixed frequency',
            description: 'The frequency of payments on the fixed leg.',
            defaultText: 'None: the convention must give it.',
            sourceKind: 'enumerated',
            choices: [
                { value: 'A', label: 'Annual' },
                { value: 'Q', label: 'Quarterly' },
            ],
        },
        {
            ...BLANK,
            column: 'float_frequency',
            ore: 'FloatFrequency',
            xpath: '/Conventions/Swap/FloatFrequency',
            documented: true,
            label: 'Float frequency',
            defaultText: 'Not given: ORE applies its own rule.',
        },
        {
            ...BLANK,
            column: 'fixed_calendar',
            ore: 'FixedCalendar',
            xpath: '/Conventions/Swap/FixedCalendar',
            value: 'TARGET',
        },
    ],
};

function render(): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['conventions', 'swap', 'documents', 'EUR-6M-SWAP'], DOCUMENT);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/refdata/conventions/swap/EUR-6M-SWAP']}>
                    <Routes>
                        <Route
                            path="/refdata/conventions/:family/:id"
                            element={<ConventionRecordPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the record page of a convention', () => {
    it('heads the page with the convention and the version it was read at', () => {
        const html = render();

        expect(html).toContain('EUR-6M-SWAP');
        expect(html).toContain('EUR-6M-SWAP · version 3');
        expect(html).toContain('Vanilla interest rate swap convention');
        expect(html).toContain('Interest rates');
    });

    it('draws each term with its one label, its marks and the value in force', () => {
        const html = render();

        expect(html).toContain('Fixed frequency');
        expect(html).toContain('Required');
        // A value from a closed list shows the words it means beside the code.
        expect(html).toContain('Annual (A)');
        // A term the convention leaves out shows the default ORE uses.
        expect(html).toContain('Default: Not given: ORE applies its own rule.');
    });

    it('draws a term the contract does not document under its ORE name', () => {
        const html = render();

        expect(html).toContain('FixedCalendar');
        expect(html).toContain('TARGET');
    });

    it('names the switch for a screen of leaves and counts the terms with their noun', () => {
        const html = render();

        expect(html).toContain('aria-label="How much to show"');
        expect(html).toContain('3 of 3 terms shown');
        expect(html).toContain('Show all documentation');
        // The data cannot say which terms change the numbers, so that filter is not drawn.
        expect(html).not.toContain('Changes the numbers');
    });

    it('shows the kind code and its ORE element only in Advanced', () => {
        const kind = (mode: 'simple' | 'advanced') =>
            renderToStaticMarkup(
                <TranslationProvider>
                    <KindCard document={DOCUMENT} mode={mode} />
                </TranslationProvider>,
            );

        expect(kind('simple')).not.toContain('ORE element');
        expect(kind('advanced')).toContain('ORE element');
        expect(kind('advanced')).toContain('SWAP');
    });
});
