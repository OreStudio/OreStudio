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
import { ConventionRecordPage, KindCard, TermRow, changedTerms } from './ConventionRecordPage.js';

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
    minimum: null,
    maximum: null,
    pattern: '',
};

const DOCUMENT: ConventionDocument = {
    family: 'swap',
    id: 'EUR-6M-SWAP',
    shared: false,
    editable: true,
    partyId: null,
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
        {
            ...BLANK,
            column: 'overnight_lockout_days',
            ore: 'OvernightLockouDays',
            xpath: '/Conventions/BMABasisSwap/OvernightLockouDays',
        },
    ],
};

function render(
    permissionCodes: readonly string[] = [],
    document = DOCUMENT,
    entry = '/refdata/conventions/swap/EUR-6M-SWAP',
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['conventions', 'swap', 'documents', 'EUR-6M-SWAP', ''], document);
    client.setQueryData(['my-access'], { roles: [{ roleId: 'r', permissionCodes }] });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[entry]}>
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

    it('shows a dash, not an invented default, for an undocumented term the convention leaves out', () => {
        const html = render();
        const row = html.slice(html.indexOf('OvernightLockouDays'));

        expect(row).toContain('—');
        expect(row.slice(0, row.indexOf('</li>'))).not.toContain('Default:');
    });

    it('names the switch for a screen of leaves and counts the terms with their noun', () => {
        const html = render();

        expect(html).toContain('aria-label="How much to show"');
        expect(html).toContain('4 of 4 terms shown');
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

    it('offers the edit only to a person who may write the entity', () => {
        expect(render()).not.toContain('Edit terms');
        expect(render(['refdata::swap_conventions:write'])).toContain('Edit terms');
    });

    it('offers no edit of a convention another party holds, and says how to change it', () => {
        const html = render(['refdata::swap_conventions:write'], {
            ...DOCUMENT,
            partyId: '66666666-6666-4666-8666-666666666666',
        });

        expect(html).not.toContain('Edit terms');
        expect(html).toContain('belongs to another party');
    });

    it('keeps a composite read-only and says why', () => {
        const html = render(['refdata::swap_conventions:write'], { ...DOCUMENT, editable: false });

        expect(html).not.toContain('Edit terms');
        expect(html).toContain('held on two parts');
    });
});

describe('a term in edit', () => {
    const [frequency] = DOCUMENT.terms;
    const row = (props: { text: string; changed?: boolean; refused?: string | null }) =>
        renderToStaticMarkup(
            <TranslationProvider>
                <ul>
                    <TermRow
                        term={frequency!}
                        mode="simple"
                        text={props.text}
                        changed={props.changed ?? false}
                        refused={props.refused ?? null}
                        open={false}
                        onToggle={() => undefined}
                        onText={() => undefined}
                        onFocus={() => undefined}
                    />
                </ul>
            </TranslationProvider>,
        );

    it('opens the select on Default (X), then the values of the set', () => {
        const html = row({ text: 'A' });
        const first = html.indexOf('<option');

        expect(html.slice(first)).toMatch(
            /^<option value="">Default \(None: the convention must give it\)<\/option>/,
        );
        expect(html).toContain('>Annual</option>');
        expect(html).toContain('>Quarterly</option>');
    });

    it('marks a changed term', () => {
        expect(row({ text: 'Q', changed: true })).toContain('changed');
        expect(row({ text: 'A' })).not.toContain('>changed<');
    });

    it('shows a refusal on the term, in the server words', () => {
        const html = row({
            text: 'Q',
            refused: 'Invalid value Q for Swap.FixedFrequency: the value is not in set x.',
        });

        expect(html).toContain('role="alert"');
        expect(html).toContain('Invalid value Q for Swap.FixedFrequency');
        expect(html).toContain('aria-invalid="true"');
    });
});

describe('the tabs of a convention record', () => {
    it('offers the record and its history, and draws the terms first', () => {
        const html = render();

        expect(html).toContain('role="tablist"');
        expect(html).toContain('>Details<');
        expect(html).toContain('>History<');
        expect(html).toContain('Fixed frequency');
    });

    it("reads the convention's versions on its history tab instead of the terms", () => {
        const html = render([], DOCUMENT, '/refdata/conventions/swap/EUR-6M-SWAP?tab=history');

        expect(html).toMatch(/aria-selected="true"[^>]*>History</);
        expect(html).not.toContain('Fixed frequency');
    });
});

describe('the changed terms of an edit', () => {
    it('names only the columns whose text differs, and unsets an emptied term', () => {
        const editing = {
            fixed_frequency: 'Q',
            float_frequency: '',
            fixed_calendar: '',
            overnight_lockout_days: '',
        };

        expect(changedTerms(DOCUMENT.terms, editing)).toEqual({
            fixed_frequency: 'Q',
            fixed_calendar: null,
        });
    });

    it('is empty when nothing changed', () => {
        const editing = Object.fromEntries(
            DOCUMENT.terms.map((term) => [term.column, term.value ?? '']),
        );

        expect(changedTerms(DOCUMENT.terms, editing)).toEqual({});
    });
});
