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

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type {
    AccountAccess,
    HistoryVersion,
    SensitivityTemplate,
    SensitivityTemplateChoices,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { textOf } from '../textOf.js';
import { ENTITY_TYPE } from './choices.js';
import { SensitivityTemplatePage, EditTemplateDialog } from './SensitivityTemplatePage.js';
import { SensitivityTemplatesPage, AddTemplateDialog } from './SensitivityTemplatesPage.js';

/**
 * The sensitivity template screens, rendered from a seeded query cache.
 *
 * What is checked is what the screens promise: the list names the four columns
 * and opens a key's page, the page says what the key does and does not decide,
 * the form draws the asset class and the method as the two selects the table
 * allows and states the token the chosen method implies, and the history reads
 * the key's versions as one timeline.
 */

const CHOICES: SensitivityTemplateChoices = {
    assetClasses: [
        { code: 'interest_rates', prefix: 'IR', name: 'Interest rate' },
        { code: 'fx', prefix: 'FX', name: 'Foreign exchange' },
        { code: 'equity', prefix: 'EQ', name: 'Equity' },
        { code: 'commodity', prefix: 'COMM', name: 'Commodity' },
        { code: 'credit', prefix: 'CR', name: 'Credit' },
    ],
    methods: [
        { code: 'Analytical', name: 'Analytical', needsAssetClass: true },
        { code: 'Semianalytical', name: 'Semianalytical', needsAssetClass: true },
        { code: 'FD', name: 'Finite difference', needsAssetClass: true },
        { code: 'MC', name: 'Monte Carlo', needsAssetClass: true },
        { code: 'Trade', name: 'Trade level', needsAssetClass: false },
    ],
};

function template(
    code: string,
    assetClassCode: string,
    assetClassName: string,
    templatePrefix: string,
    methodCode: string,
    methodName: string,
) {
    return {
        code,
        methodCode,
        methodName,
        assetClassCode,
        assetClassName,
        templatePrefix,
        description: `${code} explained`,
        version: 2,
        modifiedBy: 'priya',
        performedBy: 'ores_analytics_service',
        reasonCode: 'common.rectification',
        commentary: 'Tidy up',
        recordedAt: '2026-10-04 09:05:00.000000Z',
    };
}

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

function version(n: number, description: string, reason: string): HistoryVersion {
    return {
        version: n,
        modifiedBy: 'priya',
        recordedAt: `2026-10-0${String(n)} 09:00:00Z`,
        fields: [
            field('Code', 'IR_FD'),
            field('Asset Class', 'IR'),
            field('Method', 'Finite difference'),
            field('Method Code', 'FD'),
            field('Description', description),
            field('Modified By', 'priya'),
            field('Performed By', 'ores_analytics_service'),
            field('Change Reason Code', reason),
            field('Change Commentary', n === 2 ? 'Grid keys' : ''),
            field('Recorded At', `2026-10-0${String(n)} 09:00`),
        ],
        changes: [],
    };
}

/** Seeds the first page of the list, as the shared list reads it. */
function seedPage(client: QueryClient, rows: readonly SensitivityTemplate[]): void {
    client.setQueryData(pageKey({ key: 'sensitivity-templates' }, FIRST_PAGE), {
        rows,
        total: rows.length,
    });
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['sensitivity-templates', 'choices'], CHOICES);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route
                            path="/reporting/sensitivity-templates"
                            element={<SensitivityTemplatesPage />}
                        />
                        <Route
                            path="/reporting/sensitivity-templates/:code"
                            element={<SensitivityTemplatePage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

/** Renders a dialog on its own, with the cache it reads. */
function renderDialog(dialog: React.ReactNode, seed?: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['sensitivity-templates', 'choices'], CHOICES);
    seed?.(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>{dialog}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the sensitivity template list', () => {
    const rows = [
        template('IR_FD', 'interest_rates', 'Interest rate', 'IR', 'FD', 'Finite difference'),
        template('EQ_MC', 'equity', 'Equity', 'EQ', 'MC', 'Monte Carlo'),
        template('SensiTrade', '', '', '', 'Trade', 'Trade level'),
    ];

    it('names the four columns and finds every key through the server', () => {
        const html = render('/reporting/sensitivity-templates', (client) => {
            seedPage(client, rows);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('Sensitivity templates');
        expect(textOf(html)).toContain('Code');
        expect(textOf(html)).toContain('Asset class');
        expect(textOf(html)).toContain('Method');
        expect(textOf(html)).toContain('Description');
        expect(html).toContain('IR_FD');
        expect(html).toContain('Finite difference');
        expect(html).toContain('EQ_MC explained');
        expect(html).toContain('Search…');
        expect(html).toContain('1–3 of 3');
    });

    it('offers a writer the New template action', () => {
        const html = render('/reporting/sensitivity-templates', (client) => {
            seedPage(client, rows);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('New template');
        expect(html).not.toContain('Changing a template needs the permission');
    });

    it('offers a reader no change, and says why', () => {
        const html = render('/reporting/sensitivity-templates', (client) => {
            seedPage(client, rows);
            client.setQueryData(['my-access'], access(['analytics::sensitivity_templates:read']));
        });
        expect(html).not.toContain('New template');
        expect(html).toContain('Changing a template needs the permission');
    });
});

describe('one sensitivity template', () => {
    const row = template(
        'IR_FD',
        'interest_rates',
        'Interest rate',
        'IR',
        'FD',
        'Finite difference',
    );

    it('opens on its details, which say what it decides and what it does not', () => {
        const html = render('/reporting/sensitivity-templates/IR_FD', (client) => {
            client.setQueryData(['sensitivity-templates', 'key', 'IR_FD'], row);
            client.setQueryData(['my-access'], access(['*']));
        });
        expect(html).toContain('>IR_FD<');
        expect(html).toContain('IR_FD · version 2');
        expect(html).toContain('Editable');
        expect(html).toContain('>Edit<');
        expect(html).toContain('>Delete<');
        expect(html).toContain('aria-selected="true"');
        expect(html).toContain('IR');
        expect(html).toContain('Finite difference');
        expect(html).toContain('FD');
        expect(html).toContain('IR_FD explained');
        expect(textOf(html)).toContain(
            'It names a key a pricing engine configuration puts on a product. The class is the class of that product, and the shift sizes the key applies are set per market key in the sensitivity configuration, which may set them on market keys outside that class.',
        );
        expect(html).toContain('Last changed');
        expect(html).toContain('common.rectification');
        expect(html).toContain('“Tidy up”');
    });

    it('offers a reader the details and no action', () => {
        const html = render('/reporting/sensitivity-templates/IR_FD', (client) => {
            client.setQueryData(['sensitivity-templates', 'key', 'IR_FD'], row);
            client.setQueryData(['my-access'], access(['analytics::sensitivity_templates:read']));
        });
        expect(html).not.toContain('>Edit<');
        expect(html).not.toContain('>Delete<');
        expect(html).toContain('Read only');
    });

    it('reads the key history as one timeline, with what each version says', () => {
        const html = render('/reporting/sensitivity-templates/IR_FD?tab=history', (client) => {
            client.setQueryData(['sensitivity-templates', 'key', 'IR_FD'], row);
            client.setQueryData(['my-access'], access(['*']));
            client.setQueryData(
                ['history', ENTITY_TYPE, 'IR_FD'],
                [
                    version(2, 'IR_FD explained', 'common.rectification'),
                    version(1, '', 'system.new_record'),
                ],
            );
        });
        expect(textOf(html)).toContain('Grid keys');
        expect(textOf(html)).toContain('common.rectification');
        expect(textOf(html)).toContain('system.new_record');
    });
});

describe('the form that changes a template', () => {
    const row = template(
        'IR_FD',
        'interest_rates',
        'Interest rate',
        'IR',
        'FD',
        'Finite difference',
    );

    it('draws the method and the asset class as selects of what the tables hold', () => {
        const html = renderDialog(<EditTemplateDialog template={row} onClose={() => undefined} />);
        expect(html).toContain('Edit IR_FD');
        for (const assetClass of CHOICES.assetClasses) {
            expect(html).toContain(`<option value="${assetClass.code}"`);
        }
        for (const method of CHOICES.methods) {
            expect(html).toContain(`<option value="${method.code}"`);
        }
        // The two selects stand on the codes the row states, and show the words
        // the lookups read for them.
        expect(html).toContain('<option value="interest_rates" selected="">');
        expect(html).toContain('<option value="FD" selected="">');
        expect(html).toContain('Interest rate');
        expect(html).toContain('Finite difference');
    });

    it('states the method code the chosen method spells, and never asks for it', () => {
        const html = renderDialog(<EditTemplateDialog template={row} onClose={() => undefined} />);
        expect(textOf(html)).toContain('Method code');
        expect(html).toContain('FD');
        expect(html).toContain('The token ORE writes in a key, which the method states.');
        // Neither the key nor its token is a field: the form's selects are the
        // method, the asset class and the change reason, and its two inputs the
        // description and the commentary (RS-WRITE-01, RS-WRITE-07).
        expect(html.match(/<select/g) ?? []).toHaveLength(3);
        expect(html.match(/<input/g) ?? []).toHaveLength(2);
    });

    it('holds the key fixed, so a correction cannot rename it', () => {
        const html = renderDialog(<EditTemplateDialog template={row} onClose={() => undefined} />);
        expect(textOf(html)).toContain('Set when the record was made, and never changed here.');
        expect(html).toContain('>IR_FD</p>');
    });

    it('adds a key by stating it once, with the method and the class it names', () => {
        const html = renderDialog(<AddTemplateDialog onClose={() => undefined} />);
        expect(html).toContain('Add a sensitivity template');
        expect(textOf(html)).toContain('The key ORE reads, such as IR_FD.');
        // The method a new template opens on, and the token it spells.
        expect(html).toContain('<option value="Analytical" selected="">');
        expect(textOf(html)).toContain('Analytical');
        expect(textOf(html)).toContain('A new template is recorded as a new record');
    });

    it('names no class for a trade-level template, and holds the class closed', () => {
        const trade = template('SensiTrade', '', '', '', 'Trade', 'Trade level');
        const html = renderDialog(
            <EditTemplateDialog template={trade} onClose={() => undefined} />,
        );
        // The select stands closed on the words that say there is no class, and
        // the form says why rather than leaving the person to guess.
        expect(html).toContain(
            'A trade-level template names no asset class, because the trade states its own sensitivities.',
        );
        expect(html).toMatch(/<select[^>]*disabled=""/);
        expect(html).toContain('>None</option>');
        expect(html).toContain('<option value="" selected="">');
    });
});
