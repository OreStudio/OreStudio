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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { useReason } from '../refdata/shared.js';
import {
    OreImportHoldsBody,
    OreImportPage,
    OreImportReviewBody,
    OreImportTargetBody,
    OreImportUnknownBody,
    type OreImportKind,
    type OreImportPreview,
} from './OreExchange.js';

/**
 * The Import steps, drawn from what the server answers.
 *
 * The web package's tests hold no browser, so a step that follows a press is
 * drawn here from the state the press would have produced: the preview it
 * holds, the target the person chose, and the choices the review starts from.
 */

const REASONS = [
    {
        code: 'common.correction',
        description: 'Correction',
        requiresCommentary: false,
    },
];

function draw(node: ReactNode, seed: (client: QueryClient) => void = () => undefined): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['reference-data-reasons', 'amend'], REASONS);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/import']}>
                    <Routes>
                        <Route path="/import" element={<>{node}</>} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const PREVIEW: OreImportPreview = {
    success: true,
    message: 'The file holds 2 products and 3 settings.',
    targetName: 'EOD pricing',
    holds: {
        counts: [
            { id: 'products', label: 'Products in the file', value: 2 },
            { id: 'settings', label: 'Settings in the file', value: 3 },
        ],
        lines: [
            'Swap: model DiscountedCashflows, engine DiscountingSwapEngine, 2 setting(s)',
            'Configuration setting: ContinueOnCalibrationError',
        ],
        note: 'A product is written with its model, its engine and the settings each takes.',
    },
    rows: [
        {
            key: 'Swap#0',
            change: 'added',
            name: 'Swap#0',
            what: 'Model DiscountedCashflows, engine DiscountingSwapEngine',
        },
        {
            key: 'Swap#0/engine/DiscountingCurve',
            change: 'removed',
            name: 'Swap#0 · DiscountingCurve',
            what: 'Not in the file',
        },
    ],
    issues: [
        {
            kind: 'unknown_type',
            subject: 'CommodityForward',
            detail: 'The vocabulary does not hold it.',
        },
        { kind: 'duplicate', subject: 'Swap#0/engine/Calibration', detail: 'Stated twice.' },
    ],
    summary: '1 added, 0 changed, 1 removed, 0 already held.',
};

const KIND: OreImportKind = {
    words: 'pricingEngines',
    area: 'reporting',
    targetKind: 'configuration',
    rootElement: 'PricingEngines',
    listPath: '/reporting/pricing-engines',
    choicesFor: (row) =>
        row.change === 'removed'
            ? [
                  { id: 'keep', label: 'Keep ours' },
                  { id: 'delete', label: 'Delete it' },
              ]
            : [],
    preview: async () => PREVIEW,
    write: async () => ({ success: true, message: '', errors: [], summary: '' }),
};

const TABLE_KIND: OreImportKind = { ...KIND, targetKind: 'table' };

function Review(): ReactNode {
    const reason = useReason('amend');
    return (
        <OreImportReviewBody
            kind={KIND}
            preview={PREVIEW}
            choices={{}}
            onChoice={() => undefined}
            existing={false}
            newName="From ORE"
            reason={reason}
            commentary=""
            onCommentary={() => undefined}
            missing={false}
        />
    );
}

describe('the import steps', () => {
    it('draws five steps for a kind with configurations to choose between', () => {
        const text = draw(<OreImportPage kind={KIND} />);
        for (const step of [
            'Choose the file',
            'What the file holds',
            'What the system does not know',
            'Where it goes',
            'The difference, and the review',
        ]) {
            expect(text).toContain(step);
        }
        expect(text).toContain('An ORE document whose root element is PricingEngines');
    });

    it('draws four steps for a kind whose document is the whole table', () => {
        const text = draw(<OreImportPage kind={TABLE_KIND} />);
        expect(text).toContain('What the system does not know');
        expect(text).not.toContain('Where it goes');
    });

    it('opens on the file and writes nothing before the review', () => {
        const text = draw(<OreImportPage kind={KIND} />);
        expect(text).toContain('type="file"');
        expect(text).not.toContain('Keep ours');
        expect(text).not.toContain('Correction');
    });
});

describe('the step that reads what the file holds', () => {
    it('states the counts, the products and the root-level settings', () => {
        const text = draw(<OreImportHoldsBody holds={PREVIEW.holds} />);
        expect(text).toContain('Products in the file');
        expect(text).toContain('2');
        expect(text).toContain('Settings in the file');
        expect(text).toContain('Swap: model DiscountedCashflows');
        expect(text).toContain('Configuration setting: ContinueOnCalibrationError');
    });
});

describe('the step that reads what the system does not know', () => {
    it('names each kind of unknown, one list for each', () => {
        const text = draw(<OreImportUnknownBody issues={PREVIEW.issues} />);
        expect(text).toContain('Product types the vocabulary does not hold');
        expect(text).toContain('CommodityForward');
        expect(text).toContain('Duplicates');
        expect(text).toContain('Stated twice.');
        expect(text).not.toContain('Elements with no column');
    });

    it('says when everything in the file is known', () => {
        const text = draw(<OreImportUnknownBody issues={[]} nothingUnknown="Nothing unknown." />);
        expect(text).toContain('Nothing unknown.');
    });
});

describe('the step that chooses the target', () => {
    it('offers a new configuration, and the configurations the person holds', () => {
        const text = draw(
            <OreImportTargetBody
                targets={[{ id: 'c-1', name: 'EOD pricing', detail: 'eod · 2 products' }]}
                existing
                onExisting={() => undefined}
                targetId="c-1"
                onTargetId={() => undefined}
                name=""
                onName={() => undefined}
                variant=""
                onVariant={() => undefined}
            />,
        );
        expect(text).toContain('A new configuration');
        expect(text).toContain('A configuration I hold');
        expect(text).toContain('EOD pricing · eod · 2 products');
        expect(text).toContain('Choose a configuration');
    });

    it('states that the import writes directly, with the reason and the commentary', () => {
        const text = draw(
            <OreImportTargetBody
                targets={[]}
                existing={false}
                onExisting={() => undefined}
                targetId=""
                onTargetId={() => undefined}
                name=""
                onName={() => undefined}
                variant=""
                onVariant={() => undefined}
            />,
        );
        expect(text).toContain('An official configuration is a proposal in the standard');
        expect(text).toContain('writes the change directly, with your reason and commentary');
    });
});

describe('the step that reviews the difference', () => {
    it('puts the removed rows first, with Keep it as the choice it starts from', () => {
        const html = draw(<Review />);
        const text = draw(<Review />);
        expect(text.indexOf('Removed')).toBeLessThan(text.indexOf('Added'));
        expect(text).toContain('Keep ours');
        expect(text).toContain('Delete it');
        expect(html).toMatch(
            /name="choice-Swap#0\/engine\/DiscountingCurve"[^>]*checked|checked[^>]*name="choice-Swap#0\/engine\/DiscountingCurve"/,
        );
    });

    it('offers no choice for a row the file adds, because the file decides it', () => {
        const text = draw(<Review />);
        expect(text).not.toContain('name="choice-Swap#0"');
    });

    it('states the difference, the reach of the write, and where the file goes', () => {
        const text = draw(<Review />);
        expect(text).toContain('1 added, 0 changed, 1 removed, 0 already held.');
        expect(text).toContain('The records written are read by everything that names them');
        expect(text).toContain('Creates the configuration From ORE');
    });
});
