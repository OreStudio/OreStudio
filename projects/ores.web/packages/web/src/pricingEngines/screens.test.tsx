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
import type { QueryClient } from '@tanstack/react-query';
import { Route } from 'react-router';
import type { PricingDocument, PricingSummary, PricingVocabulary } from '../api/pricingEngines.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { accessOf, renderRecords } from '../refdata/testSupport.js';
import { textOf } from '../textOf.js';
import { PricingEnginePage, VOCABULARY_KEY, pricingEngineKey } from './PricingEnginePage.js';
import { PricingEngineJourney, HELD_KEY } from './PricingEngineJourney.js';
import { PricingEnginesPage, pageOf } from './PricingEnginesPage.js';

/**
 * The pricing engine screens, rendered from a seeded query cache.
 *
 * They read what a person reads: the cells of a row, the words a state states
 * and the actions a person may take. The dialogs and their checks are the
 * model's, and the model's own tests cover them.
 */

const EOD = '55555555-5555-4555-8555-555555555555';
const INTRADAY = '66666666-6666-4666-8666-666666666666';
const PRODUCT = '77777777-7777-4777-8777-777777777777';
const PARAMETER = '88888888-8888-4888-8888-888888888888';

const ROWS: readonly PricingSummary[] = [
    {
        id: EOD,
        version: 3,
        name: 'EOD pricing',
        description: 'The end of day set',
        variant: 'eod',
        configurationId: '99999999-9999-4999-8999-999999999999',
        products: 2,
        parameters: 7,
        modifiedBy: 'quant',
        recordedAt: '2026-10-19 10:00:00.000000Z',
    },
    {
        id: INTRADAY,
        version: 1,
        name: 'Intraday pricing',
        description: '',
        variant: '',
        configurationId: '99999999-9999-4999-8999-999999999998',
        products: 1,
        parameters: 0,
        modifiedBy: 'quant',
        recordedAt: '2026-10-18 10:00:00.000000Z',
    },
];

const DOCUMENT: PricingDocument = {
    id: EOD,
    version: 3,
    name: 'EOD pricing',
    description: 'The end of day set',
    variant: 'eod',
    configurationId: '99999999-9999-4999-8999-999999999999',
    modifiedBy: 'quant',
    recordedAt: '2026-10-19 10:00:00.000000Z',
    changeReason: 'common.correction',
    changeCommentary: '',
    products: [
        {
            id: PRODUCT,
            version: 1,
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            position: 0,
        },
    ],
    parameters: [
        {
            id: PARAMETER,
            version: 1,
            productId: PRODUCT,
            scope: 'model',
            name: 'Calibration',
            value: 'Bootstrap',
            position: 0,
        },
    ],
};

const VOCABULARY: PricingVocabulary = {
    types: [{ code: 'BermudanSwaption', description: '', instrumentTypeCode: 'SWPTN' }],
    pairs: [{ typeCode: 'BermudanSwaption', model: 'LGM', engine: 'Grid', usual: true, note: '' }],
    settings: [],
};

const WRITE = ['analytics::pricing_model_configs:read', 'analytics::pricing_model_configs:write'];
const READ = ['analytics::pricing_model_configs:read'];

function list(codes: string[]): string {
    return renderRecords(
        '/reporting/pricing-engines',
        [],
        <Route path="/reporting/pricing-engines" element={<PricingEnginesPage />} />,
        (client: QueryClient) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(pageKey({ key: 'pricing-engines' }, FIRST_PAGE), {
                rows: ROWS,
                total: ROWS.length,
            });
        },
    );
}

function record(codes: string[], vocabulary: PricingVocabulary | null = VOCABULARY): string {
    return renderRecords(
        `/reporting/pricing-engines/${EOD}`,
        [],
        <Route path="/reporting/pricing-engines/:id" element={<PricingEnginePage />} />,
        (client: QueryClient) => {
            client.setQueryData(['my-access'], accessOf(codes));
            client.setQueryData(pricingEngineKey(EOD), DOCUMENT);
            if (vocabulary !== null) {
                client.setQueryData(VOCABULARY_KEY, vocabulary);
            }
        },
    );
}

describe('pageOf', () => {
    const page = { offset: 0, limit: 15, sort: 'name', descending: false, search: '' };

    it('orders by name and pages the rows', () => {
        expect(pageOf(ROWS, page).rows.map((row) => row.name)).toEqual([
            'EOD pricing',
            'Intraday pricing',
        ]);
        expect(pageOf(ROWS, { ...page, limit: 1, offset: 1 }).rows.map((row) => row.name)).toEqual([
            'Intraday pricing',
        ]);
    });

    it('orders a number as a number, either way', () => {
        expect(pageOf(ROWS, { ...page, sort: 'parameters', descending: true }).rows[0]?.name).toBe(
            'EOD pricing',
        );
    });

    it('searches the name, the description and the variant, and counts what it found', () => {
        expect(pageOf(ROWS, { ...page, search: 'end of day' })).toMatchObject({ total: 1 });
        expect(pageOf(ROWS, { ...page, search: 'EOD' }).rows.map((row) => row.name)).toEqual([
            'EOD pricing',
        ]);
        expect(pageOf(ROWS, { ...page, search: 'nothing' })).toMatchObject({ total: 0, rows: [] });
    });
});

describe('the list of pricing engine configurations', () => {
    it('states each configuration with its variant and its counts', () => {
        const text = textOf(list(READ));
        expect(text).toContain('EOD pricing');
        expect(text).toContain('Intraday pricing');
        expect(text).toContain('No variant');
    });

    it('opens each row by the id of the configuration', () => {
        const html = list(READ);
        expect(html).toContain(`href="/reporting/pricing-engines/${EOD}"`);
        expect(html).toContain(`href="/reporting/pricing-engines/${INTRADAY}"`);
    });

    it('lists the workflows only to a person who may write', () => {
        expect(textOf(list(WRITE))).toContain('Define a pricing engine configuration');
        expect(textOf(list(READ))).not.toContain('Define a pricing engine configuration');
    });
});

describe('one pricing engine configuration', () => {
    it('states its details, its product and the settings of that product', () => {
        const text = textOf(record(READ));
        expect(text).toContain('EOD pricing');
        expect(text).toContain('BermudanSwaption');
        expect(text).toContain('LGM / Grid');
        expect(text).toContain('Calibration');
        expect(text).toContain('Bootstrap');
    });

    it('says the reports that use it cannot be listed yet', () => {
        expect(textOf(record(READ))).toContain('cannot be listed yet');
    });

    it('offers Change, Remove and Add product to a person who may write', () => {
        const text = textOf(record(WRITE));
        expect(text).toContain('Change');
        expect(text).toContain('Remove');
        expect(text).toContain('Add product');
    });

    it('offers no change to a person who may only read', () => {
        const text = textOf(record(READ));
        expect(text).not.toContain('Add product');
        expect(text).not.toContain('Remove');
    });

    it('offers no product change until the models and engines are read', () => {
        expect(textOf(record(WRITE, null))).not.toContain('Add product');
    });
});

describe('the authoring journey', () => {
    function journey(rows: readonly PricingSummary[]): string {
        return renderRecords(
            '/reporting/pricing-engines/author',
            [],
            <Route path="/reporting/pricing-engines/author" element={<PricingEngineJourney />} />,
            (client: QueryClient) => {
                client.setQueryData(['my-access'], accessOf(WRITE));
                client.setQueryData(VOCABULARY_KEY, VOCABULARY);
                client.setQueryData(HELD_KEY, rows);
            },
        );
    }

    it('lists the six steps on its rail and opens on the first', () => {
        const text = textOf(journey(ROWS));
        for (const step of [
            'Name it',
            'Choose the products',
            'Choose model and engine',
            'Set the parameters',
            'Check it',
            'Review and save',
        ]) {
            expect(text).toContain(step);
        }
        expect(text).toContain('Name the configuration');
    });

    it('offers to start from a copy of each configuration the party holds', () => {
        const text = textOf(journey(ROWS));
        expect(text).toContain('A copy of EOD pricing');
        expect(text).toContain('A copy of Intraday pricing');
        expect(textOf(journey([]))).not.toContain('Start from');
    });

    it('cannot move on before the configuration has a name', () => {
        expect(journey(ROWS)).toMatch(/<button[^>]*disabled[^>]*>\s*Next\s*<\/button>/);
    });
});
