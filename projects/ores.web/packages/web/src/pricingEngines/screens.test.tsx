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
import { ModelEnginePanels, SettingValue } from './parts.js';
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
        partyId: 'p-london',
        partyName: 'ACME Corporation UK',
        partyCode: 'UK',
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
        partyId: 'p-newyork',
        partyName: 'ACME Corporation US',
        partyCode: 'US',
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
    partyId: 'p-london',
    partyName: 'ACME Corporation UK',
    partyCode: 'UK',
    version: 3,
    name: 'EOD pricing',
    description: 'The end of day set',
    variant: 'eod',
    configurationId: '99999999-9999-4999-8999-999999999999',
    modifiedBy: 'quant',
    recordedAt: '2026-10-19 10:00:00.000000Z',
    changeReason: 'common.correction',
    changeCommentary: '',
    reports: [{ id: 'report-eod', name: 'EOD risk' }],
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
        {
            id: 'parameter-global',
            version: 1,
            productId: null,
            scope: 'global',
            name: 'Calendar',
            value: 'TARGET',
            position: 1,
        },
    ],
};

const VOCABULARY: PricingVocabulary = {
    globals: [],
    types: [
        {
            code: 'BermudanSwaption',
            description: 'A swap exercisable on several dates.',
            instrumentTypeCode: 'SWPTN',
            assetClass: 'interest_rates',
        },
    ],
    pairs: [
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            usual: true,
            help: 'Builds a numeric multi-leg option engine.',
            description: 'Builds a numeric multi-leg option engine.',
        },
    ],
    settings: [
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            scope: 'engine',
            name: 'SensitivityTemplate',
            kind: 'reference',
            usual: 'IR_FD',
            options: [],
            required: false,
            help: 'The sensitivity template of the product.',
            description: 'The sensitivity template of the product.',
        },
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            scope: 'engine',
            name: 'TimestepPeriod',
            kind: 'tenor',
            usual: '3M',
            options: [],
            required: true,
            help: 'The step of the grid.',
            description: 'The step of the grid.',
        },
    ],
    models: [
        {
            code: 'LGM',
            description: 'Linear Gauss Markov model',
            detail: 'A one-factor Gaussian rates model.',
            source: 'ore_guide',
        },
    ],
    methods: [
        {
            code: 'Grid',
            description: 'Convolution grid engine',
            detail: 'Rolls the value back on a grid.',
            source: 'authored',
        },
    ],
    templates: [
        {
            code: 'IR_FD',
            assetClass: 'IR',
            method: 'Finite difference',
            description: 'Interest rate products priced on a grid.',
        },
        {
            code: 'IR_Analytical',
            assetClass: 'IR',
            method: 'Analytical',
            description: 'Interest rate products priced by a formula.',
        },
    ],
    tenors: [
        { code: '3M', displayName: '3 months' },
        { code: '6M', displayName: '6 months' },
    ],
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

function record(
    codes: string[],
    vocabulary: PricingVocabulary | null = VOCABULARY,
    tab?: string,
): string {
    return renderRecords(
        `/reporting/pricing-engines/${EOD}${tab === undefined ? '' : `?tab=${tab}`}`,
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

    it('searches the party as well as the configuration', () => {
        expect(
            pageOf(ROWS, { ...page, search: 'corporation us' }).rows.map((row) => row.name),
        ).toEqual(['Intraday pricing']);
        expect(pageOf(ROWS, { ...page, search: 'UK' }).rows.map((row) => row.name)).toEqual([
            'EOD pricing',
        ]);
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

    it('names the party that holds each configuration', () => {
        const text = textOf(list(READ));
        expect(text).toContain('Party');
        expect(text).toContain('ACME Corporation UK');
        expect(text).toContain('ACME Corporation US');
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
    it('states its details', () => {
        const text = textOf(record(READ));
        expect(text).toContain('EOD pricing');
        expect(text).toContain('ACME Corporation UK');
        expect(text).toContain('Details');
        expect(text).toContain('Global parameters');
        expect(text).toContain('Products');
        expect(text).toContain('Reports');
        expect(text).toContain('History');
    });

    it('draws the product under the model and the engine that price it', () => {
        const text = textOf(record(READ, VOCABULARY, 'products'));
        expect(text).toContain('BermudanSwaption');
        expect(text).toContain('LGM');
        expect(text).toContain('Grid');
        expect(text).toContain('Calibration');
        expect(text).toContain('Bootstrap');
        expect(text).toContain('Interest Rates');
    });

    it('states the reach of a change', () => {
        const text = textOf(record(READ, VOCABULARY, 'reports'));
        expect(text).toContain('EOD risk');
        expect(text).toContain('A report needs a price');
    });

    it('states the global parameters of the document', () => {
        const text = textOf(record(READ, VOCABULARY, 'globals'));
        expect(text).toContain('Global parameters');
        expect(text).toContain('Calendar');
        expect(text).toContain('TARGET');
    });

    it('offers Change, Remove and Add product to a person who may write', () => {
        const text = textOf(record(WRITE, VOCABULARY, 'products'));
        expect(text).toContain('Change');
        expect(text).toContain('Remove');
        expect(text).toContain('Add product');
    });

    it('offers no change to a person who may only read', () => {
        const text = textOf(record(READ, VOCABULARY, 'products'));
        expect(text).not.toContain('Add product');
        expect(text).not.toContain('Remove');
    });

    it('offers no product change until the models and engines are read', () => {
        expect(textOf(record(WRITE, null, 'products'))).not.toContain('Add product');
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

describe('the model and engine panels', () => {
    function panels(model: string, engine: string): string {
        return renderRecords(
            '/panels',
            [],
            <Route
                path="/panels"
                element={
                    <ModelEnginePanels
                        vocabulary={VOCABULARY}
                        typeCode="BermudanSwaption"
                        model={model}
                        engine={engine}
                        models={['LGM']}
                        engines={['Grid']}
                        onModel={() => undefined}
                        onEngine={() => undefined}
                    />
                }
            />,
            () => undefined,
        );
    }

    it('states what the model is beside the model and what the engine is beside the engine', () => {
        const text = textOf(panels('LGM', 'Grid'));
        expect(text).toContain('Linear Gauss Markov model');
        expect(text).toContain('A one-factor Gaussian rates model.');
        expect(text).toContain('Convolution grid engine');
        expect(text).toContain('Rolls the value back on a grid.');
    });

    it('states where the words come from, and flags the ones written for review', () => {
        const text = textOf(panels('LGM', 'Grid'));
        expect(text).toContain('From the ORE guide');
        expect(text).toContain('Written for ORE Studio, to review');
    });

    it('states how ORE builds the engine of the pair for the product type', () => {
        const text = textOf(panels('LGM', 'Grid'));
        expect(text).toContain('How ORE builds the engine for BermudanSwaption:');
        expect(text).toContain('Builds a numeric multi-leg option engine.');
    });

    it('says so when no description is held', () => {
        expect(textOf(panels('Other', 'Grid'))).toContain(
            'No description is held for this choice.',
        );
    });
});

describe('the value of a setting', () => {
    function setting(name: string, value: string): string {
        return renderRecords(
            '/setting',
            [],
            <Route
                path="/setting"
                element={
                    <SettingValue
                        vocabulary={VOCABULARY}
                        typeCode="BermudanSwaption"
                        model="LGM"
                        engine="Grid"
                        name={name}
                        scope="engine"
                        value={value}
                        onChange={() => undefined}
                    />
                }
            />,
            () => undefined,
        );
    }

    it('offers the tenors for a setting of the tenor kind, and marks a required one', () => {
        const html = setting('TimestepPeriod', '3M');
        expect(html).toContain('<option value="3M"');
        expect(html).toContain('<option value="6M"');
        expect(textOf(html)).toContain('required');
        expect(textOf(html)).toContain('The step of the grid.');
    });

    it('offers the sensitivity templates and states the one chosen', () => {
        const html = setting('SensitivityTemplate', 'IR_FD');
        expect(html).toContain('<option value="IR_Analytical"');
        expect(textOf(html)).toContain(
            'IR · Finite difference. Interest rate products priced on a grid.',
        );
        expect(textOf(html)).not.toContain('required');
    });
});
