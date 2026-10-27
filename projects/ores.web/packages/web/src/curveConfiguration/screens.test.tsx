/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import type { QueryClient } from '@tanstack/react-query';
import { Route } from 'react-router';
import type { CurveConfiguration } from '../api/curveConfiguration.js';
import { renderRecords } from '../refdata/testSupport.js';
import { textOf } from '../textOf.js';
import { CurveConfigurationPage, curveConfigurationKey } from './CurveConfigurationPage.js';
import { configurationKey } from '../reporting/ConfigurationPage.js';

/**
 * The curve configuration page, rendered from a seeded query cache.
 *
 * The test reads what a person reads: the words of a tab, the values of a leaf,
 * the gap of a leaf the contract does not document, and the report the
 * configuration reaches.
 */

const CONFIGURATION_ID = '11111111-1111-4111-8111-111111111111';
const CURVE_ID = '22222222-2222-4222-8222-222222222222';
const ENTRY_ID = '33333333-3333-4333-8333-333333333333';
const REPORT_ID = '44444444-4444-4444-8444-444444444444';

const leaf = (
    ore: string,
    xpath: string,
    overrides: Partial<
        CurveConfiguration['sections'][number]['entries'][number]['parameters'][number]
    > = {},
): CurveConfiguration['sections'][number]['entries'][number]['parameters'][number] => ({
    entity: 'yield_curve_config',
    column: ore.toLowerCase(),
    ore,
    xpath,
    value: null,
    documented: true,
    required: false,
    label: ore,
    help: '',
    description: `${ore} of the curve.`,
    defaultText: 'None.',
    format: 'Text.',
    introducedIn: '1.8.0.7 or earlier',
    removedIn: '',
    valueSet: '',
    sourceKind: '',
    choices: [],
    minimum: null,
    maximum: null,
    pattern: '',
    ...overrides,
});

const CURVE: CurveConfiguration = {
    configurationId: CONFIGURATION_ID,
    id: CURVE_ID,
    name: 'CurveConfiguration',
    description: 'Imported from ORE XML',
    version: 3,
    partyId: 'party-hk',
    modifiedBy: 'analyst',
    recordedAt: '2026-10-27 09:00:00.000000Z',
    documented: 2,
    total: 5,
    sections: [
        {
            code: 'YieldCurves',
            entryElement: 'YieldCurve',
            description: 'Interest rate curves.',
            mapped: true,
            entries: [
                {
                    id: ENTRY_ID,
                    curveId: 'USD-6M',
                    description: 'USD discount curve',
                    version: 2,
                    position: 0,
                    parameters: [
                        leaf('CurveId', '/CurveConfiguration/YieldCurves/YieldCurve/CurveId', {
                            entity: 'curve_definition',
                            column: 'curve_id',
                            value: 'USD-6M',
                            documented: true,
                            label: 'Curve id',
                        }),
                        leaf(
                            'CurveDescription',
                            '/CurveConfiguration/YieldCurves/YieldCurve/CurveDescription',
                            {
                                entity: 'curve_definition',
                                column: 'description',
                                value: 'USD discount curve',
                                documented: true,
                                label: 'Curve description',
                            },
                        ),
                        leaf('Currency', '/CurveConfiguration/YieldCurves/YieldCurve/Currency', {
                            value: 'USD',
                            required: true,
                            label: 'Currency',
                        }),
                        leaf(
                            'InterpolationMethod',
                            '/CurveConfiguration/YieldCurves/YieldCurve/InterpolationMethod',
                            {
                                value: 'Linear',
                                label: 'Interpolation method',
                                help: 'How the curve interpolates between its pillars.',
                                valueSet: 'curve_interpolation_method',
                                sourceKind: 'enumerated',
                                choices: [
                                    { value: 'Linear', label: 'Linear' },
                                    { value: 'Cubic', label: 'Cubic' },
                                ],
                            },
                        ),
                        leaf('Tolerance', '/CurveConfiguration/YieldCurves/YieldCurve/Tolerance', {
                            value: '0.0001',
                            documented: false,
                            label: '',
                            description: '',
                        }),
                        leaf(
                            'IncludeAtm',
                            '/CurveConfiguration/CapFloorVolatilities/CapFloorVolatility/IncludeAtm',
                            {
                                value: 'true',
                                sourceKind: 'boolean',
                                label: 'Include at the money',
                            },
                        ),
                    ],
                    blocks: [
                        { element: 'Report', present: true },
                        { element: 'Segments', present: null },
                    ],
                },
            ],
        },
        {
            code: 'EquityCurves',
            entryElement: 'EquityCurve',
            description: 'Equity forward curves.',
            mapped: true,
            entries: [],
        },
        {
            code: 'CurveSettings',
            entryElement: 'CurveSetting',
            description: 'A family the screen does not map yet.',
            mapped: false,
            entries: [],
        },
    ],
};

/** The report configuration the curve document fills, as the configuration page reads it. */
const SERVING = {
    configuration: {
        id: CONFIGURATION_ID,
        name: 'HK standard curves',
        typeCode: 'curve_configuration',
        typeName: 'Curve configuration',
        owningComponent: 'ores.refdata',
        partyId: 'party-hk',
        partyName: 'ACME Corporation HK',
        version: 3,
        modified_by: 'analyst',
        performed_by: 'analyst',
        change_reason_code: 'common.correction',
        change_commentary: '',
        recorded_at: '2026-10-27 09:00:00.000000Z',
    },
    analytics: [],
    reports: [
        {
            id: REPORT_ID,
            name: 'HK daily VaR',
            partyName: 'ACME Corporation HK',
            regulatoryBookType: null,
        },
    ],
};

function page(tab = '', curve: CurveConfiguration = CURVE): string {
    const address = `/reporting/curve-configurations/${CONFIGURATION_ID}${
        tab === '' ? '' : `?tab=${tab}`
    }`;
    return renderRecords(
        address,
        [],
        <Route path="/reporting/curve-configurations/:id" element={<CurveConfigurationPage />} />,
        (client: QueryClient) => {
            client.setQueryData(curveConfigurationKey(CONFIGURATION_ID), curve);
            client.setQueryData(configurationKey(CONFIGURATION_ID), SERVING);
        },
    );
}

describe('the curve configuration page', () => {
    it('draws Details first and one Curves tab for the whole document, then Used by and History', () => {
        const html = page();
        const tabs = [...html.matchAll(/role="tab"[^>]*>([^<]*)</g)].map((match) => match[1]);
        expect(tabs).toEqual(['Details', 'Curves', 'Used by', 'History']);
    });

    it('names the configuration by the report configuration it fills', () => {
        expect(textOf(page())).toContain('HK standard curves');
    });

    it('groups the curves by the sections the document fills, in the order it declares them', () => {
        const html = page('curves');
        const groups = [
            ...html.matchAll(/<summary class="[^"]*">\s*<span>([A-Za-z]+)<\/span>/g),
        ].map((match) => match[1]);
        expect(groups).toEqual(['YieldCurves']);
        const words = textOf(html);
        expect(words).toContain('1 in 1 of 3 sections');
        expect(words).toContain('2 sections hold no curve in this document');
        expect(words).toContain('EquityCurves');
        expect(words).toContain('CurveSettings');
    });

    it('draws the first curve in the workbench with its value, its words and the values it accepts', () => {
        const words = textOf(page('curves'));
        expect(words).toContain('USD-6M');
        expect(words).toContain('USD discount curve');
        expect(words).toContain('Interpolation method');
        expect(words).toContain('Linear');
        expect(words).toContain('The curve');
        expect(words).toContain('Its parameters');
    });

    it('counts the gaps of a curve and still shows the value of a leaf the contract does not document', () => {
        const words = textOf(page('curves'));
        expect(words).toContain('Tolerance');
        expect(words).toContain('0.0001');
        expect(words).toContain('1 without documentation');
    });

    it("states a curve's documentation once, under the panel it is collapsed in", () => {
        const words = textOf(page('curves'));
        expect(words).toContain('Documentation');
        // The panel is closed, so the sentence it holds is not repeated under the value.
        expect(words).not.toContain('How the curve interpolates between its pillars.');
    });

    it('draws a flag as Yes or No', () => {
        const words = textOf(page('curves'));
        expect(words).toContain('Include at the money');
        expect(words).toContain('Yes');
    });

    it('names the child elements a curve holds', () => {
        const words = textOf(page('curves'));
        expect(words).toContain('Report');
        expect(words).toContain('Segments');
        expect(words).toContain('not recorded here');
    });

    it('names the reports that bind the configuration', () => {
        expect(textOf(page('usedBy'))).toContain('HK daily VaR');
    });

    it('reads the history as the record timeline draws it', () => {
        expect(textOf(page('history'))).toContain(
            'Every version of the configuration, newest first',
        );
    });
});
