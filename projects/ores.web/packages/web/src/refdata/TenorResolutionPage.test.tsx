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
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { ReactNode } from 'react';
import type { TenorResolution } from '../api/tenors.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ResolutionAnswer, TenorResolutionPage } from './TenorResolutionPage.js';

const catalogue = {
    tenors: [
        {
            version: 1,
            code: '3M',
            display_name: 'Three months',
            description: '',
            sort_order: 30,
            kind: 'PERIOD',
            unit: 'MONTH',
            multiplier: 3,
        },
    ],
    conventions: [
        {
            version: 1,
            code: 'RATES_SPOT_FORWARD',
            description: '',
            measured_from: 'SPOT',
            resolution_algorithm: 'ANCHOR_OFFSET',
            business_day_convention_type: 'ModifiedFollowing',
        },
    ],
    schedules: [],
};

const pickLists = {
    kinds: [],
    units: [],
    anchors: [],
    algorithms: [],
    rollRules: [],
    calendars: [{ version: 1, code: 'Argentina', name: 'Argentina' }],
};

const resolutions = [
    {
        version: 1,
        convention_code: 'RATES_SPOT_FORWARD',
        tenor_code: '3M',
        anchor_override: null,
        offset_unit: null,
        offset_multiplier: null,
        schedule_code: null,
        schedule_step_count: null,
    },
];

function render(node: ReactNode): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [{ permissionCodes: ['*'] }] });
    client.setQueryData(['tenors', 'catalogue'], catalogue);
    client.setQueryData(['tenors', 'pick-lists'], pickLists);
    client.setQueryData(
        ['tenors', 'conventions', 'RATES_SPOT_FORWARD', 'resolutions'],
        resolutions,
    );
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>{node}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const answered: TenorResolution = {
    result: { outcome: 'ok', code: '', message: '', fields: [] },
    anchorDate: '2026-03-16',
    dateBeforeRoll: '2026-06-16',
    adjustedDate: '2026-06-16',
    rollRule: 'ModifiedFollowing',
    calendar: 'Argentina',
};

describe('the tenor resolution screen', () => {
    it('asks the four questions and offers the authoring journey', () => {
        const html = render(<TenorResolutionPage />);

        expect(html).toContain('Convention');
        expect(html).toContain('Spot date');
        expect(html).toContain('Tenor');
        expect(html).toContain('Horizon date');
        expect(html).toContain('>Resolve<');
        expect(html).toContain('Argentina');
        expect(html).toContain('href="/refdata/tenors/author"');
        expect(html).not.toContain('The tenor resolves to this date.');
        expect(html).not.toContain('The server declined to resolve this tenor.');
        expect(html).toContain('disabled');
    });

    it('shows the three dates and the rule that reached them', () => {
        const html = render(
            <ResolutionAnswer answer={{ kind: 'answered', resolution: answered }} />,
        );

        expect(html).toContain('The tenor resolves to this date.');
        expect(html).toContain('2026-03-16');
        expect(html).toContain('2026-06-16');
        expect(html).toContain('ModifiedFollowing');
        expect(html).toContain('Argentina');
    });

    it('shows a refusal with its code and the field at fault, and no date it did not reach', () => {
        const html = render(
            <ResolutionAnswer
                answer={{
                    kind: 'refused',
                    resolution: {
                        result: {
                            outcome: 'invalid',
                            code: 'tenor_not_in_convention',
                            message: 'The convention does not resolve this tenor.',
                            fields: [
                                {
                                    field: 'tenor_code',
                                    code: 'tenor_not_in_convention',
                                    message: 'No rule resolves 3M.',
                                },
                            ],
                        },
                        anchorDate: '',
                        dateBeforeRoll: '',
                        adjustedDate: '',
                        rollRule: 'ModifiedFollowing',
                        calendar: 'Argentina',
                    },
                }}
            />,
        );

        expect(html).toContain('The server declined to resolve this tenor.');
        expect(html).toContain('tenor_not_in_convention');
        expect(html).toContain('tenor_code: No rule resolves 3M.');
        expect(html).not.toContain('Anchor date');
        expect(html).not.toContain('The tenor resolves to this date.');
    });

    it('says a request failed rather than a rule was declined', () => {
        const html = render(
            <ResolutionAnswer answer={{ kind: 'unreachable', message: 'the socket closed' }} />,
        );

        expect(html).toContain('The request to resolve the tenor failed.');
        expect(html).toContain('the socket closed');
        expect(html).not.toContain('The server declined to resolve this tenor.');
    });
});
