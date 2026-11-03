/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { VarianceCalibration } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { StartLevel } from './CalibrationPages.js';
import { VarianceCalibrationFacts, VarianceReview } from './VarianceCalibration.js';

const FIT: VarianceCalibration = {
    input_id: '11111111-1111-4111-8111-111111111111',
    calibration_id: 'b608b4be-6a2d-56d1-a144-09cb668ba393',
    method: 'gjr_garch_qmle',
    state: 'proposed',
    state_reason: '',
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
    provider_code: 'ecb',
    inverted: false,
    window_days: 1826,
    window_end: '2026-11-02',
    point_count: 280,
    used_count: 279,
    alpha: 0.011340771,
    gamma: 0.022995552,
    beta: 0.932267071,
    persistence: 0.955105619,
    starting_variance_ratio: 0.918952996,
    starts: 4,
    starts_agreed: true,
};

function render(node: ReactNode): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <MemoryRouter>
                <TranslationProvider>{node}</TranslationProvider>
            </MemoryRouter>
        </QueryClientProvider>,
    );
}

describe('the evidence of a variance calibration', () => {
    it('shows the shape, its persistence, the fit and the input it came from', () => {
        const html = render(<VarianceCalibrationFacts variance={FIT} />);
        expect(html).toContain('Proposed');
        expect(html).toContain('Alpha (α)');
        expect(html).toContain('Gamma (γ)');
        expect(html).toContain('Beta (β)');
        expect(html).toContain('0.0113');
        expect(html).toContain('0.0230');
        expect(html).toContain('0.9323');
        expect(html).toContain('0.9551');
        expect(html).toContain('0.9190');
        expect(html).toContain('4, all agree');
        expect(html).toContain('279 of 280 fixings');
        expect(html).toContain('1826 days to 2026-11-02');
        expect(html).toContain('ecb');
        expect(html).toContain('Direct');
        expect(html).toContain('oresmd://fx/EUR?type=fixing&amp;index=fx');
        expect(html).toContain(FIT.calibration_id);
        expect(html).toContain(FIT.input_id);
    });

    it('warns when the fit’s starts disagree', () => {
        const html = render(
            <VarianceCalibrationFacts variance={{ ...FIT, starts_agreed: false }} />,
        );
        expect(html).toContain('4, they disagree');
        expect(html).toContain('weakly identified');
    });

    it('shows a rejection’s reason and no shape', () => {
        const html = render(
            <VarianceCalibrationFacts
                variance={{
                    ...FIT,
                    state: 'rejected',
                    state_reason: 'the input holds 241 daily returns and a fit needs 250',
                    alpha: null,
                    gamma: null,
                    beta: null,
                    persistence: null,
                    starting_variance_ratio: null,
                    starts: 0,
                }}
            />,
        );
        expect(html).toContain('Rejected');
        expect(html).toContain('the input holds 241 daily returns');
        expect(html).not.toContain('Persistence');
        expect(html).not.toContain('weakly identified');
    });

    it('reads an inverted input as such', () => {
        const html = render(<VarianceCalibrationFacts variance={{ ...FIT, inverted: true }} />);
        expect(html).toContain('the item quotes the pair the other way round');
    });
});

describe('the method of a variance calibration', () => {
    it('names an authored shape and shows no input, since it was fitted from none', () => {
        const html = render(
            <VarianceCalibrationFacts
                variance={{
                    ...FIT,
                    method: 'authored',
                    state: 'approved',
                    state_reason: 'The authored shape installed with the system feeds',
                    input_id: '',
                    item: '',
                    point_count: 0,
                    used_count: 0,
                    window_days: 0,
                    window_end: '',
                }}
            />,
        );
        expect(html).toContain('Authored');
        expect(html).toContain('Approved');
        expect(html).not.toContain('Returns used');
        expect(html).not.toContain('Window');
    });

    it('names a fit', () => {
        expect(
            render(<VarianceCalibrationFacts variance={{ ...FIT, method: 'gjr_garch_qmle' }} />),
        ).toContain('Fitted');
    });
});

describe('the review of a variance calibration', () => {
    it('offers approve and reject, with a required reason, for a proposed calibration', () => {
        const html = render(<VarianceReview variance={FIT} onReviewed={() => undefined} />);
        expect(html).toContain('Approve');
        expect(html).toContain('Reject');
        expect(html).toContain('Required.');
        expect(html.match(/<button[^>]*disabled=""/g)?.length).toBe(2);
    });

    it('offers no review for a decided calibration or a fit that is not stored', () => {
        for (const state of ['approved', 'rejected', 'superseded']) {
            expect(
                render(
                    <VarianceReview variance={{ ...FIT, state }} onReviewed={() => undefined} />,
                ),
            ).toBe('');
        }
        expect(
            render(
                <VarianceReview
                    variance={{ ...FIT, calibration_id: '' }}
                    onReviewed={() => undefined}
                />,
            ),
        ).toBe('');
    });
});

describe('a feed’s start level', () => {
    const start = {
        from_fixing: true,
        item: 'oresmd://fx/USD?type=fixing&index=fx&source=IMF&ccy=AUD',
        provider_code: 'imf',
        vendor_id: 'AUD',
        value: '1.4239',
        inverted: true,
        window_days: 90,
        used: 60,
        note: '',
        dispersion: [],
    };

    it('reads an inverted item as one over its value, in market convention, first', () => {
        const html = render(<StartLevel start={start} />);
        const level = (1 / 1.4239).toPrecision(6);
        expect(html).toContain(level);
        expect(html).toContain('one over the fixing 1.4239');
        expect(html.indexOf(level)).toBeLessThan(html.indexOf('1.4239'));
    });

    it('reads a direct item as its value', () => {
        expect(render(<StartLevel start={{ ...start, inverted: false, value: '0.7023' }} />)).toBe(
            '0.7023',
        );
    });
});
