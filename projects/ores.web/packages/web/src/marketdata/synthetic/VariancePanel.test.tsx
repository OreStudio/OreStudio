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
 */

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { FeedStart, RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FeedStartVariance } from './feedStart.js';
import { VarianceTab } from './VariancePanel.js';

const CALIBRATION = '84902b7e-58f4-4127-86cb-129ebedabe95';

function feed(over: Record<string, unknown> = {}): RecordRow {
    return {
        id: 'feed-1',
        version: 1,
        source_name: 'synthetic.eurusd',
        process_type: 'geometric',
        variance_model: 'gjr_garch',
        variance_calibration_id: CALIBRATION,
        ...over,
    } as unknown as RecordRow;
}

function tab(row: RecordRow): string {
    return renderToStaticMarkup(
        <MemoryRouter>
            <TranslationProvider>
                <VarianceTab feed={row} />
            </TranslationProvider>
        </MemoryRouter>,
    );
}

describe('a feed’s variance tab', () => {
    it('names the model and the calibration a GJR-GARCH feed runs', () => {
        const html = tab(feed());
        expect(html).toContain('Variance model');
        expect(html).toContain('GJR-GARCH');
        expect(html).toContain(CALIBRATION);
    });

    it('names no calibration for a constant variance', () => {
        const html = tab(feed({ variance_model: 'constant', variance_calibration_id: null }));
        expect(html).toContain('Constant');
        expect(html).not.toContain(CALIBRATION);
        expect(html).toContain('None');
    });

    it('offers nothing to type and nothing to edit, since the shape comes from a review', () => {
        const html = tab(feed());
        expect(html).not.toContain('<input');
        expect(html).not.toContain('<button');
        expect(html).toContain('Calibration screen');
    });

    it('shows the shape the running feed carries and links to its calibration', () => {
        const html = tab(
            feed({
                asset_class_code: 'fx',
                start: {
                    variance: {
                        model: 'gjr_garch',
                        calibration_id: CALIBRATION,
                        calibration_version: 2,
                        fitted: true,
                        alpha: 0.0113,
                        gamma: 0.023,
                        beta: 0.9323,
                        persistence: 0.9551,
                        initial_ratio: 0.92,
                        returns: 279,
                        note: '',
                    },
                },
            }),
        );
        expect(html).toContain('What the running feed carries');
        expect(html).toContain('version 2');
        expect(html).toContain('0.932');
        expect(html).toMatch(/href="[^"]*calibration[^"]*tab=variance"/);
    });
});

describe('the variance in the notice of where a run started', () => {
    const variance: NonNullable<FeedStart['variance']> = {
        model: 'gjr_garch',
        fitted: true,
        alpha: 0.0612,
        gamma: 0.0431,
        beta: 0.9012,
        persistence: 0.9842,
        initial_ratio: 1.734,
        returns: 1259,
        note: '',
    };
    const render = (v: NonNullable<FeedStart['variance']>): string =>
        renderToStaticMarkup(
            <TranslationProvider>
                <FeedStartVariance variance={v} />
            </TranslationProvider>,
        );

    it('says a fitted shape, the returns it read and how the first day stands', () => {
        const html = render(variance);
        expect(html).toContain('fitted from 1259 daily returns');
        expect(html).toContain('alpha (α) 0.061');
        expect(html).toContain('persistence 0.984');
        expect(html).toContain('1.73 times the long-run level');
    });

    it('says an authored shape was kept, and why, with no first-day line', () => {
        const html = render({
            ...variance,
            fitted: false,
            returns: 60,
            initial_ratio: 1,
            note: 'The window holds 60 daily returns and a fit needs 250, so the authored shape is kept.',
        });
        expect(html).toContain('with the authored shape');
        expect(html).toContain('a fit needs 250');
        expect(html).not.toContain('times the long-run level');
    });
});
