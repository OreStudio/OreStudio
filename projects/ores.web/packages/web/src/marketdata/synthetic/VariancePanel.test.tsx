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
import type { FeedStart, RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FeedStartVariance } from './feedStart.js';
import {
    settingsOf,
    VarianceDialog,
    VariancePanel,
    varianceProblems,
    type VarianceSettings,
} from './VariancePanel.js';

const noSave = (): Promise<void> => Promise.resolve();

function feed(over: Record<string, unknown> = {}): RecordRow {
    return {
        id: 'feed-1',
        version: 1,
        source_name: 'synthetic.eurusd',
        process_type: 'geometric',
        variance_model: 'gjr_garch',
        garch_alpha: 0.07,
        garch_gamma: -0.03,
        garch_beta: 0.88,
        garch_window_days: 1500,
        ...over,
    } as unknown as RecordRow;
}

const garch: VarianceSettings = {
    model: 'gjr_garch',
    alpha: 0.05,
    gamma: 0.02,
    beta: 0.9,
    windowDays: 1826,
};

function panel(row: RecordRow, mayWrite = true): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <VariancePanel feed={row} mayWriteFeed={mayWrite} save={noSave} />
        </TranslationProvider>,
    );
}

function dialog(row: RecordRow, running = false): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <VarianceDialog feed={row} running={running} save={noSave} onClose={() => undefined} />
        </TranslationProvider>,
    );
}

/** Whether the input that follows the given label is disabled. */
function disabledAfter(html: string, label: string): boolean {
    const at = html.indexOf(label);
    if (at < 0) return false;
    const input = html.indexOf('<input', at);
    if (input < 0) return false;
    const tag = html.slice(input, html.indexOf('>', input));
    return /\sdisabled(=|\s|\/?>)/.test(tag);
}

describe('the rules a daily variance is held to before it is saved', () => {
    it('lets a constant variance through whatever the shape says', () => {
        expect(
            varianceProblems({ ...garch, model: 'constant', alpha: -1 }, 'ornstein_uhlenbeck'),
        ).toEqual([]);
    });

    it('accepts an admissible GJR-GARCH shape on an engine that can scale', () => {
        expect(varianceProblems(garch, 'geometric')).toEqual([]);
        expect(varianceProblems(garch, 'arithmetic')).toEqual([]);
    });

    it('refuses a GJR-GARCH variance on an engine that cannot scale its law', () => {
        expect(varianceProblems(garch, 'ornstein_uhlenbeck')).toEqual(['engine']);
    });

    it('refuses each bound the table checks, and names it', () => {
        expect(varianceProblems({ ...garch, alpha: -0.01 }, 'geometric')).toContain('alpha');
        expect(varianceProblems({ ...garch, beta: -0.01 }, 'geometric')).toContain('beta');
        expect(varianceProblems({ ...garch, gamma: -0.06 }, 'geometric')).toEqual(['gamma']);
        expect(varianceProblems({ ...garch, windowDays: 29 }, 'geometric')).toEqual(['window']);
        expect(varianceProblems({ ...garch, windowDays: 3661 }, 'geometric')).toEqual(['window']);
        expect(varianceProblems({ ...garch, windowDays: 100.5 }, 'geometric')).toEqual(['window']);
    });

    it('allows a negative gamma down to minus alpha, the boundary the table allows', () => {
        expect(varianceProblems({ ...garch, gamma: -0.05 }, 'geometric')).toEqual([]);
    });
});

describe('a feed’s stored variance', () => {
    it('reads the stored model and shape', () => {
        expect(settingsOf(feed())).toEqual({
            model: 'gjr_garch',
            alpha: 0.07,
            gamma: -0.03,
            beta: 0.88,
            windowDays: 1500,
        });
    });

    it('falls back to the table’s defaults for a row written before the columns', () => {
        const old = { id: 'feed-1', version: 1 } as unknown as RecordRow;
        expect(settingsOf(old)).toEqual({
            model: 'constant',
            alpha: 0.05,
            gamma: 0,
            beta: 0.9,
            windowDays: 1826,
        });
    });
});

describe('a feed’s variance tab', () => {
    it('reads the model, the shape and the window, with no field to type in', () => {
        const html = panel(feed());
        expect(html).toContain('Variance model');
        expect(html).toContain('GJR-GARCH');
        expect(html).toContain('0.07');
        expect(html).toContain('-0.03');
        expect(html).toContain('0.88');
        expect(html).toContain('1500');
        expect(html).not.toContain('<input');
    });

    it('reads no shape for a constant variance', () => {
        const html = panel(feed({ variance_model: 'constant' }));
        expect(html).toContain('Constant');
        expect(html).not.toContain('0.88');
    });

    it('offers Edit only to a caller who may write the feed', () => {
        expect(panel(feed())).toMatch(/<button[^>]*>.*Edit<\/button>/);
        expect(panel(feed(), false)).not.toContain('Edit</button>');
    });
});

describe('the dialog that edits a feed’s variance', () => {
    it('enables the shape for a GJR-GARCH variance', () => {
        expect(disabledAfter(dialog(feed()), 'Beta')).toBe(false);
    });

    it('disables the shape under a constant variance and keeps its values', () => {
        const html = dialog(feed({ variance_model: 'constant' }));
        expect(disabledAfter(html, 'Beta')).toBe(true);
        expect(html).toContain('0.88');
    });

    it('says why a GJR-GARCH variance cannot run on the mean-reverting engine', () => {
        expect(dialog(feed({ process_type: 'ornstein_uhlenbeck' }))).toContain(
            'needs the geometric or arithmetic engine',
        );
        expect(dialog(feed())).not.toContain('needs the geometric or arithmetic engine');
    });

    it('offers to restart a running feed, which keeps the variance it started with', () => {
        expect(dialog(feed(), true)).toContain('Restart it to apply');
        expect(dialog(feed(), false)).not.toContain('Restart it to apply');
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
        expect(html).toContain('alpha 0.061');
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
