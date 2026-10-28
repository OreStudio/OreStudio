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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { MarketRegime, RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { ArrivalsDialog, ArrivalsPanel, saveArrivals } from './ArrivalsPanel.js';

const noSave = (): Promise<void> => Promise.resolve();

function feed(over: Record<string, unknown> = {}): RecordRow {
    return {
        id: 'feed-1',
        version: 1,
        arrival_mode: 'hawkes',
        branching_ratio: 0.6,
        decay: 20,
        sensitivity: 1.3,
        ...over,
    } as unknown as RecordRow;
}

function render(row: RecordRow, mayWrite = true, market?: MarketRegime, running = false): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <ArrivalsPanel
                    feed={row}
                    market={market}
                    mayWriteFeed={mayWrite}
                    running={running}
                    save={noSave}
                />
            </MemoryRouter>
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

function dialog(row: RecordRow, running = false): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ArrivalsDialog feed={row} running={running} save={noSave} onClose={() => undefined} />
        </TranslationProvider>,
    );
}

describe('a feed’s arrivals tab', () => {
    it('reads the model and the three numbers, with no field to type in', () => {
        const html = render(feed());
        expect(html).toContain('Tick arrival model');
        expect(html).toContain('Hawkes');
        expect(html).toContain('Branching ratio');
        expect(html).toContain('0.6');
        expect(html).toContain('Sensitivity to the market');
        expect(html).toContain('1.3');
        expect(html).not.toContain('<input');
        expect(html).not.toContain('<select');
    });

    it('reads no Hawkes numbers for a model that does not use them', () => {
        const html = render(feed({ arrival_mode: 'periodic' }));
        expect(html).not.toContain('0.6');
    });

    it('offers Edit to a caller who may write the feed, and not to one who may not', () => {
        expect(render(feed())).toMatch(/<button[^>]*>.*Edit<\/button>/);
        expect(render(feed(), false)).not.toContain('Edit</button>');
    });

    it('reads the market state the run reports, and says when it cannot be read', () => {
        expect(render(feed(), true, { state: 'calm', multiplier: 0.5, pinned: false })).toContain(
            'Market now: Calm ×0.5.',
        );
        expect(render(feed())).toContain('the market state is not readable right now');
    });

    it('links to the market, which is set on the Synthetic screen’s Market tab', () => {
        expect(render(feed())).toContain('href="/market-data/synthetic?tab=regime"');
    });
});

describe('the dialog that edits a feed’s arrivals', () => {
    it('enables the Hawkes numbers for a Hawkes feed', () => {
        expect(disabledAfter(dialog(feed()), 'Branching ratio')).toBe(false);
    });

    it('disables the Hawkes numbers for a model that does not read them, and keeps their values', () => {
        const html = dialog(feed({ arrival_mode: 'periodic' }));
        expect(disabledAfter(html, 'Branching ratio')).toBe(true);
        expect(html).toContain('0.6');
    });

    it('offers a restart for a running feed, ticked, and nothing for a stopped one', () => {
        expect(dialog(feed(), true)).toContain('The feed is running on its old law.');
        expect(dialog(feed(), true)).toMatch(/type="checkbox" checked=""/);
        expect(dialog(feed(), false)).not.toContain('The feed is running on its old law.');
    });

    it('reads a stored value as typed, so an empty field is not taken for zero', () => {
        const html = dialog(feed({ sensitivity: 1.3 }));
        expect(html).toContain('value="1.3"');
        expect(html).not.toContain('aria-invalid="true"');
    });

    it('saves with a button, which waits for a change', () => {
        expect(dialog(feed())).toMatch(/<button[^>]*disabled=""[^>]*>[^<]*Save/);
    });
});

describe('saving a feed’s arrivals', () => {
    function steps(fail?: 'save' | 'restart') {
        const calls: string[] = [];
        return {
            calls,
            save: async () => {
                calls.push('save');
                if (fail === 'save') throw new Error('refused');
            },
            saved: async () => {
                calls.push('saved');
            },
            restart: async () => {
                calls.push('restart');
                if (fail === 'restart') throw new Error('no bus');
            },
        };
    }

    it('stores, re-reads, restarts and re-reads again', async () => {
        const run = steps();
        expect(await saveArrivals(run)).toEqual({ kind: 'saved' });
        expect(run.calls).toEqual(['save', 'saved', 'restart', 'saved']);
    });

    it('tells a failed restart from a failed save, and still re-reads the feed', async () => {
        const run = steps('restart');
        expect(await saveArrivals(run)).toEqual({ kind: 'restartFailed', message: 'no bus' });
        expect(run.calls).toEqual(['save', 'saved', 'restart', 'saved']);
    });

    it('neither re-reads nor restarts when the save is refused', async () => {
        const run = steps('save');
        expect(await saveArrivals(run)).toEqual({ kind: 'failed', message: 'refused' });
        expect(run.calls).toEqual(['save']);
    });

    it('skips the restart when it is not asked for', async () => {
        const run = steps();
        expect(await saveArrivals({ save: run.save, saved: run.saved })).toEqual({ kind: 'saved' });
        expect(run.calls).toEqual(['save', 'saved']);
    });
});
