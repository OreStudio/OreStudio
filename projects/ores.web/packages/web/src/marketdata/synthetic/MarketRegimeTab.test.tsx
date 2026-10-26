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

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { FeedRaster, MarketRegime, RegimeMode } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { MarketRegimePanel } from './MarketRegimeTab.js';

const NOW = Date.parse('2026-10-26T12:00:00Z');

function regime(over: Partial<MarketRegime> = {}): MarketRegime {
    return { state: 'calm', multiplier: 0.5, pinned: false, ...over };
}

function raster(over: Partial<FeedRaster> = {}): FeedRaster {
    return {
        seconds: 60,
        feeds: [
            { sourceName: 'live.eurusd', ticks: [new Date(NOW - 10_000).toISOString()] },
            { sourceName: 'live.usdsofr', ticks: [new Date(NOW - 20_000).toISOString()] },
        ],
        ...over,
    };
}

function render(
    regimeNow: MarketRegime,
    rasterNow: FeedRaster | undefined = raster(),
    pending?: RegimeMode,
    onPin: (mode: RegimeMode) => void = () => undefined,
): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MarketRegimePanel
                regime={regimeNow}
                raster={rasterNow}
                now={NOW}
                pending={pending}
                error={null}
                onPin={onPin}
            />
        </TranslationProvider>,
    );
}

/** The attributes of the switch whose label the given text is, and nothing else's. */
function button(html: string, label: string): string {
    const marked = html.indexOf(`>${label}</button>`);
    const openAt = html.lastIndexOf('<button', marked);
    return marked < 0 ? '' : html.slice(openAt, html.indexOf('>', openAt));
}

describe('the market regime tab', () => {
    it('shows the state, the multiplier and that the chain holds it', () => {
        const html = render(regime());
        expect(html).toContain('Calm');
        expect(html).toContain('×0.5');
        expect(html).toContain('The chain, turning on its own');
    });

    it('says a person holds the state when it is pinned', () => {
        const html = render(regime({ state: 'active', multiplier: 2, pinned: true }));
        expect(html).toContain('Active');
        expect(html).toContain('×2');
        expect(html).toContain('A person, and it holds until it is handed back');
    });

    it('marks auto as the switch in force while the chain holds the state', () => {
        const html = render(regime());
        expect(button(html, 'Auto')).toContain('aria-pressed="true"');
        expect(button(html, 'Active')).toContain('aria-pressed="false"');
    });

    it('marks the pinned state as the switch in force', () => {
        const html = render(regime({ state: 'active', multiplier: 2, pinned: true }));
        expect(button(html, 'Active')).toContain('aria-pressed="true"');
        expect(button(html, 'Auto')).toContain('aria-pressed="false"');
    });

    it('shows a row for every running feed, named', () => {
        const html = render(regime());
        expect(html).toContain('live.eurusd');
        expect(html).toContain('live.usdsofr');
    });

    it('says there are no rows when no feed is running', () => {
        const html = render(regime(), { seconds: 60, feeds: [] });
        expect(html).toContain('No feed is running, so there are no rows.');
        expect(html).not.toContain('data-mark');
    });

    it('draws one mark for each tick in the window', () => {
        const html = render(regime());
        expect(html.match(/data-mark/g)).toHaveLength(2);
    });

    it('draws no mark for a tick that has left the window', () => {
        const old = new Date(NOW - 120_000).toISOString();
        const html = render(regime(), {
            seconds: 60,
            feeds: [{ sourceName: 'live.eurusd', ticks: [old] }],
        });
        expect(html).not.toContain('data-mark');
    });
});
