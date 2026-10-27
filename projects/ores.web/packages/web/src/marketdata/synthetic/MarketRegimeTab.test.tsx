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
import type {
    FeedRaster,
    MarketRegime,
    MarketState,
    MarketTrace,
    RegimeMode,
    TraceEvent,
} from '../../api/client.js';
import type { StateSample } from './marketRaster.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { MarketRegimePanel } from './MarketRegimeTab.js';

const NOW = Date.parse('2026-10-26T12:00:00Z');

function regime(over: Partial<MarketRegime> = {}): MarketRegime {
    return {
        state: 'calm',
        multiplier: 0.5,
        pinned: false,
        heat: 0.12,
        jump: 0.5,
        decay: 20,
        samples: [],
        pace: 0.5,
        tau: 30,
        populationRate: 2,
        loopGain: 0.025,
        runSeed: 42,
        ...over,
    };
}

/** Two readings of the heat, so the chart has a line to draw. */
function heatReadings(): MarketRegime['samples'] {
    return [
        { at: new Date(NOW - 20_000).toISOString(), heat: 0.5 },
        { at: new Date(NOW - 10_000).toISOString(), heat: 0.02 },
    ];
}

function raster(over: Partial<FeedRaster> = {}): FeedRaster {
    return {
        seconds: 60,
        feeds: [
            {
                sourceName: 'live.eurusd',
                label: 'EUR/USD',
                assetClass: 'fx',
                running: true,
                ticks: [new Date(NOW - 10_000).toISOString()],
                expectedPerSecond: 1,
            },
            {
                sourceName: 'live.usdsofr',
                label: 'USD SOFR',
                assetClass: 'interest_rates',
                running: true,
                ticks: [new Date(NOW - 20_000).toISOString()],
                expectedPerSecond: 1 / 60,
            },
        ],
        ...over,
    };
}

function render(
    regimeNow: MarketRegime,
    rasterNow: FeedRaster | undefined = raster(),
    pending?: RegimeMode,
    onPin: (mode: RegimeMode) => void = () => undefined,
    marked: { readonly at: number; readonly state: MarketState } | null = null,
    states: readonly StateSample[] = [],
): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MarketRegimePanel
                regime={regimeNow}
                raster={rasterNow}
                now={NOW}
                pending={pending}
                error={null}
                marked={marked}
                states={states}
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

    it('says a person holds the state when it is pinned, and marks it forced', () => {
        const html = render(regime({ state: 'active', multiplier: 2, pinned: true }));
        expect(html).toContain('Active');
        expect(html).toContain('×2');
        expect(html).toContain('A person, and it holds until it is handed back');
        expect(html).toContain('forced');
    });

    it('does not call a state the chain chose forced', () => {
        expect(render(regime())).not.toContain('forced');
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

    it('shows a row for every feed, named what a person calls it', () => {
        const html = render(regime());
        expect(html).toContain('EUR/USD');
        expect(html).toContain('USD SOFR');
    });

    it('draws a feed of each asset class in its class’s colour', () => {
        const html = render(regime());
        // The dot of the feed's own row, not the legend's swatch.
        expect(html).toMatch(/rounded-full bg-class-fx"/);
        expect(html).toMatch(/rounded-full bg-class-interest-rates"/);
    });

    it('says a feed that is not running is stopped, and draws no dot for it', () => {
        const html = render(regime(), {
            seconds: 60,
            feeds: [
                {
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: false,
                    ticks: [],
                    expectedPerSecond: 1,
                },
            ],
        });
        expect(html).toContain('stopped');
        expect(html).not.toContain('data-mark');
    });

    it('counts the arrivals of a feed that runs', () => {
        const html = render(regime());
        expect(html.match(/data-mark/g)).toHaveLength(2);
    });

    it('draws the factor in its own section above the arrivals', () => {
        const html = render(regime({ samples: heatReadings() }));
        expect(html).toContain('The factor over the window');
    });

    it('writes a feed’s name in its asset class colour', () => {
        const html = render(regime());
        // The name's own element, so the chart's line cannot satisfy it.
        expect(html).toMatch(/text-class-fx[^>]*>EUR\/USD</);
        expect(html).toMatch(/text-class-interest-rates[^>]*>USD SOFR</);
    });

    it('draws the shared heat the service sampled, and the numbers that move it', () => {
        const html = render(regime({ samples: heatReadings() }));
        expect(html).toContain('the shared heat');
        expect(html).toContain('0.12');
        expect(html).toContain('one event adds 0.5 of heat');
    });

    it('draws no heat line before the service has sampled one', () => {
        expect(render(regime())).not.toContain('the shared heat');
    });

    it('draws how fast each class ticked above the arrivals', () => {
        const html = render(regime());
        expect(html).toContain('ticks a second, per feed');
        expect(html).toContain('<path');
        expect(html).toContain('text-class-fx');
    });

    it('draws the state as the band the arrivals sit on', () => {
        const html = render(regime(), raster(), undefined, undefined, null, [
            { at: NOW - 50_000, state: 'calm' },
            { at: NOW - 10_000, state: 'active' },
        ]);
        expect(html).toContain('an active market');
        expect(html).toContain('bg-ink/10');
    });

    it('marks where a switch landed', () => {
        const html = render(regime(), raster(), undefined, undefined, {
            at: NOW - 5_000,
            state: 'active',
        });
        expect(html).toContain('the line is when the state was set: Active');
    });

    it('says a picture that has not filled yet starts when the tab opens', () => {
        const html = render(regime(), {
            seconds: 120,
            feeds: [
                {
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: true,
                    ticks: [],
                    expectedPerSecond: 1,
                },
            ],
        });
        expect(html).toContain('A row fills as its feed ticks');
    });

    it('does not explain a picture that has marks', () => {
        expect(render(regime())).not.toContain('A row fills as its feed ticks');
    });

    it('says there is nothing to draw when there are no feeds', () => {
        const html = render(regime(), { seconds: 60, feeds: [] });
        expect(html).toContain('There are no feeds to draw.');
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
            feeds: [
                {
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: true,
                    ticks: [old],
                    expectedPerSecond: 1,
                },
            ],
        });
        expect(html).not.toContain('data-mark');
    });
});

/** A tick or a decision of the scheduler, so many seconds before now. */
function traced(secondsAgo: number, over: Partial<TraceEvent> = {}): TraceEvent {
    return {
        kind: 'tick',
        sourceName: 'live.eurusd',
        at: new Date(NOW - secondsAgo * 1000).toISOString(),
        tau: 60 - secondsAgo / 2,
        operationalGap: 1,
        wallGap: 2,
        compensatedGap: 1,
        heat: 0.05,
        multiplier: 0.5,
        pace: 0.55,
        state: 'calm',
        ...over,
    };
}

function trace(over: Partial<MarketTrace> = {}): MarketTrace {
    return {
        runSeed: 42,
        ticksPerSecond: 12.5,
        heapSize: 2,
        latenessSeconds: 0.0025,
        maxLatenessSeconds: 0.027,
        series: 2,
        events: [
            traced(50, { kind: 'add', sourceName: 'live.eurusd' }),
            traced(40),
            traced(30, { kind: 'pin', sourceName: '', state: 'active', multiplier: 2, pace: 2.05 }),
            traced(20, { state: 'active', multiplier: 2, pace: 2.05, compensatedGap: 0.8 }),
            traced(10, { state: 'active', multiplier: 2, pace: 2.05, compensatedGap: 1.3 }),
        ],
        feeds: [
            {
                sourceName: 'live.eurusd',
                mode: 'periodic',
                rate: 1,
                sensitivity: 1,
                effectiveRate: 2.05,
                phase: 0.37,
                excess: 0,
            },
        ],
        ...over,
    };
}

function renderTraced(traceNow: MarketTrace = trace()): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MarketRegimePanel
                regime={regime({ pace: 2.05, state: 'active', multiplier: 2, pinned: true })}
                raster={raster()}
                now={NOW}
                pending={undefined}
                error={null}
                marked={null}
                states={[]}
                trace={traceNow}
                onPin={() => undefined}
            />
        </TranslationProvider>,
    );
}

describe('the market regime tab with the scheduler’s record', () => {
    it('shows the pace, the run seed, the loop gain against one and the loop’s numbers', () => {
        const html = renderTraced();
        expect(html).toContain('×2.05');
        expect(html).toContain('0.025 of 1');
        expect(html).toContain('>42<');
        expect(html).toContain('12.5');
        expect(html).toContain('2.5 ms, at most 27.0 ms');
    });

    it('draws the pace stacked into the regime and the heat', () => {
        const html = renderTraced();
        expect(html).toContain('how fast market time runs');
        expect(html.match(/data-pace/g)?.length).toBe(5);
    });

    it('shows each running feed’s law, rate, sensitivity, rate now and phase on its row', () => {
        const html = renderTraced();
        expect(html).toContain('Periodic 1.00/s ×1.00 → 2.05/s initial phase 0.37');
    });

    it('offers to place the marks by clock time or by market time', () => {
        const html = renderTraced();
        expect(html).toContain('Clock time');
        expect(html).toContain('Market time τ');
    });

    it('sets each feed against its law and plots its rescaled gaps against Exp(1)', () => {
        const html = renderTraced();
        expect(html).toContain('Each feed against its law');
        // Three ticks give two gaps, too few for a plot, so the tab says so instead.
        expect(html).toContain('live.eurusd has too few ticks in the window for a plot');
        expect(html).not.toContain('data-qq');
    });

    it('lists what the loop applied, newest first', () => {
        const html = renderTraced();
        const pinned = html.indexOf('pinned');
        const added = html.indexOf('added');
        expect(pinned).toBeGreaterThan(0);
        expect(added).toBeGreaterThan(pinned);
    });

    it('draws none of it before the record is read', () => {
        const html = render(regime());
        expect(html).not.toContain('Each feed against its law');
        expect(html).not.toContain('What the loop applied');
    });
});

describe('the QQ plot of a busy feed', () => {
    it('plots a feed with enough gaps against Exp(1)', () => {
        const ticks = [60, 50, 40, 30, 20, 10, 5].map((ago) => traced(ago));
        const html = renderTraced(trace({ events: ticks }));
        expect(html).toContain('The rescaled gaps of live.eurusd against Exp(1)');
        expect(html.match(/data-qq/g)?.length).toBe(6);
    });
});
