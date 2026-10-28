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

import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type {
    FeedRaster,
    MarketHistory,
    MarketRegime,
    MarketTrace,
    TraceEvent,
    TraceFeed,
} from '../../../api/client.js';
import { TranslationProvider } from '../../../i18n/Provider.js';
import { tabAddress } from '../../../ui/Tabs.js';
import {
    ArrivalsView,
    coveredSeconds,
    LANE_SLOTS,
    MARKS_AT_MOST,
    meanPaceOf,
    providerOf,
    slotCounts,
} from './ArrivalsView.js';
import { FitView, ratioPosition } from './FitView.js';
import { FoldCard } from './FoldCard.js';
import { foldHistory, ticksBySecond } from './fold.js';
import { LawsView, lawRows, lawStable } from './LawsView.js';
import { MarketStrip } from './MarketStrip.js';
import { NowView, heldFor, marketChanges } from './NowView.js';
import { clean, compact, duration } from './numbers.js';
import { SettingsView, settingProblems, settingsOf, toSettings } from './SettingsView.js';
import { TraceView, traceRows } from './TraceView.js';

const NOW = Date.parse('2026-10-28T12:00:00Z');

function regime(over: Partial<MarketRegime> = {}): MarketRegime {
    return {
        state: 'calm',
        multiplier: 0.5,
        pinned: false,
        heat: 0.12,
        jump: 0.5,
        decay: 20,
        samples: [],
        pace: 0.52,
        tau: 30,
        populationRate: 2,
        loopGain: 0.025,
        runSeed: 42,
        speed: 1440,
        wakeRate: 1 / 600,
        calmRate: 1 / 300,
        tailLoading: 0,
        activeMultiplier: 2,
        ...over,
    };
}

function event(over: Partial<TraceEvent>): TraceEvent {
    return {
        kind: 'tick',
        sourceName: 'live.eurusd',
        at: new Date(NOW - 10_000).toISOString(),
        tau: 10,
        operationalGap: 1,
        wallGap: 1,
        compensatedGap: 1,
        heat: 0.1,
        multiplier: 0.5,
        pace: 0.5,
        state: 'calm',
        ...over,
    };
}

function law(over: Partial<TraceFeed> = {}): TraceFeed {
    return {
        sourceName: 'live.eurusd',
        mode: 'hawkes',
        rate: 1,
        sensitivity: 1.2,
        effectiveRate: 0.6,
        phase: 0,
        excess: 0.1,
        branchingRatio: 0.6,
        decay: 2,
        ...over,
    };
}

function trace(over: Partial<MarketTrace> = {}): MarketTrace {
    return {
        runSeed: 42,
        ticksPerSecond: 7.5,
        heapSize: 2,
        latenessSeconds: 0.0006,
        maxLatenessSeconds: 0.0031,
        series: 2,
        events: [
            event({
                kind: 'transition',
                sourceName: '',
                at: new Date(NOW - 60_000).toISOString(),
                tau: 0,
                state: 'active',
            }),
            event({ tau: 5, compensatedGap: 0.5 }),
            event({ tau: 10, compensatedGap: 1.5 }),
            event({
                kind: 'pin',
                sourceName: '',
                at: new Date(NOW - 5_000).toISOString(),
                tau: 20,
            }),
        ],
        feeds: [
            law(),
            law({
                sourceName: 'live.usdsofr',
                mode: 'periodic',
                rate: 1 / 60,
                sensitivity: 1,
                branchingRatio: 0,
                decay: 0,
            }),
        ],
        ...over,
    };
}

function raster(ticks: readonly string[] = [new Date(NOW - 10_000).toISOString()]): FeedRaster {
    return {
        seconds: 120,
        feeds: [
            {
                id: 'feed-1',
                kind: 'fx_spot',
                sourceName: 'live.eurusd',
                label: 'EUR/USD',
                assetClass: 'fx',
                running: true,
                ticks,
                expectedPerSecond: 1,
            },
            {
                id: 'feed-2',
                kind: 'ir_curve',
                sourceName: 'live.usdsofr',
                label: 'USD SOFR',
                assetClass: 'interest_rates',
                running: true,
                ticks: [],
                expectedPerSecond: 1 / 60,
            },
        ],
    };
}

/** Two classes over the last minute: busy for ten seconds, quiet after. */
function history(): MarketHistory {
    const second = (ago: number): string =>
        new Date(Math.floor(NOW / 1000) * 1000 - ago * 1000).toISOString();
    return {
        slotSeconds: 1,
        groups: ['fx', 'interest_rates'],
        bins: [
            { at: second(3700), counts: [99], pace: 1, activeShare: 0 },
            ...Array.from({ length: 10 }, (_, i) => ({
                at: second(60 - i),
                counts: [8, 1],
                pace: 2,
                activeShare: 1,
            })),
            { at: second(5), counts: [2], pace: 0.5, activeShare: 0 },
        ],
    };
}

function render(node: ReactNode): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>{node}</MemoryRouter>
        </TranslationProvider>,
    );
}

describe('the market numbers', () => {
    it('print without float noise', () => {
        expect(clean(1 / 600)).toBe('0.00166667');
        expect(clean(1440)).toBe('1440');
        expect(clean(0.1 + 0.2)).toBe('0.3');
    });

    it('fit a narrow column and say a span in its own unit', () => {
        expect(compact(11500)).toBe('11.5k');
        expect(compact(298)).toBe('298');
        expect(duration(600)).toBe('10 min');
        expect(duration(Math.LN2 / 20)).toBe('34.7 ms');
        expect(duration(86400)).toBe('24 h');
    });
});

describe('the market tabs', () => {
    it('keep the Synthetic screen’s tab when the market’s own tab changes', () => {
        expect(tabAddress(['now', 'trace'], 'trace', 'view', { tab: 'regime' })).toEqual({
            tab: 'regime',
            view: 'trace',
        });
        expect(tabAddress(['now', 'trace'], 'now', 'view', { tab: 'regime' })).toEqual({
            tab: 'regime',
        });
    });
});

describe('the market strip', () => {
    it('states the market in seven figures, with dashes until the loop is read', () => {
        const html = render(
            <MarketStrip
                regime={regime({ pinned: true, state: 'active', multiplier: 2 })}
                trace={undefined}
            />,
        );
        expect(html).toContain('forced');
        expect(html).toContain('×2');
        expect(html).toContain('1440');
        expect(html).toContain('—');
        const read = render(<MarketStrip regime={regime()} trace={trace()} />);
        expect(read).toContain('7.5');
        expect(read).toContain('0.6 ms');
        expect(read).toContain('at most 3.1 ms');
    });
});

describe('the Now tab', () => {
    const now = (mayPin: boolean, over: Partial<MarketRegime> = {}): string =>
        render(
            <NowView
                regime={regime(over)}
                trace={trace()}
                history={history()}
                now={NOW}
                mayPin={mayPin}
                pending={undefined}
                error={null}
                onPin={() => undefined}
                onOpenTrace={() => undefined}
            />,
        );

    it('offers the switch to a system session, with the state in force pressed', () => {
        const html = now(true, { pinned: true, state: 'active' });
        expect(html).toMatch(/aria-pressed="true"[^>]*>Active</);
        expect(html).toContain('A person, and it holds until it is handed back');
    });

    it('reads the market to a tenant and offers no switch', () => {
        const html = now(false);
        expect(html).toContain(
            'The system administrator sets the market for the whole installation',
        );
        expect(html).not.toContain('aria-pressed');
    });

    it('says what the chain’s numbers mean and how long the state has held', () => {
        const html = now(true);
        expect(html).toContain('calm 10 min · active 5 min, on average');
        expect(html).toContain('33.3%');
        expect(heldFor(trace().events, NOW)).toBeCloseTo(5);
    });

    it('lists what changed the market, newest first, and no tick', () => {
        const changes = marketChanges(trace().events);
        expect(changes.map((e) => e.kind)).toEqual(['pin', 'transition']);
    });
});

describe('the Settings tab', () => {
    it('edits spells in seconds and sends the rates the service stores', () => {
        const values = settingsOf(regime());
        expect(clean(values.calmSpell)).toBe('600');
        expect(clean(values.activeSpell)).toBe('300');
        const settings = toSettings(regime(), { ...values, calmSpell: 900 }, ['calmSpell']);
        expect(settings.wakeRate).toBeCloseTo(1 / 900);
        // A number nobody touched goes back exactly as the service holds it.
        expect(settings.calmRate).toBe(1 / 300);
        expect(toSettings(regime(), { ...values, calmSpell: 600.0000001 }, []).wakeRate).toBe(
            1 / 600,
        );
    });

    it('applies the service’s rules as a person types, naming the number', () => {
        const values = settingsOf(regime());
        expect(settingProblems(values, new Set(), 2)).toEqual({});
        expect(settingProblems({ ...values, jump: 40 }, new Set(), 2).decay).toEqual({
            key: 'gain',
            values: { gain: '2' },
        });
        expect(
            settingProblems({ ...values, activeSpell: 900 }, new Set(), 2).activeSpell?.key,
        ).toBe('share');
        expect(settingProblems({ ...values, tailLoading: 11 }, new Set(), 2).tailLoading?.key).toBe(
            'between',
        );
        expect(settingProblems({ ...values, speed: -1 }, new Set(), 2).speed?.key).toBe('positive');
        expect(settingProblems(values, new Set(['jump']), 2).jump?.key).toBe('notANumber');
    });

    it('shows a system session the six fields filled from the service, with nothing to apply yet', () => {
        const html = render(
            <SettingsView
                regime={regime()}
                mayChange
                pending={false}
                error={null}
                onApply={() => undefined}
            />,
        );
        expect(html.match(/<input/g)).toHaveLength(6);
        expect(html).toContain('value="600"');
        expect(html).toContain('value="1440"');
        expect(html).toMatch(/disabled=""[^>]*>Apply</);
        expect(html).toContain('34.7 ms');
        expect(html).toContain('×0.5');
    });

    it('shows a tenant the same numbers read only', () => {
        const html = render(
            <SettingsView
                regime={regime()}
                mayChange={false}
                pending={false}
                error={null}
                onApply={() => undefined}
            />,
        );
        expect(html.match(/readOnly=""|readonly=""/g)).toHaveLength(6);
        expect(html).not.toContain('>Apply<');
        expect(html).toContain('Read only.');
    });

    it('shows the service’s refusal in its own words', () => {
        const html = render(
            <SettingsView
                regime={regime()}
                mayChange
                pending={false}
                error="operational_clock: speed is -1; it must be positive"
                onApply={() => undefined}
            />,
        );
        expect(html).toContain('speed is -1; it must be positive');
    });
});

describe('the Arrivals tab', () => {
    const arrivals = (ticks: readonly string[]): string =>
        render(
            <ArrivalsView
                raster={raster(ticks)}
                trace={trace()}
                now={NOW}
                window={120}
                onWindow={() => undefined}
            />,
        );

    it('draws every tick while they fit', () => {
        const html = arrivals([
            new Date(NOW - 10_000).toISOString(),
            new Date(NOW - 20_000).toISOString(),
        ]);
        expect(html.match(/data-mark/g)).toHaveLength(2);
        expect(html).not.toContain('data-slot');
        expect(html).toContain('every tick');
    });

    it('draws each slot’s count once the ticks would merge into a bar', () => {
        const many = Array.from({ length: MARKS_AT_MOST + 50 }, (_, i) =>
            new Date(NOW - 1000 - i * 300).toISOString(),
        );
        const html = arrivals(many);
        expect(html).not.toContain('data-mark');
        expect(html).toContain('data-slot');
        expect(html).toContain('density: up to 300 ticks a row');
    });

    it('counts a curve’s pillars published at one instant as one tick', () => {
        const instant = new Date(NOW - 10_000).toISOString();
        const html = arrivals(Array.from({ length: 12 }, () => instant));
        expect(html.match(/data-mark/g)).toHaveLength(1);
        expect(html).toMatch(/>1<\/span><span[^>]*>\/ /);
    });

    it('measures the pace over the time the trace covers, not its window', () => {
        const events = [
            event({ at: new Date(NOW - 40_000).toISOString(), tau: 0 }),
            event({ at: new Date(NOW).toISOString(), tau: 20 }),
        ];
        expect(meanPaceOf(events)).toBeCloseTo(0.5);
        expect(meanPaceOf([])).toBe(1);
    });

    it('asks for ticks only over the stretch the ticks cover', () => {
        const at = (ago: number): string => new Date(NOW - ago * 1000).toISOString();
        expect(
            coveredSeconds([{ ticks: [at(30), at(5)] }, { ticks: [at(10)] }], 120, NOW),
        ).toBeCloseTo(30);
        expect(coveredSeconds([{ ticks: [at(300)] }], 120, NOW)).toBe(0);
        expect(coveredSeconds([{ ticks: [at(200), at(119)] }], 120, NOW)).toBeCloseTo(119);
    });

    it('names the provider, which tells two feeds of one pair apart', () => {
        expect(providerOf('synthetic.bloomberg.audusd')).toBe('bloomberg');
        expect(providerOf('live.eurusd')).toBe('');
    });

    it('counts ticks into slots across the window', () => {
        const counts = slotCounts([
            { position: 0, count: 1 },
            { position: 0.999, count: 3 },
            { position: 1, count: 1 },
        ]);
        expect(counts).toHaveLength(LANE_SLOTS);
        expect(counts[0]).toBe(1);
        expect(counts[LANE_SLOTS - 1]).toBe(4);
    });
});

describe('the Laws tab', () => {
    it('joins each running law to its feed and links to the feed’s Arrivals tab', () => {
        const rows = lawRows(trace(), raster());
        expect(rows.map((row) => row.feed?.label)).toEqual(['EUR/USD', 'USD SOFR']);
        const html = render(<LawsView trace={trace()} raster={raster()} now={NOW} />);
        expect(html).toContain('href="/market-data/synthetic/fx/feed-1?tab=arrivals"');
        expect(html).toContain('href="/market-data/synthetic/ir/feed-2?tab=arrivals"');
        expect(html).toContain('stable');
        expect(html).not.toContain('<input');
    });

    it('calls a Hawkes law that would not settle unstable, and the memoryless laws always stable', () => {
        expect(lawStable(law({ branchingRatio: 0.6 }))).toBe(true);
        expect(lawStable(law({ branchingRatio: 1.2 }))).toBe(false);
        expect(lawStable(law({ mode: 'poisson', branchingRatio: 0 }))).toBe(true);
    });
});

describe('the Fit tab', () => {
    it('places seen against asked on a log scale, one in the middle', () => {
        expect(ratioPosition(1)).toBe(50);
        expect(ratioPosition(2)).toBe(75);
        expect(ratioPosition(0.5)).toBe(25);
    });

    it('lists each feed against its law and the loop’s health', () => {
        const html = render(<FitView regime={regime()} trace={trace()} />);
        expect(html).toContain('live.eurusd');
        expect(html).toContain('Run seed');
        expect(html).toContain('42');
    });
});

describe('the Trace tab', () => {
    it('filters by kind and by feed, newest first', () => {
        const events = trace().events;
        expect(traceRows(events, new Set(['tick']), '').map((e) => e.tau)).toEqual([10, 5]);
        expect(traceRows(events, new Set(['tick', 'pin']), 'usd').map((e) => e.kind)).toEqual([
            'tick',
            'tick',
        ]);
        expect(traceRows(events, new Set(['pin']), '')).toHaveLength(1);
    });

    it('draws the decisions with their kinds', () => {
        const html = render(<TraceView trace={trace()} />);
        expect(html).toContain('4 decisions match');
        expect(html).toContain('turn');
    });
});

describe('the folded hour', () => {
    it('puts each second in the column of its minute and the row of its place in it', () => {
        const picture = foldHistory(history(), 'second', 'all', NOW);
        expect(picture.cells).toHaveLength(11);
        const minute = Math.floor((Math.floor(NOW / 1000) - 5) / 60);
        const latest = picture.cells.find((cell) => cell.value === 2);
        expect(latest?.column).toBe(minute - (Math.floor(Math.floor(NOW / 1000) / 60) - 59));
        expect(latest?.row).toBe((Math.floor(NOW / 1000) - 5) % 60);
    });

    it('leaves out what is older than the hour, and counts ticks a second', () => {
        const picture = foldHistory(history(), 'second', 'all', NOW);
        expect(picture.cells.some((cell) => cell.value === 99)).toBe(false);
        expect(picture.cells.filter((cell) => cell.value === 9)).toHaveLength(10);
    });

    it('colours by one class, or by the pace', () => {
        const rates = foldHistory(history(), 'second', { group: 'interest_rates' }, NOW);
        expect(rates.cells.filter((cell) => cell.value === 1)).toHaveLength(10);
        expect(rates.cells.find((cell) => cell.value === 2)).toBeUndefined();
        const pace = foldHistory(history(), 'second', 'pace', NOW);
        expect(pace.cells.filter((cell) => cell.value === 2)).toHaveLength(10);
    });

    it('marks the minutes the market was active', () => {
        const picture = foldHistory(history(), 'second', 'all', NOW);
        expect(picture.activeShares.some((share) => share > 0.9)).toBe(true);
        expect(picture.activeShares.at(-1)).toBe(0);
    });

    it('scales a tenth by its length, so a cell still reads in ticks a second', () => {
        const tenths: MarketHistory = {
            slotSeconds: 0.1,
            groups: ['fx'],
            bins: [
                { at: new Date(NOW - 2000).toISOString(), counts: [3], pace: 1, activeShare: 0 },
            ],
        };
        expect(foldHistory(tenths, 'tenth', 'all', NOW).cells[0]?.value).toBeCloseTo(30);
    });

    it('draws a cell for each slot that had ticks or a reading', () => {
        const html = render(
            <FoldCard
                history={history()}
                resolution="second"
                onResolution={() => undefined}
                now={NOW}
            />,
        );
        expect(html.match(/data-cell/g)).toHaveLength(11);
        expect(html).toContain('The last hour, folded by the minute');
    });
});

describe('the intensity on Now', () => {
    it('counts the ticks of every class in each second of the window', () => {
        const seen = ticksBySecond(history(), 120, NOW);
        expect(seen).toHaveLength(11);
        expect(seen.at(-1)?.ticks).toBe(2);
    });

    it('draws λ(t) over the ticks seen, on one axis', () => {
        const samples = [
            { at: new Date(NOW - 20_000).toISOString(), heat: 0.1, intensity: 6 },
            { at: new Date(NOW - 10_000).toISOString(), heat: 0.1, intensity: 9 },
        ];
        const html = render(
            <NowView
                regime={regime({ samples })}
                trace={trace()}
                history={history()}
                now={NOW}
                mayPin
                pending={undefined}
                error={null}
                onPin={() => undefined}
                onOpenTrace={() => undefined}
            />,
        );
        expect(html).toContain('data-lambda');
        expect(html.match(/data-seen/g)).toHaveLength(11);
    });
});
