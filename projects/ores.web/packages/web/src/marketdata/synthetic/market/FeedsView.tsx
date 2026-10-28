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

import { useState, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import type { FeedRaster, MarketTrace, TraceEvent, TraceFeed } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx, Notice } from '../../../ui/Primitives.js';
import { assetClassColour } from '../../assetClassColour.js';
import { processKey, processName, ProcessTag } from '../processSummary.js';
import { markGroups, type StateBand } from '../marketRaster.js';
import { tauPositions, tauSpan, traceBands } from '../marketTrace.js';
import { MarketCard, MicroLabel, Segmented } from './MarketCard.js';
import { FeedLink, type FeedPages } from './FeedLink.js';
import { LAWS, LawTag, type Law } from './LawTag.js';
import { compact } from './numbers.js';

/** The windows a person can look back over, in seconds. */
export const WINDOWS = [30, 60, 120] as const;
export type ArrivalWindow = (typeof WINDOWS)[number];

/**
 * The columns of a lane: the feed, its process and its law, each wide enough to read in full,
 * then the ticks, which take what is left, and the count against what the law asks.
 */
const LANE_COLUMNS = 'grid-cols-[minmax(11rem,14rem)_11rem_9rem_minmax(12rem,1fr)_7.5rem]';

/** The slots a dense lane is cut into. */
export const LANE_SLOTS = 120;

/**
 * The most ticks a lane draws one by one. A mark is about two pixels wide, so past a few
 * hundred in a row they merge into a bar and the row says nothing; a row then draws a count
 * a slot.
 */
export const MARKS_AT_MOST = 250;

export type LaneDraw = 'auto' | 'marks' | 'density';

/**
 * A sequential ramp from the sunk ground through the accent to red: more is brighter. Its top is
 * red, not gold, since gold is the colour of an interest-rate feed.
 */
export const RAMP = ['#16181d', '#1d2a3d', '#26466b', '#3a6ea5', '#58a6ff', '#a7c7e7', '#f85149'];

export function ramp(value: number): string {
    const x = Math.max(0, Math.min(1, value)) * (RAMP.length - 1);
    const i = Math.min(RAMP.length - 2, Math.floor(x));
    const f = x - i;
    const parse = (hex: string): number[] =>
        [1, 3, 5].map((at) => Number.parseInt(hex.slice(at, at + 2), 16));
    const a = parse(RAMP[i] ?? '#000000');
    const b = parse(RAMP[i + 1] ?? '#000000');
    return `rgb(${a.map((c, k) => Math.round(c + ((b[k] ?? c) - c) * f)).join(',')})`;
}

/**
 * The market time that passed for each second of the trace: its tau over the wall time it
 * covers. A trace read soon after a start covers less than its window, so the window is not
 * the divisor.
 */
export function meanPaceOf(events: readonly TraceEvent[]): number {
    let first: TraceEvent | undefined;
    let last: TraceEvent | undefined;
    for (const event of events) {
        if (first === undefined || event.tau < first.tau) {
            first = event;
        }
        if (last === undefined || event.tau > last.tau) {
            last = event;
        }
    }
    if (first === undefined || last === undefined) {
        return 1;
    }
    const wall = (Date.parse(last.at) - Date.parse(first.at)) / 1000;
    return wall > 0 ? (last.tau - first.tau) / wall : 1;
}

/**
 * The seconds of the window the ticks cover. The web server keeps the ticks it saw since it
 * started, so just after a restart the oldest tick is younger than the window, and a count
 * asked over the whole window would call every feed slow.
 */
export function coveredSeconds(
    feeds: readonly { readonly ticks: readonly string[] }[],
    seconds: number,
    now: number,
): number {
    let oldest = now;
    for (const feed of feeds) {
        for (const tick of feed.ticks) {
            const at = Date.parse(tick);
            if (Number.isFinite(at) && at < oldest && now - at <= seconds * 1000) {
                oldest = at;
            }
        }
    }
    return Math.min(seconds, Math.max(0, (now - oldest) / 1000));
}

/** The provider part of a source name, which tells two feeds of one pair apart. */
export function providerOf(sourceName: string): string {
    const parts = sourceName.split('.');
    return parts.length > 2 ? (parts[1] ?? '') : '';
}

/** A lane's ticks counted into slots across the window, oldest first. */
export function slotCounts(
    positions: readonly { position: number; count: number }[],
    slots = LANE_SLOTS,
): number[] {
    const counts = new Array<number>(slots).fill(0);
    for (const { position, count } of positions) {
        const slot = Math.min(slots - 1, Math.max(0, Math.floor(position * slots)));
        counts[slot] = (counts[slot] ?? 0) + count;
    }
    return counts;
}

function Lane({
    feed,
    seconds,
    now,
    bands,
    positions,
    law,
    dense,
    meanPace,
    covered,
    pages,
}: {
    readonly feed: FeedRaster['feeds'][number];
    readonly seconds: number;
    readonly now: number;
    readonly bands: readonly StateBand[];
    readonly positions: readonly number[] | undefined;
    readonly law: TraceFeed | undefined;
    readonly dense: boolean;
    readonly meanPace: number;
    /** The seconds of the window the ticks cover, which is less just after a restart. */
    readonly covered: number;
    readonly pages: FeedPages;
}): ReactNode {
    const { t } = useTranslation();
    const groups =
        positions === undefined
            ? markGroups(feed.ticks, seconds, now)
            : positions.map((position) => ({ position, count: 1 }));
    // A curve publishes a tick for every pillar at one instant, and its law asks for instants.
    const seen = groups.length;
    const sensitivity = law?.sensitivity ?? 1;
    const asked = feed.expectedPerSecond * sensitivity * meanPace * covered;
    const colour = assetClassColour(feed.assetClass);
    // Against what the law asks for a slot at the window's pace: twice as many is the top of
    // the ramp, so a row brightens when it runs ahead of its law.
    const perSlot = Math.max(1e-9, asked / LANE_SLOTS);
    return (
        <div
            className={`grid ${LANE_COLUMNS} items-center border-b border-line-subtle odd:bg-white/[0.012] hover:bg-accent-dim`}
        >
            <span
                className={cx(
                    'flex min-w-0 items-center gap-2 px-2.5 font-mono text-xs',
                    !feed.running && 'opacity-50',
                )}
            >
                <span className={cx('size-[7px] shrink-0 rounded-sm', colour.dot)} />
                <FeedLink sourceName={feed.sourceName} pages={pages} className="truncate">
                    {feed.label}
                </FeedLink>
                <span className="truncate text-[10px] text-ink-faint">
                    {providerOf(feed.sourceName)}
                </span>
            </span>
            <span className="min-w-0 px-2">
                <ProcessTag feed={feed} />
            </span>
            <span className="min-w-0 px-2 font-mono text-[11px] text-ink-muted">
                {law === undefined ? '—' : <LawTag law={law} pages={pages} />}
            </span>
            <svg
                aria-hidden
                viewBox="0 0 100 22"
                preserveAspectRatio="none"
                className="block h-[22px] w-full"
            >
                {bands.map((band, index) =>
                    band.state === 'active' ? (
                        <rect
                            key={index}
                            x={band.from}
                            width={Math.max(0, band.to - band.from)}
                            y={0}
                            height={22}
                            className="fill-warn/12"
                        />
                    ) : null,
                )}
                {dense
                    ? slotCounts(
                          groups.map((group) => ({ position: group.position, count: 1 })),
                      ).map((count, slot) =>
                          count === 0 ? null : (
                              <rect
                                  key={slot}
                                  data-slot
                                  x={(slot * 100) / LANE_SLOTS}
                                  width={100 / LANE_SLOTS + 0.05}
                                  y={3}
                                  height={16}
                                  fill={ramp(count / (2 * perSlot))}
                              />
                          ),
                      )
                    : groups.map((group, index) => (
                          <rect
                              key={index}
                              data-mark
                              x={group.position * 100}
                              width={0.22}
                              y={5}
                              height={12}
                              className={colour.text}
                              fill="currentColor"
                          />
                      ))}
            </svg>
            <span className="flex justify-end gap-1 px-2.5 font-mono text-[11px] tabular-nums">
                {feed.running ? (
                    <>
                        <span className={seen < asked * 0.6 ? 'text-warn' : 'text-ink-muted'}>
                            {compact(seen)}
                        </span>
                        <span className="text-ink-faint">{`/ ${compact(asked)}`}</span>
                    </>
                ) : (
                    <span className="text-ink-faint italic">
                        {t('marketdata.marketRegime.raster.stopped')}
                    </span>
                )}
            </span>
        </div>
    );
}

/** The feeds a law or a process card sends a person to, held in the address. */
export const LAW_PARAM = 'law';
export const PROCESS_PARAM = 'process';

/**
 * The Feeds tab: every feed's ticks over the last minutes, one row a feed on one time axis,
 * the active spells shaded behind them, with the feed's process and law named and their
 * numbers a hover away. A row draws every tick while they fit and a count a slot once they
 * would not, so the picture holds at any tick rate. The rows can be narrowed to one law or
 * one process, which the address carries so a card on the Arrivals tab can link here.
 */
export function FeedsView({
    raster,
    trace,
    now,
    window: seconds,
    onWindow,
    pages,
}: {
    readonly raster: FeedRaster | undefined;
    readonly trace: MarketTrace | undefined;
    readonly now: number;
    readonly window: ArrivalWindow;
    readonly onWindow: (window: ArrivalWindow) => void;
    readonly pages: FeedPages;
}): ReactNode {
    const { t } = useTranslation();
    const [axis, setAxis] = useState<'clock' | 'tau'>('clock');
    const [draw, setDraw] = useState<LaneDraw>('auto');
    const classes = [...new Set((raster?.feeds ?? []).map((feed) => feed.assetClass))].sort();
    const [hidden, setHidden] = useState<ReadonlySet<string>>(new Set());
    const shown = new Set(classes.filter((c) => !hidden.has(c)));
    const [search, setSearch] = useSearchParams();
    const lawAsked = search.get(LAW_PARAM);
    const lawShown: Law | 'all' = (LAWS as readonly string[]).includes(lawAsked ?? '')
        ? (lawAsked as Law)
        : 'all';
    const processShown = search.get(PROCESS_PARAM) ?? '';
    const narrow = (param: string, value: string): void =>
        setSearch(
            (was) => {
                const next = new URLSearchParams(was);
                if (value === '' || value === 'all') {
                    next.delete(param);
                } else {
                    next.set(param, value);
                }
                return next;
            },
            { replace: true },
        );
    const laws = new Map((trace?.feeds ?? []).map((feed) => [feed.sourceName, feed]));
    const feeds = (raster?.feeds ?? [])
        .filter((feed) => shown.has(feed.assetClass))
        .filter((feed) => lawShown === 'all' || laws.get(feed.sourceName)?.mode === lawShown)
        .filter((feed) => processShown === '' || processKey(feed) === processShown);
    const processFeed = (raster?.feeds ?? []).find((feed) => processKey(feed) === processShown);
    const span = trace === undefined ? undefined : tauSpan(trace.events);
    const byTau = axis === 'tau' && trace !== undefined && span !== undefined;
    const bands = byTau || trace === undefined ? [] : traceBands(trace.events, seconds, now);
    const meanPace = trace === undefined ? 1 : meanPaceOf(trace.events);
    const covered = coveredSeconds(raster?.feeds ?? [], seconds, now);
    const most = feeds.reduce(
        (high, feed) => Math.max(high, markGroups(feed.ticks, seconds, now).length),
        0,
    );
    const dense = draw === 'density' || (draw === 'auto' && most > MARKS_AT_MOST);
    const running = feeds.filter((feed) => feed.running);
    const stopped = feeds.filter((feed) => !feed.running);
    const lane = (feed: FeedRaster['feeds'][number]): ReactNode => (
        <Lane
            key={feed.sourceName}
            feed={feed}
            seconds={seconds}
            now={now}
            bands={bands}
            positions={
                byTau && trace !== undefined && span !== undefined
                    ? tauPositions(trace.events, feed.sourceName, span)
                    : undefined
            }
            law={laws.get(feed.sourceName)}
            dense={dense}
            meanPace={meanPace}
            covered={covered}
            pages={pages}
        />
    );
    return (
        <MarketCard
            glyph="laws"
            title={t('marketdata.market.arrivals.title')}
            lead={t('marketdata.market.arrivals.lead')}
        >
            <div className="flex flex-wrap items-center gap-2.5">
                <span className="flex items-center gap-2.5">
                    <MicroLabel>{t('marketdata.market.arrivals.window')}</MicroLabel>
                    <Segmented
                        label={t('marketdata.market.arrivals.window')}
                        options={WINDOWS.map(String) as readonly string[]}
                        value={String(seconds)}
                        onChange={(option) => onWindow(Number(option) as ArrivalWindow)}
                        titleOf={(option) => `${option} s`}
                    />
                </span>
                <span className="flex items-center gap-2.5">
                    <MicroLabel>{t('marketdata.marketRegime.raster.axis.label')}</MicroLabel>
                    <Segmented
                        label={t('marketdata.marketRegime.raster.axis.label')}
                        options={['clock', 'tau'] as const}
                        value={axis}
                        onChange={setAxis}
                        titleOf={(option) => t(`marketdata.marketRegime.raster.axis.${option}`)}
                    />
                </span>
                {classes.length < 2 ? null : (
                    <>
                        <span className="flex items-center gap-2.5">
                            <MicroLabel>{t('marketdata.market.arrivals.show')}</MicroLabel>
                            <Segmented
                                label={t('marketdata.market.arrivals.show')}
                                options={classes}
                                value={shown}
                                onChange={(option) =>
                                    setHidden((was) => {
                                        const next = new Set(was);
                                        if (next.has(option)) {
                                            next.delete(option);
                                        } else if (shown.size > 1) {
                                            next.add(option);
                                        }
                                        return next;
                                    })
                                }
                                titleOf={(option) => t(`marketdata.market.classes.${option}`)}
                            />
                        </span>
                    </>
                )}
                <span className="flex items-center gap-2.5">
                    <MicroLabel>{t('marketdata.market.laws.law')}</MicroLabel>
                    <Segmented
                        label={t('marketdata.market.laws.law')}
                        options={['all', ...LAWS] as const}
                        value={lawShown}
                        onChange={(option) => narrow(LAW_PARAM, option)}
                        titleOf={(option) =>
                            option === 'all'
                                ? t('marketdata.market.arrivals.allLaws')
                                : t(`marketdata.marketRegime.modes.${option}`)
                        }
                    />
                </span>
                {processShown === '' ? null : (
                    <button
                        type="button"
                        onClick={() => narrow(PROCESS_PARAM, '')}
                        className="rounded-chip border border-accent/40 bg-accent-dim px-2 py-0.5 font-mono text-[11px] text-accent hover:border-accent"
                    >
                        {t('marketdata.market.arrivals.onlyProcess', {
                            process:
                                processFeed === undefined
                                    ? processShown
                                    : processName(t, processFeed),
                        })}
                    </button>
                )}
                <span className="flex items-center gap-2.5">
                    <MicroLabel>{t('marketdata.market.arrivals.draw')}</MicroLabel>
                    <Segmented
                        label={t('marketdata.market.arrivals.draw')}
                        options={['auto', 'marks', 'density'] as const}
                        value={draw}
                        onChange={setDraw}
                        titleOf={(option) => t(`marketdata.market.arrivals.draws.${option}`)}
                    />
                </span>
                {draw === 'auto' ? (
                    <span className="ml-auto font-mono text-[11px] text-ink-faint">
                        {dense
                            ? t('marketdata.market.arrivals.saysDensity', { most: compact(most) })
                            : t('marketdata.market.arrivals.saysMarks')}
                    </span>
                ) : null}
            </div>
            {raster === undefined ? (
                <p className="text-[13px] text-ink-muted">{t('common.loading')}</p>
            ) : feeds.length === 0 ? (
                <Notice tone="info">{t('marketdata.marketRegime.raster.none')}</Notice>
            ) : (
                <div
                    role="group"
                    aria-label={t('marketdata.marketRegime.raster.label')}
                    className="rounded-lg border border-line bg-bg-primary"
                >
                    <div
                        className={`grid ${LANE_COLUMNS} rounded-t-lg border-b border-line bg-surface-raised py-1.5`}
                    >
                        <span className="px-2.5">
                            <MicroLabel>{t('marketdata.market.laws.columns.feed')}</MicroLabel>
                        </span>
                        <span className="px-2">
                            <MicroLabel>{t('marketdata.market.laws.columns.process')}</MicroLabel>
                        </span>
                        <span className="px-2">
                            <MicroLabel>{t('marketdata.market.laws.columns.law')}</MicroLabel>
                        </span>
                        <span>
                            <MicroLabel>
                                {byTau && span !== undefined
                                    ? t('marketdata.market.arrivals.tauSpan', {
                                          from: span.from.toFixed(1),
                                          to: span.to.toFixed(1),
                                      })
                                    : t('marketdata.market.arrivals.clockSpan', { seconds })}
                            </MicroLabel>
                        </span>
                        <span className="px-2.5 text-right whitespace-nowrap">
                            <MicroLabel>{t('marketdata.market.arrivals.seenAsked')}</MicroLabel>
                        </span>
                    </div>
                    {running.map(lane)}
                    {stopped.length === 0 ? null : (
                        <details>
                            <summary className="cursor-pointer px-2.5 py-1 text-[11px] text-ink-faint">
                                {t('marketdata.marketRegime.raster.stoppedFeeds', {
                                    count: stopped.length,
                                })}
                            </summary>
                            {stopped.map(lane)}
                        </details>
                    )}
                    <div className={`grid ${LANE_COLUMNS} py-1.5`}>
                        <span />
                        <span />
                        <span />
                        <span className="flex justify-between font-mono text-[10px] text-ink-faint">
                            {byTau ? (
                                <>
                                    <span>{`τ ${span?.from.toFixed(1) ?? ''}`}</span>
                                    <span>{`τ ${span?.to.toFixed(1) ?? ''}`}</span>
                                </>
                            ) : (
                                <>
                                    <span>{`−${String(seconds)}s`}</span>
                                    <span>{`−${String(Math.round(seconds / 2))}s`}</span>
                                    <span>{t('marketdata.market.now.axisNow')}</span>
                                </>
                            )}
                        </span>
                        <span />
                    </div>
                </div>
            )}
            <div className="flex flex-wrap gap-4 font-mono text-[11px] text-ink-muted">
                {classes.map((assetClass) => (
                    <span key={assetClass} className="flex items-center gap-1.5">
                        <span
                            className={cx('size-2.5 rounded-sm', assetClassColour(assetClass).dot)}
                        />
                        {t(`marketdata.market.classes.${assetClass}`)}
                    </span>
                ))}
                <span className="flex items-center gap-1.5">
                    <span className="size-2.5 rounded-sm border border-warn/40 bg-warn/15" />
                    {t('marketdata.market.now.pace.active')}
                </span>
                {dense ? (
                    <span className="flex items-center gap-1.5">
                        <span
                            className="h-2.5 w-10 rounded-sm"
                            style={{ background: `linear-gradient(90deg, ${RAMP.join(',')})` }}
                        />
                        {t('marketdata.market.arrivals.rampLegend')}
                    </span>
                ) : null}
                <span className="text-ink-faint">
                    {t('marketdata.market.arrivals.seenAskedLegend')}
                </span>
            </div>
        </MarketCard>
    );
}
