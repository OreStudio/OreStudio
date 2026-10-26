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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import {
    api,
    REGIME_MODES,
    type FeedRaster,
    type MarketRegime,
    type MarketState,
    type RegimeMode,
} from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Button, Fact, Notice, Tag } from '../../ui/Primitives.js';
import {
    arrivalPositions,
    curvePath,
    multiplierText,
    rateCurves,
    RASTER_SECONDS,
    stateBands,
    type StateSample,
} from './marketRaster.js';
import { REGIME_KEY, stateName, useMarketRegimeQuery, useStateTrace } from './useMarketRegime.js';

/** How often the raster is read again, in milliseconds. Slower than the state:
 * it carries a row for every running feed, and a mark that arrives two seconds
 * late still falls where it belongs in the window. */
const RASTER_EVERY_MS = 4_000;

const RASTER_KEY = ['market-regime-raster'] as const;

/** A lane's height in pixels: a row of the raster, and the room a mark takes. */
const LANE_HEIGHT = 18;

/** The room the rate curves take above the rows. */
const RATE_HEIGHT = 48;

/**
 * The dot colour of a feed's class. A class this screen does not know is drawn
 * in the accent, so a new asset class appears at once rather than not at all.
 */
function colourOf(assetClass: string): string {
    if (assetClass === 'interest_rates') {
        return 'bg-ir';
    }
    return 'bg-fx';
}

/** The same colour, for a line rather than a dot. */
function colourTextOf(assetClass: string): string {
    return colourOf(assetClass).replace('bg-', 'text-');
}

/**
 * The market-wide arrival factor: the state every feed's rate is scaled by, the
 * switch that forces it, and the arrivals of every running feed on one raster.
 *
 * The factor belongs to the run, so this is the whole market's control and not a
 * feed's: it sets the state and reads the feeds. It never edits a feed's own
 * clustering, which is that feed's Arrivals tab, so the two views never do the
 * same job.
 */
export function MarketRegimeTab(): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [marked, setMarked] = useState<{
        readonly at: number;
        readonly state: MarketState;
    } | null>(null);
    const regime = useMarketRegimeQuery();
    const states = useStateTrace(regime.dataUpdatedAt, regime.data?.state);
    const raster = useQuery({
        queryKey: RASTER_KEY,
        queryFn: () => api.feedRaster({ seconds: RASTER_SECONDS }),
        refetchInterval: RASTER_EVERY_MS,
    });
    const pin = useMutation({
        mutationFn: (mode: RegimeMode) => api.pinMarketRegime(mode),
        onSuccess: (after: MarketRegime) => {
            queries.setQueryData(REGIME_KEY, after);
            // When a switch lands, so the picture says which marks came after it.
            setMarked({ at: Date.now(), state: after.state });
        },
    });

    if (regime.data === undefined) {
        return regime.error === null ? (
            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
        ) : (
            <Notice tone="error">{regime.error.message}</Notice>
        );
    }
    return (
        <MarketRegimePanel
            regime={regime.data}
            raster={raster.data}
            now={Date.now()}
            pending={pin.isPending ? pin.variables : undefined}
            error={pin.error === null ? null : pin.error.message}
            marked={marked}
            states={states}
            onPin={(mode) => pin.mutate(mode)}
        />
    );
}

/**
 * The market's state, the switch and the raster, drawn from what the service
 * says. It is given its data so a test renders the same picture without a
 * browser, and so the connected tab holds no rendering of its own.
 */
export function MarketRegimePanel({
    regime,
    raster,
    now,
    pending,
    error,
    marked,
    states,
    onPin,
}: {
    readonly regime: MarketRegime;
    readonly raster: FeedRaster | undefined;
    readonly now: number;
    readonly pending: RegimeMode | undefined;
    readonly error: string | null;
    /** Where a switch landed, so the marks before and after it are told apart. */
    readonly marked: { readonly at: number; readonly state: MarketState } | null;
    /** The state over the window, so the arrivals sit under the market that produced them. */
    readonly states: readonly StateSample[];
    readonly onPin: (mode: RegimeMode) => void;
}): ReactNode {
    const { t } = useTranslation();
    const forced = regime.pinned ? regime.state : 'auto';
    // A row is empty until its feed ticks after the tab opened, so a picture with
    // no mark at all says why rather than reading as a market at a standstill.
    const warming =
        raster !== undefined &&
        raster.feeds.length > 0 &&
        raster.feeds.every((feed) => feed.ticks.length === 0);
    return (
        <section className="space-y-5" aria-label={t('marketdata.marketRegime.title')}>
            <p className="text-sm text-ink-muted">{t('marketdata.marketRegime.lead')}</p>

            <dl className="grid gap-4 sm:grid-cols-3">
                <Fact label={t('marketdata.marketRegime.state')}>
                    <span className="flex flex-wrap items-center gap-2">
                        <Tag tone={regime.state === 'active' ? 'accent' : 'neutral'}>
                            {stateName(regime.state, t)}
                        </Tag>
                        {regime.pinned ? (
                            <Tag tone="warn" small>
                                {t('marketdata.marketRegime.forced')}
                            </Tag>
                        ) : null}
                    </span>
                </Fact>
                <Fact
                    label={t('marketdata.marketRegime.multiplier')}
                    value={multiplierText(regime.multiplier)}
                />
                <Fact
                    label={t('marketdata.marketRegime.heldBy')}
                    value={
                        regime.pinned
                            ? t('marketdata.marketRegime.pinned')
                            : t('marketdata.marketRegime.chain')
                    }
                />
            </dl>

            {error === null ? null : <Notice tone="error">{error}</Notice>}

            <div className="flex flex-wrap items-center gap-2">
                <span className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('marketdata.marketRegime.force')}
                </span>
                {REGIME_MODES.map((mode) => (
                    <Button
                        key={mode}
                        variant={forced === mode ? 'primary' : 'secondary'}
                        size="sm"
                        aria-pressed={forced === mode}
                        pending={pending === mode}
                        disabled={pending !== undefined}
                        onClick={() => onPin(mode)}
                    >
                        {t(`marketdata.marketRegime.set.${mode}`)}
                    </Button>
                ))}
            </div>

            <div className="space-y-2">
                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('marketdata.marketRegime.raster.title', {
                        seconds: raster?.seconds ?? RASTER_SECONDS,
                    })}
                </h3>
                <MarketRaster
                    feeds={raster?.feeds ?? []}
                    seconds={raster?.seconds ?? RASTER_SECONDS}
                    now={now}
                    marked={marked}
                    states={states}
                />
                {warming ? (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.marketRegime.raster.warming')}
                    </p>
                ) : null}
            </div>
        </section>
    );
}

/**
 * One row for each feed, and a dot for each tick it published.
 *
 * The feeds that run come first, because the picture opens on the market; a
 * stopped feed keeps its row in the colour that says so, because a picture that
 * left the silent feeds out could not show that they are silent. A row carries
 * times and not prices: what a market-wide factor changes is when ticks arrive,
 * and the count on the right says how many arrived.
 *
 * A row is a name, a track and a count, drawn as one because a picture of a
 * thousand dots is not one a person reads on a canvas: the name stays text, so
 * it is legible and can be selected.
 */
/**
 * One row for each feed, and a dot for each tick it published.
 *
 * The feeds that run are drawn first, because the picture opens on the market;
 * a stopped feed keeps its row, opened by the reader, in the count that says so,
 * because a picture that left the silent feeds out could not show that they are
 * silent. A row carries times and not prices: what a market-wide factor changes
 * is when ticks arrive, and the count on the right says how many arrived.
 *
 * A row is a name, a track and a count, drawn as one because a picture of a
 * thousand dots is not one a person reads on a canvas: the name stays text, so
 * it is legible and can be selected.
 */
export function MarketRaster({
    feeds,
    seconds,
    now,
    marked,
    states,
}: {
    readonly feeds: FeedRaster['feeds'];
    /** The window the rows were read over, which is what the marks are placed in. */
    readonly seconds: number;
    readonly now: number;
    readonly marked: { readonly at: number; readonly state: MarketState } | null;
    /** The state over the window, drawn as the band the arrivals sit on. */
    readonly states: readonly StateSample[];
}): ReactNode {
    const { t } = useTranslation();
    if (feeds.length === 0) {
        return <Notice tone="info">{t('marketdata.marketRegime.raster.none')}</Notice>;
    }
    const markAt =
        marked === null
            ? undefined
            : arrivalPositions([new Date(marked.at).toISOString()], seconds, now)[0];
    const bands = stateBands(states, seconds, now);
    const rates = rateCurves(feeds, seconds, now);
    const running = feeds.filter((feed) => feed.running);
    const stopped = feeds.filter((feed) => !feed.running);
    return (
        <div
            role="img"
            aria-label={t('marketdata.marketRegime.raster.label')}
            className="rounded-md border border-line bg-surface-base py-2"
        >
            {rates.curves.length === 0 ? null : (
                <div className="flex items-stretch gap-3 px-3 pb-1" style={{ height: RATE_HEIGHT }}>
                    <span className="w-44 shrink-0 self-end pb-1 text-[10px] text-ink-faint">
                        {t('marketdata.marketRegime.raster.rate')}
                    </span>
                    <svg
                        aria-hidden
                        className="flex-1"
                        viewBox="0 0 100 1"
                        preserveAspectRatio="none"
                    >
                        {rates.curves.map((curve) => (
                            <path
                                key={curve.assetClass}
                                d={curvePath(curve.points, rates.peak)}
                                className={colourTextOf(curve.assetClass)}
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="1.5"
                                vectorEffect="non-scaling-stroke"
                            />
                        ))}
                    </svg>
                    <span className="w-12 shrink-0 self-end pb-1 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                        {`${rates.peak.toFixed(1)}/s`}
                    </span>
                </div>
            )}
            {running.map((feed) => (
                <RasterRow
                    key={feed.sourceName}
                    feed={feed}
                    seconds={seconds}
                    now={now}
                    markAt={markAt}
                    bands={bands}
                />
            ))}
            {stopped.length === 0 ? null : (
                <details className="px-3">
                    <summary className="cursor-pointer py-1 text-[11px] text-ink-faint">
                        {t('marketdata.marketRegime.raster.stoppedFeeds', {
                            count: stopped.length,
                        })}
                    </summary>
                    {stopped.map((feed) => (
                        <RasterRow
                            key={feed.sourceName}
                            feed={feed}
                            seconds={seconds}
                            now={now}
                            markAt={markAt}
                            bands={bands}
                        />
                    ))}
                </details>
            )}
            <div className="flex items-center gap-3 px-3 pt-1">
                <span className="w-44 shrink-0" />
                <span className="flex flex-1 justify-between font-mono text-[10px] text-ink-faint">
                    <span>{`\u2212${String(seconds)}s`}</span>
                    <span>{`\u2212${String(Math.round((seconds * 2) / 3))}s`}</span>
                    <span>{`\u2212${String(Math.round(seconds / 3))}s`}</span>
                    <span>{t('marketdata.marketRegime.raster.now')}</span>
                </span>
                <span className="w-12 shrink-0" />
            </div>
            <div className="flex flex-wrap items-center gap-4 px-3 pt-1 text-[10px] text-ink-faint">
                <span className="flex items-center gap-1">
                    <span className="size-1.5 rounded-full bg-fx" />
                    {t('marketdata.marketRegime.raster.fx')}
                </span>
                <span className="flex items-center gap-1">
                    <span className="size-1.5 rounded-full bg-ir" />
                    {t('marketdata.marketRegime.raster.ir')}
                </span>
                <span className="flex items-center gap-1">
                    <span className="h-2 w-3 rounded-sm bg-ink/10" />
                    {t('marketdata.marketRegime.raster.band')}
                </span>
                {marked === null ? null : (
                    <span className="flex items-center gap-1">
                        <span className="h-2 w-px bg-ink/50" />
                        {t('marketdata.marketRegime.raster.marked', {
                            state: stateName(marked.state, t),
                        })}
                    </span>
                )}
            </div>
        </div>
    );
}

/** One feed's lane: its name, a dot for each arrival, and how many arrived. */
function RasterRow({
    feed,
    seconds,
    now,
    markAt,
    bands,
}: {
    readonly feed: FeedRaster['feeds'][number];
    readonly seconds: number;
    readonly now: number;
    readonly markAt: number | undefined;
    readonly bands: readonly {
        readonly from: number;
        readonly to: number;
        readonly state: MarketState;
    }[];
}): ReactNode {
    const { t } = useTranslation();
    const positions = arrivalPositions(feed.ticks, seconds, now);
    return (
        <div
            className="flex items-center gap-3 px-3"
            style={{ height: LANE_HEIGHT }}
            title={feed.sourceName}
        >
            <span
                className={`w-44 shrink-0 truncate text-[11px] ${
                    feed.running ? 'text-ink' : 'text-ink-faint'
                }`}
            >
                {feed.label}
            </span>
            <span className="relative h-2 flex-1">
                {bands.map((band, bandAt) =>
                    band.state === 'active' ? (
                        <span
                            key={bandAt}
                            className="absolute inset-y-0 bg-ink/10"
                            style={{
                                left: `${String(band.from)}%`,
                                width: `${String(band.to - band.from)}%`,
                            }}
                        />
                    ) : null,
                )}
                {positions.map((position, index) => (
                    <span
                        key={index}
                        data-mark
                        className={`absolute top-1/2 size-1.5 -translate-x-1/2 -translate-y-1/2 rounded-full ${colourOf(feed.assetClass)}`}
                        style={{ left: `${String(position * 100)}%` }}
                    />
                ))}
                {markAt === undefined ? null : (
                    <span
                        className="absolute inset-y-0 w-px bg-ink/50"
                        style={{ left: `${String(markAt * 100)}%` }}
                    />
                )}
            </span>
            <span className="w-12 shrink-0 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                {feed.running ? positions.length : t('marketdata.marketRegime.raster.stopped')}
            </span>
        </div>
    );
}
