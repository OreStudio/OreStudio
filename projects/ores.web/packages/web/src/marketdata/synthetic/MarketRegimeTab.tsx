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
    type MarketTrace,
    type RegimeMode,
    type TraceFeed,
} from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { useSession } from '../../session/SessionProvider.js';
import { Button, Fact, Notice, Tag } from '../../ui/Primitives.js';
import { assetClassColour } from '../assetClassColour.js';
import {
    arrivalPositions,
    curvePath,
    heatSeries,
    markGroups,
    multiplierText,
    rateCurves,
    RASTER_SECONDS,
    stateBands,
    type StateSample,
} from './marketRaster.js';
import {
    commandLog,
    exponentialQuantiles,
    feedFit,
    lawText,
    paceSteps,
    QQ_MINIMUM_GAPS,
    tauPositions,
    tauSpan,
    traceBands,
} from './marketTrace.js';
import {
    REGIME_KEY,
    stateName,
    useMarketRegimeQuery,
    useMarketTrace,
    useStateTrace,
} from './useMarketRegime.js';

/** How often the raster is read again, in milliseconds. Slower than the state:
 * it carries a row for every running feed, and a mark that arrives two seconds
 * late still falls where it belongs in the window. */
const RASTER_EVERY_MS = 4_000;

const RASTER_KEY = ['market-regime-raster'] as const;

/** A lane's height in pixels: a row of the raster, and the room a mark takes. */
const LANE_HEIGHT = 18;

/** The room each of the two factor charts takes, in pixels: big enough to read a decay. */
const CHART_HEIGHT = 84;

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
    const session = useSession();
    // The market is the installation's, so only a system session sets it; a tenant reads it.
    const mayPin =
        session.state.status === 'authenticated' &&
        session.state.session.mode === 'system-administration';
    const regime = useMarketRegimeQuery();
    const states = useStateTrace(regime.dataUpdatedAt, regime.data?.state);
    const trace = useMarketTrace();
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
            trace={trace.data}
            mayPin={mayPin}
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
    trace,
    mayPin = true,
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
    /** The scheduler's own record, which the pace, the bands, the fit and the log are drawn from. */
    readonly trace?: MarketTrace | undefined;
    /** Whether this session may set the market: the system tenant's, and no other. */
    readonly mayPin?: boolean;
    readonly onPin: (mode: RegimeMode) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [axis, setAxis] = useState<'clock' | 'tau'>('clock');
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

            {trace === undefined ? null : <SchedulerFacts regime={regime} trace={trace} />}

            {error === null ? null : <Notice tone="error">{error}</Notice>}

            {mayPin ? (
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
            ) : (
                <p className="text-xs text-ink-faint">{t('marketdata.marketRegime.readOnly')}</p>
            )}

            <FactorCharts
                feeds={raster?.feeds ?? []}
                seconds={raster?.seconds ?? RASTER_SECONDS}
                now={now}
                regime={regime}
                trace={trace}
            />

            <div className="space-y-2">
                <div className="flex flex-wrap items-center gap-2">
                    <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                        {t('marketdata.marketRegime.raster.title', {
                            seconds: raster?.seconds ?? RASTER_SECONDS,
                        })}
                    </h3>
                    {trace === undefined ? null : (
                        <span
                            role="radiogroup"
                            aria-label={t('marketdata.marketRegime.raster.axis.label')}
                            className="ml-auto flex items-center gap-1"
                        >
                            {(['clock', 'tau'] as const).map((option) => (
                                <Button
                                    key={option}
                                    size="sm"
                                    role="radio"
                                    variant={axis === option ? 'primary' : 'secondary'}
                                    aria-checked={axis === option}
                                    onClick={() => setAxis(option)}
                                >
                                    {t(`marketdata.marketRegime.raster.axis.${option}`)}
                                </Button>
                            ))}
                        </span>
                    )}
                </div>
                <MarketRaster
                    feeds={raster?.feeds ?? []}
                    seconds={raster?.seconds ?? RASTER_SECONDS}
                    now={now}
                    marked={marked}
                    states={states}
                    trace={trace}
                    axis={axis}
                />
                {warming ? (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.marketRegime.raster.warming')}
                    </p>
                ) : null}
            </div>

            {trace === undefined ? null : <FeedFits trace={trace} />}
            {trace === undefined ? null : <CommandLog trace={trace} />}
        </section>
    );
}

/** The run's seed, the heat's loop gain against one, and how the scheduler's loop is doing. */
function SchedulerFacts({
    regime,
    trace,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <dl className="grid gap-4 sm:grid-cols-4 lg:grid-cols-7">
            <Fact label={t('marketdata.marketRegime.pace')} value={`×${regime.pace.toFixed(2)}`} />
            <Fact
                label={t('marketdata.marketRegime.loopGain')}
                value={t('marketdata.marketRegime.loopGainValue', {
                    gain: regime.loopGain.toFixed(3),
                })}
            />
            <Fact label={t('marketdata.marketRegime.runSeed')} value={String(trace.runSeed)} />
            <Fact
                label={t('marketdata.marketRegime.speed')}
                value={t('marketdata.marketRegime.speedValue', { speed: regime.speed })}
            />
            <Fact
                label={t('marketdata.marketRegime.ticksPerSecond')}
                value={trace.ticksPerSecond.toFixed(1)}
            />
            <Fact label={t('marketdata.marketRegime.heapSize')} value={String(trace.heapSize)} />
            <Fact
                label={t('marketdata.marketRegime.lateness')}
                value={t('marketdata.marketRegime.latenessValue', {
                    smoothed: (trace.latenessSeconds * 1000).toFixed(1),
                    most: (trace.maxLatenessSeconds * 1000).toFixed(1),
                })}
            />
        </dl>
    );
}

/** The pixels a QQ plot takes on each side. */
const QQ_SIZE = 160;

/**
 * Each feed's ticks over the window against its law: how many it made and how many its law
 * asks for, and for the feed a person picks, its compensated gaps against Exp(1).
 */
function FeedFits({ trace }: { readonly trace: MarketTrace }): ReactNode {
    const { t } = useTranslation();
    const [picked, setPicked] = useState<string | undefined>(undefined);
    const span = tauSpan(trace.events);
    if (span === undefined || trace.feeds.length === 0) {
        return null;
    }
    const fits = trace.feeds.map((feed) => ({ feed, fit: feedFit(trace.events, feed, span) }));
    // Until a person picks one, the feed with the most ticks, whose plot says the most.
    const busiest = fits.reduce<(typeof fits)[number] | undefined>(
        (best, entry) =>
            best === undefined || entry.fit.observed > best.fit.observed ? entry : best,
        undefined,
    );
    const chosen = fits.find((entry) => entry.feed.sourceName === picked) ?? busiest;
    const points = chosen === undefined ? [] : exponentialQuantiles(chosen.fit.gaps);
    const top = points.reduce((high, point) => Math.max(high, point.expected, point.observed), 1);
    return (
        <section
            aria-label={t('marketdata.marketRegime.fit.title')}
            className="space-y-2 rounded-md border border-line bg-surface-base p-3"
        >
            <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.marketRegime.fit.title')}
            </h3>
            <p className="text-[11px] text-ink-faint">{t('marketdata.marketRegime.fit.lead')}</p>
            <div className="flex flex-wrap items-start gap-4">
                <div className="max-h-72 overflow-y-auto">
                    <table className="text-[11px]">
                        <thead className="text-ink-faint">
                            <tr>
                                <th className="pr-3 text-left font-normal">
                                    {t('marketdata.marketRegime.fit.feed')}
                                </th>
                                <th className="pr-3 text-right font-normal">
                                    {t('marketdata.marketRegime.fit.observed')}
                                </th>
                                <th className="pr-3 text-right font-normal">
                                    {t('marketdata.marketRegime.fit.expected')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {fits.map(({ feed, fit }) => (
                                <tr
                                    key={feed.sourceName}
                                    className={`cursor-pointer ${
                                        chosen?.feed.sourceName === feed.sourceName
                                            ? 'bg-ink/10'
                                            : ''
                                    }`}
                                    onClick={() => setPicked(feed.sourceName)}
                                >
                                    <td className="pr-3">{feed.sourceName}</td>
                                    <td className="pr-3 text-right font-mono tabular-nums">
                                        {fit.observed}
                                    </td>
                                    <td className="pr-3 text-right font-mono tabular-nums">
                                        {fit.expected.toFixed(1)}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
                {chosen === undefined ? null : points.length < QQ_MINIMUM_GAPS ? (
                    <p className="max-w-[160px] text-[10px] text-ink-faint">
                        {t('marketdata.marketRegime.fit.tooFew', {
                            feed: chosen.feed.sourceName,
                            minimum: QQ_MINIMUM_GAPS,
                        })}
                    </p>
                ) : (
                    <figure className="space-y-1">
                        <svg
                            role="img"
                            aria-label={t('marketdata.marketRegime.fit.qq', {
                                feed: chosen.feed.sourceName,
                            })}
                            width={QQ_SIZE}
                            height={QQ_SIZE}
                            viewBox={`0 0 ${String(QQ_SIZE)} ${String(QQ_SIZE)}`}
                            className="text-flow"
                        >
                            <line
                                x1={0}
                                y1={QQ_SIZE}
                                x2={QQ_SIZE}
                                y2={0}
                                className="stroke-ink/30"
                                strokeDasharray="3 3"
                            />
                            {points.map((point, index) => (
                                <circle
                                    key={index}
                                    data-qq
                                    cx={(point.expected / top) * QQ_SIZE}
                                    cy={QQ_SIZE - (point.observed / top) * QQ_SIZE}
                                    r={2}
                                    fill="currentColor"
                                />
                            ))}
                        </svg>
                        <figcaption className="max-w-[160px] text-[10px] text-ink-faint">
                            {t('marketdata.marketRegime.fit.qqCaption', {
                                feed: chosen.feed.sourceName,
                            })}
                        </figcaption>
                    </figure>
                )}
            </div>
        </section>
    );
}

/** The pins and the changes of the feeds over the window, newest first. */
function CommandLog({ trace }: { readonly trace: MarketTrace }): ReactNode {
    const { t } = useTranslation();
    const log = commandLog(trace.events);
    return (
        <section
            aria-label={t('marketdata.marketRegime.log.title')}
            className="space-y-1 rounded-md border border-line bg-surface-base p-3"
        >
            <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.marketRegime.log.title')}
            </h3>
            {log.length === 0 ? (
                <p className="text-[11px] text-ink-faint">
                    {t('marketdata.marketRegime.log.none')}
                </p>
            ) : (
                <ul className="space-y-0.5 text-[11px]">
                    {log.map((event, index) => (
                        <li key={index} className="flex gap-3 font-mono tabular-nums">
                            <span className="text-ink-faint">
                                {new Date(event.at).toLocaleTimeString()}
                            </span>
                            <span>{t(`marketdata.marketRegime.log.${event.kind}`)}</span>
                            <span className="text-ink-muted">
                                {event.sourceName === ''
                                    ? stateName(event.state, t)
                                    : event.sourceName}
                            </span>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}

/**
 * The two halves of the factor, over the window, in one section above the
 * arrivals they moved.
 *
 * The heat is what jump and decay act on, so the spikes and the falls are the
 * process at work; the rate is what the arrivals came at, per asset class. Both
 * share the raster's time axis and its label column, so the three pictures read
 * as one.
 */
export function FactorCharts({
    feeds,
    seconds,
    now,
    regime,
    trace,
}: {
    readonly feeds: FeedRaster['feeds'];
    readonly seconds: number;
    readonly now: number;
    /** The factor in force, which carries the heat the service sampled. */
    readonly regime: MarketRegime;
    /** The scheduler's record, which carries the pace at every decision. */
    readonly trace?: MarketTrace | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const rates = rateCurves(feeds, seconds, now);
    const heat = heatSeries(regime.samples, seconds, now);
    const heatPeak = heat.reduce((high, value) => Math.max(high, value), 0);
    const pace =
        trace === undefined ? { steps: [], peak: 0 } : paceSteps(trace.events, seconds, now);
    if (heat.length < 2 && rates.curves.length === 0 && pace.steps.length === 0) {
        return null;
    }
    return (
        <section
            aria-label={t('marketdata.marketRegime.charts.title')}
            className="space-y-2 rounded-md border border-line bg-surface-base p-3"
        >
            <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.marketRegime.charts.title')}
            </h3>
            {pace.steps.length === 0 ? null : (
                <div className="flex items-stretch gap-3" style={{ height: CHART_HEIGHT }}>
                    <span className="w-44 shrink-0 self-end pb-1 text-[10px] text-ink-faint">
                        {t('marketdata.marketRegime.charts.pace')}
                        <span className="mt-1 flex flex-col gap-0.5">
                            <span className="flex items-center gap-1">
                                <span className="h-2 w-3 rounded-sm bg-ink/30" />
                                {t('marketdata.marketRegime.charts.paceRegime')}
                            </span>
                            <span className="flex items-center gap-1">
                                <span className="h-2 w-3 rounded-sm bg-flow/60" />
                                {t('marketdata.marketRegime.charts.paceHeat')}
                            </span>
                        </span>
                    </span>
                    <svg
                        role="img"
                        aria-label={t('marketdata.marketRegime.charts.pace')}
                        className="flex-1"
                        viewBox="0 0 100 1"
                        preserveAspectRatio="none"
                    >
                        {pace.steps.map((step, index) => {
                            const regimeHeight = step.multiplier / pace.peak;
                            const heatHeight = step.heat / pace.peak;
                            return (
                                <g key={index} data-pace>
                                    <rect
                                        x={step.from}
                                        width={Math.max(0, step.to - step.from)}
                                        y={1 - regimeHeight}
                                        height={regimeHeight}
                                        className="fill-ink/30"
                                    />
                                    <rect
                                        x={step.from}
                                        width={Math.max(0, step.to - step.from)}
                                        y={1 - regimeHeight - heatHeight}
                                        height={heatHeight}
                                        className="fill-flow/60"
                                    />
                                </g>
                            );
                        })}
                    </svg>
                    <span className="w-12 shrink-0 self-end pb-1 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                        {`×${regime.pace.toFixed(2)}`}
                    </span>
                </div>
            )}
            {heat.length < 2 ? null : (
                <div className="flex items-stretch gap-3" style={{ height: CHART_HEIGHT }}>
                    <span className="w-44 shrink-0 self-end pb-1 text-[10px] text-ink-faint">
                        {t('marketdata.marketRegime.raster.heat')}
                    </span>
                    <svg
                        aria-hidden
                        className="flex-1 text-flow"
                        viewBox="0 0 100 1"
                        preserveAspectRatio="none"
                    >
                        <path
                            d={curvePath(heat, heatPeak)}
                            fill="none"
                            stroke="currentColor"
                            strokeWidth="2"
                            vectorEffect="non-scaling-stroke"
                        />
                    </svg>
                    <span className="w-12 shrink-0 self-end pb-1 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                        {regime.heat.toFixed(2)}
                    </span>
                </div>
            )}
            {rates.curves.length === 0 ? null : (
                <div className="flex items-stretch gap-3" style={{ height: CHART_HEIGHT }}>
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
                                className={assetClassColour(curve.assetClass).text}
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="2"
                                vectorEffect="non-scaling-stroke"
                            />
                        ))}
                    </svg>
                    <span className="w-12 shrink-0 self-end pb-1 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                        {`${rates.peak.toFixed(1)}/s`}
                    </span>
                </div>
            )}
            <div className="flex items-center gap-3">
                <span className="w-44 shrink-0" />
                <span className="flex flex-1 justify-between font-mono text-[10px] text-ink-faint">
                    <span>{`\u2212${String(seconds)}s`}</span>
                    <span>{`\u2212${String(Math.round((seconds * 2) / 3))}s`}</span>
                    <span>{`\u2212${String(Math.round(seconds / 3))}s`}</span>
                    <span>{t('marketdata.marketRegime.raster.now')}</span>
                </span>
                <span className="w-12 shrink-0" />
            </div>
            <p className="text-[10px] text-ink-faint">
                {t('marketdata.marketRegime.raster.heatParams', {
                    jump: regime.jump,
                    decay: regime.decay,
                })}
            </p>
        </section>
    );
}

/**
 * One row for each feed, and a dot for each tick it published.
 *
 * The feeds that run are drawn first, because the picture opens on the market;
 * a stopped feed keeps its row, opened by the reader, in the count that says so.
 * A row is a name in its class's colour, a track of dots, and a count.
 */
export function MarketRaster({
    feeds,
    seconds,
    now,
    marked,
    states,
    trace,
    axis = 'clock',
}: {
    readonly feeds: FeedRaster['feeds'];
    /** The window the rows were read over, which is what the marks are placed in. */
    readonly seconds: number;
    readonly now: number;
    readonly marked: { readonly at: number; readonly state: MarketState } | null;
    /** The state over the window, drawn as the band the arrivals sit on. */
    readonly states: readonly StateSample[];
    /** The scheduler's record: the bands, the market-time marks and each feed's law. */
    readonly trace?: MarketTrace | undefined;
    /** Whether the marks are placed by clock time or by market time, tau. */
    readonly axis?: 'clock' | 'tau';
}): ReactNode {
    const { t } = useTranslation();
    if (feeds.length === 0) {
        return <Notice tone="info">{t('marketdata.marketRegime.raster.none')}</Notice>;
    }
    const span = trace === undefined ? undefined : tauSpan(trace.events);
    const byTau = axis === 'tau' && trace !== undefined && span !== undefined;
    const markAt =
        marked === null || byTau
            ? undefined
            : arrivalPositions([new Date(marked.at).toISOString()], seconds, now)[0];
    // In market time the clock's bands do not line up with the marks, so none are drawn.
    const bands = byTau
        ? []
        : trace === undefined
          ? stateBands(states, seconds, now)
          : traceBands(trace.events, seconds, now);
    const laws = new Map((trace?.feeds ?? []).map((feed) => [feed.sourceName, feed]));
    const positionsOf = (sourceName: string): readonly number[] | undefined =>
        byTau && trace !== undefined && span !== undefined
            ? tauPositions(trace.events, sourceName, span)
            : undefined;
    const running = feeds.filter((feed) => feed.running);
    const stopped = feeds.filter((feed) => !feed.running);
    return (
        <div
            role="img"
            aria-label={t('marketdata.marketRegime.raster.label')}
            className="rounded-md border border-line bg-surface-base py-2"
        >
            {running.map((feed) => (
                <RasterRow
                    key={feed.sourceName}
                    feed={feed}
                    seconds={seconds}
                    now={now}
                    markAt={markAt}
                    bands={bands}
                    positions={positionsOf(feed.sourceName)}
                    law={laws.get(feed.sourceName)}
                    showLaw={trace !== undefined}
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
                            showLaw={trace !== undefined}
                        />
                    ))}
                </details>
            )}
            <div className="flex items-center gap-3 px-3 pt-1">
                <span className="w-44 shrink-0" />
                {byTau && span !== undefined ? (
                    <span className="flex flex-1 justify-between font-mono text-[10px] text-ink-faint">
                        <span>{`\u03c4 ${span.from.toFixed(1)}`}</span>
                        <span>{`\u03c4 ${span.to.toFixed(1)}`}</span>
                    </span>
                ) : (
                    <span className="flex flex-1 justify-between font-mono text-[10px] text-ink-faint">
                        <span>{`\u2212${String(seconds)}s`}</span>
                        <span>{`\u2212${String(Math.round((seconds * 2) / 3))}s`}</span>
                        <span>{`\u2212${String(Math.round(seconds / 3))}s`}</span>
                        <span>{t('marketdata.marketRegime.raster.now')}</span>
                    </span>
                )}
                {trace === undefined ? null : (
                    <span className="w-56 shrink-0 text-[9px] text-ink-faint">
                        {t('marketdata.marketRegime.raster.lawColumn')}
                    </span>
                )}
                <span className="w-10 shrink-0 text-right text-[9px] text-ink-faint">
                    {t('marketdata.marketRegime.raster.seen')}
                </span>
                <span className="w-14 shrink-0 text-right text-[9px] text-ink-faint">
                    {t('marketdata.marketRegime.raster.asked')}
                </span>
            </div>
            <div className="flex flex-wrap items-center gap-4 px-3 pt-1 text-[10px] text-ink-faint">
                <span className="flex items-center gap-1">
                    <span className="size-1.5 rounded-full bg-class-fx" />
                    {t('marketdata.marketRegime.raster.fx')}
                </span>
                <span className="flex items-center gap-1">
                    <span className="size-1.5 rounded-full bg-class-interest-rates" />
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
    positions,
    law,
    showLaw = false,
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
    /** The marks placed by market time, which replace the clock's when they are given. */
    readonly positions?: readonly number[] | undefined;
    /** The feed's law and its state now, when the scheduler's record is read. */
    readonly law?: TraceFeed | undefined;
    /** Whether the raster has a law column, which a row keeps even when it has no law. */
    readonly showLaw?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const groups =
        positions === undefined
            ? markGroups(feed.ticks, seconds, now)
            : positions.map((position) => ({ position, count: 1 }));
    return (
        <div
            className="flex items-center gap-3 px-3"
            style={{ height: LANE_HEIGHT }}
            title={feed.sourceName}
        >
            <span
                className={`w-44 shrink-0 truncate text-[11px] ${assetClassColour(feed.assetClass).text} ${
                    feed.running ? '' : 'opacity-50'
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
                {groups.map((group, index) => (
                    <span
                        key={index}
                        className="absolute top-1/2 -translate-x-1/2 -translate-y-1/2"
                        style={{ left: `${String(group.position * 100)}%` }}
                    >
                        {group.count > 1 ? (
                            // Several arrivals at one instant: a ring, because a
                            // dozen dots in one place read as one.
                            <span
                                data-coincident
                                className={`absolute top-1/2 left-1/2 size-3.5 -translate-x-1/2 -translate-y-1/2 rounded-full border opacity-70 ${assetClassColour(feed.assetClass).text}`}
                            />
                        ) : null}
                        <span
                            data-mark
                            className={`block size-1.5 rounded-full ${assetClassColour(feed.assetClass).dot}`}
                        />
                    </span>
                ))}
                {markAt === undefined ? null : (
                    <span
                        className="absolute inset-y-0 w-px bg-ink/50"
                        style={{ left: `${String(markAt * 100)}%` }}
                    />
                )}
            </span>
            {showLaw ? (
                <span
                    data-law
                    className="w-56 shrink-0 truncate font-mono text-[10px] text-ink-faint"
                    title={law === undefined ? undefined : lawText(law, t)}
                >
                    {law === undefined ? '' : lawText(law, t)}
                </span>
            ) : null}
            <span className="w-10 shrink-0 text-right font-mono text-[10px] tabular-nums text-ink-faint">
                {feed.running ? groups.length : t('marketdata.marketRegime.raster.stopped')}
            </span>
            <span className="w-14 shrink-0 text-right font-mono text-[10px] tabular-nums text-ink-faint/70">
                {`${feed.expectedPerSecond.toFixed(2)}/s`}
            </span>
        </div>
    );
}
