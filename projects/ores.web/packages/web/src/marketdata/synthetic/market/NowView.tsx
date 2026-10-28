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
import {
    REGIME_MODES,
    type FeedRaster,
    type MarketRegime,
    type MarketTrace,
    type RegimeMode,
    type TraceEvent,
} from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx, Notice } from '../../../ui/Primitives.js';
import { assetClassColour } from '../../assetClassColour.js';
import { curvePath, RASTER_SECONDS, rateCurves } from '../marketRaster.js';
import { paceSteps, traceBands } from '../marketTrace.js';
import { stateName } from '../useMarketRegime.js';
import { FactRows, MarketCard } from './MarketCard.js';
import { clean, duration } from './numbers.js';

/** The decisions that changed the market rather than ticked it, newest first. */
export function marketChanges(events: readonly TraceEvent[], most = 8): readonly TraceEvent[] {
    return [...events]
        .filter((event) => event.kind !== 'tick')
        .sort((left, right) => left.tau - right.tau)
        .slice(-most)
        .reverse();
}

/** How long the market has held the state in force, from the last decision that set it. */
export function heldFor(events: readonly TraceEvent[], now: number): number | undefined {
    let last: number | undefined;
    for (const event of events) {
        if (event.kind === 'transition' || event.kind === 'pin' || event.kind === 'unpin') {
            const at = Date.parse(event.at);
            if (Number.isFinite(at) && (last === undefined || at > last)) {
                last = at;
            }
        }
    }
    return last === undefined ? undefined : (now - last) / 1000;
}

function TimeAxis({ seconds }: { readonly seconds: number }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="flex justify-between font-mono text-[10px] text-ink-faint">
            <span>{`−${String(seconds)}s`}</span>
            <span>{`−${String(Math.round((seconds * 2) / 3))}s`}</span>
            <span>{`−${String(Math.round(seconds / 3))}s`}</span>
            <span>{t('marketdata.market.now.axisNow')}</span>
        </div>
    );
}

/** The pace over the window: the regime's part and the heat's stacked on it, active spells shaded. */
function PaceCard({
    regime,
    trace,
    raster,
    now,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace | undefined;
    readonly raster: FeedRaster | undefined;
    readonly now: number;
}): ReactNode {
    const { t } = useTranslation();
    const seconds = RASTER_SECONDS;
    const pace =
        trace === undefined ? { steps: [], peak: 0 } : paceSteps(trace.events, seconds, now);
    const bands = trace === undefined ? [] : traceBands(trace.events, seconds, now);
    const top = Math.max(2.4, pace.peak) * 1.1;
    const rates = rateCurves(raster?.feeds ?? [], seconds, now);
    return (
        <MarketCard
            glyph="pace"
            title={t('marketdata.market.now.pace.title')}
            lead={t('marketdata.market.now.pace.lead')}
            action={
                <span className="font-mono text-lg font-bold text-accent">{`×${regime.pace.toFixed(2)}`}</span>
            }
        >
            <svg
                role="img"
                aria-label={t('marketdata.market.now.pace.title')}
                viewBox="0 0 100 1"
                preserveAspectRatio="none"
                className="h-48 w-full"
            >
                {bands.map((band, index) =>
                    band.state === 'active' ? (
                        <rect
                            key={`b${String(index)}`}
                            x={band.from}
                            width={Math.max(0, band.to - band.from)}
                            y={0}
                            height={1}
                            className="fill-warn/10"
                        />
                    ) : null,
                )}
                {[1, 2].map((level) => (
                    <line
                        key={level}
                        x1={0}
                        x2={100}
                        y1={1 - level / top}
                        y2={1 - level / top}
                        className="stroke-line-subtle"
                        vectorEffect="non-scaling-stroke"
                    />
                ))}
                {pace.steps.map((step, index) => {
                    const regimeHeight = step.multiplier / top;
                    const heatHeight = step.heat / top;
                    return (
                        <g key={index} data-pace>
                            <rect
                                x={step.from}
                                width={Math.max(0, step.to - step.from)}
                                y={1 - regimeHeight}
                                height={regimeHeight}
                                className="fill-accent/25"
                            />
                            <rect
                                x={step.from}
                                width={Math.max(0, step.to - step.from)}
                                y={1 - regimeHeight - heatHeight}
                                height={heatHeight}
                                className="fill-warn/60"
                            />
                        </g>
                    );
                })}
            </svg>
            <TimeAxis seconds={seconds} />
            <div className="flex flex-wrap gap-4 font-mono text-[11px] text-ink-muted">
                <span className="flex items-center gap-1.5">
                    <span className="h-2.5 w-2.5 rounded-sm bg-accent/50" />
                    {t('marketdata.market.now.pace.regime')}
                </span>
                <span className="flex items-center gap-1.5">
                    <span className="h-2.5 w-2.5 rounded-sm bg-warn/70" />
                    {t('marketdata.market.now.pace.heat')}
                </span>
                <span className="flex items-center gap-1.5">
                    <span className="h-2.5 w-2.5 rounded-sm border border-warn/40 bg-warn/15" />
                    {t('marketdata.market.now.pace.active')}
                </span>
            </div>
            {rates.curves.length === 0 ? null : (
                <div className="grid gap-1.5 border-t border-line-subtle pt-3">
                    <div className="flex items-baseline justify-between">
                        <span className="font-mono text-xs font-bold tracking-wider uppercase">
                            {t('marketdata.market.now.rates.title')}
                        </span>
                        <span className="font-mono text-[10px] text-ink-faint">
                            {`${clean(rates.peak, 3)}/s`}
                        </span>
                    </div>
                    <svg
                        role="img"
                        aria-label={t('marketdata.market.now.rates.title')}
                        viewBox="0 0 100 1"
                        preserveAspectRatio="none"
                        className="h-20 w-full"
                    >
                        {rates.curves.map((curve) => (
                            <path
                                key={curve.assetClass}
                                d={curvePath(curve.points, rates.peak)}
                                className={assetClassColour(curve.assetClass).text}
                                fill="none"
                                stroke="currentColor"
                                strokeWidth="1.6"
                                vectorEffect="non-scaling-stroke"
                            />
                        ))}
                    </svg>
                    <p className="text-[11px] text-ink-faint">
                        {t('marketdata.market.now.rates.lead')}
                    </p>
                </div>
            )}
        </MarketCard>
    );
}

/** The force switch, who holds the state and for how long, and what the chain's numbers mean. */
function ForceCard({
    regime,
    trace,
    now,
    mayPin,
    pending,
    error,
    onPin,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace | undefined;
    readonly now: number;
    readonly mayPin: boolean;
    readonly pending: RegimeMode | undefined;
    readonly error: string | null;
    readonly onPin: (mode: RegimeMode) => void;
}): ReactNode {
    const { t } = useTranslation();
    const forced = regime.pinned ? regime.state : 'auto';
    const held = trace === undefined ? undefined : heldFor(trace.events, now);
    const share =
        regime.wakeRate > 0 && regime.calmRate > 0
            ? regime.wakeRate / (regime.wakeRate + regime.calmRate)
            : undefined;
    return (
        <MarketCard
            glyph="force"
            tone="warn"
            title={t('marketdata.market.now.force.title')}
            lead={t('marketdata.market.now.force.lead')}
        >
            {mayPin ? (
                <div
                    role="group"
                    aria-label={t('marketdata.market.now.force.title')}
                    className="grid grid-cols-3 gap-0.5 rounded-md border border-line bg-bg-primary p-[3px]"
                >
                    {REGIME_MODES.map((mode) => {
                        const on = forced === mode;
                        return (
                            <button
                                key={mode}
                                type="button"
                                aria-pressed={on}
                                disabled={pending !== undefined}
                                onClick={() => onPin(mode)}
                                className={cx(
                                    'rounded py-2 font-mono text-sm font-semibold disabled:opacity-60',
                                    !on && 'text-ink-muted hover:text-ink',
                                    on && mode === 'auto' && 'bg-accent-dim text-accent',
                                    on && mode === 'calm' && 'bg-up/12 text-up',
                                    on && mode === 'active' && 'bg-warn/12 text-warn',
                                )}
                            >
                                {pending === mode ? '…' : t(`marketdata.marketRegime.set.${mode}`)}
                            </button>
                        );
                    })}
                </div>
            ) : (
                <p className="text-[13px] text-ink-faint">
                    {t('marketdata.marketRegime.readOnly')}
                </p>
            )}
            {error === null ? null : <Notice tone="error">{error}</Notice>}
            <FactRows
                rows={[
                    [
                        t('marketdata.market.now.force.by'),
                        regime.pinned
                            ? t('marketdata.marketRegime.pinned')
                            : t('marketdata.marketRegime.chain'),
                    ],
                    [
                        t('marketdata.market.now.force.held'),
                        held === undefined
                            ? t('marketdata.market.now.force.heldLong', { seconds: RASTER_SECONDS })
                            : duration(held),
                    ],
                    [
                        t('marketdata.market.now.force.spells'),
                        regime.wakeRate > 0 && regime.calmRate > 0
                            ? t('marketdata.market.now.force.spellsValue', {
                                  calm: duration(1 / regime.wakeRate),
                                  active: duration(1 / regime.calmRate),
                              })
                            : '—',
                    ],
                    [
                        t('marketdata.market.now.force.share'),
                        share === undefined ? '—' : `${clean(share * 100, 3)}%`,
                    ],
                ]}
            />
        </MarketCard>
    );
}

/** What changed the market over the window: turns, pins, settings and feeds, newest first. */
function ChangesCard({
    trace,
    onOpenTrace,
}: {
    readonly trace: MarketTrace | undefined;
    readonly onOpenTrace: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const changes = trace === undefined ? [] : marketChanges(trace.events);
    return (
        <MarketCard
            glyph="log"
            title={t('marketdata.market.now.changes.title')}
            lead={t('marketdata.market.now.changes.lead')}
            action={
                <button
                    type="button"
                    onClick={onOpenTrace}
                    className="rounded-md border border-line bg-surface-overlay px-3 py-1.5 font-mono text-xs font-semibold hover:border-line-strong"
                >
                    {t('marketdata.market.now.changes.open')}
                </button>
            }
        >
            {changes.length === 0 ? (
                <p className="text-[13px] text-ink-faint">
                    {t('marketdata.market.now.changes.none')}
                </p>
            ) : (
                <table className="w-full font-mono text-xs">
                    <tbody>
                        {changes.map((event, index) => (
                            <tr key={index} className="border-b border-line-subtle">
                                <td className="py-1.5 pr-3 text-ink-faint tabular-nums">
                                    {new Date(event.at).toLocaleTimeString()}
                                </td>
                                <td className="py-1.5 pr-3">
                                    <KindChip kind={event.kind} />
                                </td>
                                <td className="py-1.5 pr-3 text-ink-muted">
                                    {event.sourceName === ''
                                        ? stateName(event.state, t)
                                        : event.sourceName}
                                </td>
                                <td className="py-1.5 text-right text-ink-faint tabular-nums">
                                    {`τ ${event.tau.toFixed(2)}`}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </MarketCard>
    );
}

const KIND_TONES: Readonly<Record<TraceEvent['kind'], string>> = {
    tick: 'text-ink-faint border-line',
    transition: 'text-warn border-warn/40',
    pin: 'text-stale border-stale/40',
    unpin: 'text-stale border-stale/40',
    configure: 'text-accent border-accent-line',
    add: 'text-up border-up/40',
    remove: 'text-down border-down/40',
};

/** A trace decision's kind as a small chip, coloured by what it did to the market. */
export function KindChip({ kind }: { readonly kind: TraceEvent['kind'] }): ReactNode {
    const { t } = useTranslation();
    return (
        <span
            className={cx(
                'rounded-chip border px-1.5 font-mono text-[10px] font-semibold tracking-wide uppercase',
                KIND_TONES[kind],
            )}
        >
            {t(`marketdata.market.kinds.${kind}`)}
        </span>
    );
}

/** The Now tab: what the market is doing, the switch that forces it, and what changed it. */
export function NowView({
    regime,
    trace,
    raster,
    now,
    mayPin,
    pending,
    error,
    onPin,
    onOpenTrace,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace | undefined;
    readonly raster: FeedRaster | undefined;
    readonly now: number;
    readonly mayPin: boolean;
    readonly pending: RegimeMode | undefined;
    readonly error: string | null;
    readonly onPin: (mode: RegimeMode) => void;
    readonly onOpenTrace: () => void;
}): ReactNode {
    return (
        <div className="grid gap-4">
            <div className="grid gap-4 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                <PaceCard regime={regime} trace={trace} raster={raster} now={now} />
                <ForceCard
                    regime={regime}
                    trace={trace}
                    now={now}
                    mayPin={mayPin}
                    pending={pending}
                    error={error}
                    onPin={onPin}
                />
            </div>
            <ChangesCard trace={trace} onOpenTrace={onOpenTrace} />
        </div>
    );
}
