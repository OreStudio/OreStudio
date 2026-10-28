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
import type { MarketRegime, MarketTrace } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx } from '../../../ui/Primitives.js';
import { stateName } from '../useMarketRegime.js';
import { MicroLabel } from './MarketCard.js';
import { clean, times } from './numbers.js';

/** The state as a badge: green when calm, amber when active, with a glow that says it is live. */
export function StateBadge({ state }: { readonly state: MarketRegime['state'] }): ReactNode {
    const { t } = useTranslation();
    return (
        <span
            className={cx(
                'inline-flex items-center rounded-chip border px-2 py-0.5 font-mono text-xs font-bold tracking-wide uppercase',
                state === 'active'
                    ? 'border-warn/45 bg-warn/10 text-warn shadow-[0_0_12px_rgba(210,153,34,0.22)]'
                    : 'border-up/40 bg-up/10 text-up shadow-[0_0_12px_rgba(63,185,80,0.18)]',
            )}
        >
            {stateName(state, t)}
        </span>
    );
}

function Metric({
    label,
    value,
    sub,
    tone,
}: {
    readonly label: string;
    readonly value: ReactNode;
    readonly sub?: string | undefined;
    readonly tone?: 'accent' | 'warn' | 'up';
}): ReactNode {
    return (
        <div className="grid min-w-0 gap-1.5 rounded-card border border-line bg-surface-raised px-3 py-2.5">
            <MicroLabel>{label}</MicroLabel>
            <div className="flex items-baseline justify-between gap-1.5">
                <span
                    className={cx(
                        'font-mono text-xl leading-tight font-bold whitespace-nowrap tabular-nums',
                        tone === 'accent' && 'text-accent',
                        tone === 'warn' && 'text-warn',
                        tone === 'up' && 'text-up',
                    )}
                >
                    {value}
                </span>
                {sub === undefined ? null : (
                    <span className="truncate font-mono text-[11px] text-ink-faint">{sub}</span>
                )}
            </div>
        </div>
    );
}

/**
 * The market in seven figures, above every tab of the market, so a person on any tab sees
 * the market without leaving the question they came with. The loop's own figures wait for
 * the trace; until it is read they show a dash.
 */
export function MarketStrip({
    regime,
    trace,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section
            aria-label={t('marketdata.market.strip.label')}
            className="grid grid-cols-2 gap-2.5 sm:grid-cols-4 xl:grid-cols-7"
        >
            <Metric
                label={t('marketdata.market.strip.state')}
                value={
                    <span className="flex items-center gap-2">
                        <StateBadge state={regime.state} />
                        {regime.pinned ? (
                            <span className="rounded-chip border border-stale/45 px-1.5 font-mono text-[10px] font-semibold text-stale uppercase">
                                {t('marketdata.market.strip.forced')}
                            </span>
                        ) : null}
                    </span>
                }
            />
            <Metric
                label={t('marketdata.market.strip.multiplier')}
                value={times(regime.multiplier)}
                sub={t('marketdata.market.strip.multiplierSub')}
            />
            <Metric
                label={t('marketdata.market.strip.pace')}
                value={`×${regime.pace.toFixed(2)}`}
                sub={t('marketdata.market.strip.paceSub')}
                tone="accent"
            />
            <Metric
                label={t('marketdata.market.strip.loopGain')}
                value={clean(regime.loopGain, 3)}
                sub={t('marketdata.market.strip.loopGainSub')}
                tone="warn"
            />
            <Metric
                label={t('marketdata.market.strip.ticksPerSecond')}
                value={trace === undefined ? '—' : trace.ticksPerSecond.toFixed(1)}
                sub={t('marketdata.market.strip.ticksPerSecondSub')}
                tone="up"
            />
            <Metric
                label={t('marketdata.market.strip.speed')}
                value={clean(regime.speed)}
                sub={t('marketdata.market.strip.speedSub')}
            />
            <Metric
                label={t('marketdata.market.strip.lateness')}
                value={
                    trace === undefined ? '—' : `${(trace.latenessSeconds * 1000).toFixed(1)} ms`
                }
                sub={
                    trace === undefined
                        ? undefined
                        : t('marketdata.market.strip.latenessSub', {
                              most: (trace.maxLatenessSeconds * 1000).toFixed(1),
                          })
                }
            />
        </section>
    );
}
