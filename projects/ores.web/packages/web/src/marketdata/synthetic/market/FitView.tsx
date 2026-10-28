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
import type { MarketRegime, MarketTrace } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx } from '../../../ui/Primitives.js';
import { exponentialQuantiles, feedFit, QQ_MINIMUM_GAPS, tauSpan } from '../marketTrace.js';
import { MarketCard, MicroLabel } from './MarketCard.js';
import { clean } from './numbers.js';

/** The pixels a QQ plot takes on each side. */
const QQ_SIZE = 260;

/** Where seen over asked sits on a bar a log scale wide: one in the middle, a half and twice at the ends. */
export function ratioPosition(ratio: number): number {
    return Math.max(0, Math.min(100, 50 + Math.log2(Math.max(ratio, 1e-3)) * 25));
}

/**
 * The Fit tab: each running feed's ticks over the window against its law, the gaps of the
 * feed a person picks against Exp(1), and how the loop that schedules them keeps up.
 */
export function FitView({
    regime,
    trace,
}: {
    readonly regime: MarketRegime;
    readonly trace: MarketTrace | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [picked, setPicked] = useState<string | undefined>(undefined);
    const span = trace === undefined ? undefined : tauSpan(trace.events);
    const fits =
        trace === undefined || span === undefined
            ? []
            : trace.feeds.map((feed) => ({ feed, fit: feedFit(trace.events, feed, span) }));
    // Until a person picks one, the feed with the most ticks, whose plot says the most.
    const busiest = fits.reduce<(typeof fits)[number] | undefined>(
        (best, entry) =>
            best === undefined || entry.fit.observed > best.fit.observed ? entry : best,
        undefined,
    );
    const chosen = fits.find((entry) => entry.feed.sourceName === picked) ?? busiest;
    const points = chosen === undefined ? [] : exponentialQuantiles(chosen.fit.gaps);
    const top =
        points.reduce((high, point) => Math.max(high, point.expected, point.observed), 1) * 1.05;
    return (
        <div className="grid gap-4">
            <div className="grid gap-4 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                <MarketCard
                    glyph="fit"
                    title={t('marketdata.market.fit.title')}
                    lead={t('marketdata.marketRegime.fit.lead')}
                >
                    {fits.length === 0 ? (
                        <p className="text-[13px] text-ink-faint">
                            {t('marketdata.market.fit.none')}
                        </p>
                    ) : (
                        <div className="max-h-[32rem] overflow-y-auto">
                            <table className="w-full font-mono text-xs">
                                <thead>
                                    <tr className="border-b border-line text-left">
                                        <th className="px-2 py-1.5">
                                            <MicroLabel>
                                                {t('marketdata.marketRegime.fit.feed')}
                                            </MicroLabel>
                                        </th>
                                        <th className="px-2 py-1.5">
                                            <MicroLabel>
                                                {t('marketdata.market.laws.columns.law')}
                                            </MicroLabel>
                                        </th>
                                        <th className="px-2 py-1.5 text-right">
                                            <MicroLabel>
                                                {t('marketdata.marketRegime.fit.observed')}
                                            </MicroLabel>
                                        </th>
                                        <th className="px-2 py-1.5 text-right">
                                            <MicroLabel>
                                                {t('marketdata.marketRegime.fit.expected')}
                                            </MicroLabel>
                                        </th>
                                        <th className="px-2 py-1.5 text-right">
                                            <MicroLabel>
                                                {t('marketdata.market.fit.ratio')}
                                            </MicroLabel>
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {fits.map(({ feed, fit }) => {
                                        const ratio =
                                            fit.expected > 0 ? fit.observed / fit.expected : 0;
                                        return (
                                            <tr
                                                key={feed.sourceName}
                                                className={cx(
                                                    'cursor-pointer border-b border-line-subtle hover:bg-surface-overlay',
                                                    chosen?.feed.sourceName === feed.sourceName &&
                                                        'bg-accent-dim',
                                                )}
                                                onClick={() => setPicked(feed.sourceName)}
                                            >
                                                <td className="px-2 py-1.5">{feed.sourceName}</td>
                                                <td className="px-2 py-1.5">
                                                    {t(
                                                        `marketdata.marketRegime.modes.${feed.mode}`,
                                                    )}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {fit.observed}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {clean(fit.expected, 3)}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {clean(ratio, 3)}
                                                    <span
                                                        aria-hidden
                                                        className="relative ml-2 inline-block h-1.5 w-16 rounded-full border border-line bg-bg-primary align-middle"
                                                    >
                                                        <span className="absolute -inset-y-1 left-1/2 w-px bg-line-strong" />
                                                        <span
                                                            className="absolute -inset-y-px w-0.5 bg-accent"
                                                            style={{
                                                                left: `${String(ratioPosition(ratio))}%`,
                                                            }}
                                                        />
                                                    </span>
                                                </td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        </div>
                    )}
                </MarketCard>

                <MarketCard
                    title={
                        chosen === undefined
                            ? t('marketdata.market.fit.qqTitle')
                            : t('marketdata.marketRegime.fit.qq', { feed: chosen.feed.sourceName })
                    }
                    lead={t('marketdata.market.fit.qqLead')}
                >
                    {chosen === undefined || points.length < QQ_MINIMUM_GAPS ? (
                        <p className="text-[13px] text-ink-faint">
                            {t('marketdata.marketRegime.fit.tooFew', {
                                feed: chosen?.feed.sourceName ?? '—',
                                minimum: QQ_MINIMUM_GAPS,
                            })}
                        </p>
                    ) : (
                        <svg
                            role="img"
                            aria-label={t('marketdata.marketRegime.fit.qq', {
                                feed: chosen.feed.sourceName,
                            })}
                            viewBox={`0 0 ${String(QQ_SIZE)} ${String(QQ_SIZE)}`}
                            className="w-full max-w-[260px] text-accent"
                        >
                            <line
                                x1={0}
                                y1={QQ_SIZE}
                                x2={QQ_SIZE}
                                y2={0}
                                className="stroke-line-strong"
                                strokeDasharray="4 4"
                            />
                            {points.map((point, index) => (
                                <circle
                                    key={index}
                                    data-qq
                                    cx={(point.expected / top) * QQ_SIZE}
                                    cy={QQ_SIZE - (point.observed / top) * QQ_SIZE}
                                    r={2.5}
                                    fill="currentColor"
                                    opacity={0.8}
                                />
                            ))}
                        </svg>
                    )}
                    {chosen === undefined ? null : (
                        <p className="font-mono text-[11px] text-ink-faint">
                            {t('marketdata.market.fit.gaps', { count: chosen.fit.gaps.length })}
                        </p>
                    )}
                </MarketCard>
            </div>

            <MarketCard
                glyph="health"
                title={t('marketdata.market.fit.health.title')}
                lead={t('marketdata.market.fit.health.lead')}
            >
                <dl className="grid grid-cols-2 gap-2.5 sm:grid-cols-4 xl:grid-cols-7">
                    {(
                        [
                            [
                                'ticksPerSecond',
                                trace === undefined ? '—' : trace.ticksPerSecond.toFixed(1),
                            ],
                            ['heap', trace === undefined ? '—' : String(trace.heapSize)],
                            [
                                'lateness',
                                trace === undefined
                                    ? '—'
                                    : `${(trace.latenessSeconds * 1000).toFixed(1)} ms`,
                            ],
                            [
                                'latest',
                                trace === undefined
                                    ? '—'
                                    : `${(trace.maxLatenessSeconds * 1000).toFixed(1)} ms`,
                            ],
                            ['series', trace === undefined ? '—' : String(trace.series)],
                            ['enrolled', `${clean(regime.populationRate, 3)}/s`],
                            ['seed', String(regime.runSeed)],
                        ] as const
                    ).map(([key, value]) => (
                        <div
                            key={key}
                            className="grid gap-1 rounded-card border border-line-subtle bg-bg-primary px-3 py-2"
                        >
                            <dt>
                                <MicroLabel>{t(`marketdata.market.fit.health.${key}`)}</MicroLabel>
                            </dt>
                            <dd className="font-mono text-base font-bold tabular-nums">{value}</dd>
                        </div>
                    ))}
                </dl>
            </MarketCard>
        </div>
    );
}
