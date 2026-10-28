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
import { Link } from 'react-router';
import type { FeedRaster, MarketTrace, TraceFeed } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx } from '../../../ui/Primitives.js';
import { assetClassColour } from '../../assetClassColour.js';
import { feedPageOf } from '../feedTree.js';
import { markGroups } from '../marketRaster.js';
import { providerOf } from './ArrivalsView.js';
import { FactRows, MarketCard, MicroLabel, Segmented } from './MarketCard.js';
import { clean } from './numbers.js';

export const LAWS = ['periodic', 'poisson', 'hawkes'] as const;
export type Law = (typeof LAWS)[number];

/** How many seconds of each feed's ticks a row shows beside its law. */
const RECENT_SECONDS = 60;

/** One feed's law with the facts the feed list knows about it: its name, its class, its page. */
export interface LawRow {
    readonly law: TraceFeed;
    readonly feed: FeedRaster['feeds'][number] | undefined;
}

export function lawRows(
    trace: MarketTrace | undefined,
    raster: FeedRaster | undefined,
): readonly LawRow[] {
    const feeds = new Map((raster?.feeds ?? []).map((feed) => [feed.sourceName, feed]));
    return (trace?.feeds ?? []).map((law) => ({ law, feed: feeds.get(law.sourceName) }));
}

/** Whether a law's clustering settles: always for the memoryless laws, below one for Hawkes. */
export function lawStable(law: TraceFeed): boolean {
    return law.mode !== 'hawkes' || law.branchingRatio < 1;
}

/**
 * The Laws tab: which law each running feed draws its ticks from, read across feeds. A law
 * belongs to its feed and is set on the feed's own Arrivals tab, so each row links there and
 * nothing here edits one.
 */
export function LawsView({
    trace,
    raster,
    now,
}: {
    readonly trace: MarketTrace | undefined;
    readonly raster: FeedRaster | undefined;
    readonly now: number;
}): ReactNode {
    const { t } = useTranslation();
    const [shown, setShown] = useState<ReadonlySet<Law>>(new Set(LAWS));
    const rows = lawRows(trace, raster);
    return (
        <div className="grid gap-4">
            <div className="grid gap-4 md:grid-cols-3">
                {LAWS.map((law) => (
                    <MarketCard
                        key={law}
                        title={t(`marketdata.marketRegime.modes.${law}`)}
                        lead={t(`marketdata.market.laws.families.${law}.reads`)}
                        action={
                            <span className="flex items-baseline gap-1.5">
                                <span className="font-mono text-lg font-bold">
                                    {rows.filter((row) => row.law.mode === law).length}
                                </span>
                                <MicroLabel>{t('marketdata.market.laws.feeds')}</MicroLabel>
                            </span>
                        }
                    >
                        <FactRows
                            rows={[
                                [
                                    t('marketdata.market.laws.intensity'),
                                    t(`marketdata.market.laws.families.${law}.formula`),
                                ],
                                [
                                    t('marketdata.market.laws.stableWhen'),
                                    t(`marketdata.market.laws.families.${law}.stable`),
                                ],
                            ]}
                        />
                    </MarketCard>
                ))}
            </div>

            <MarketCard
                glyph="laws"
                title={t('marketdata.market.laws.title')}
                lead={t('marketdata.market.laws.lead')}
            >
                <div className="flex flex-wrap items-center gap-2.5">
                    <MicroLabel>{t('marketdata.market.laws.law')}</MicroLabel>
                    <Segmented
                        label={t('marketdata.market.laws.law')}
                        options={LAWS}
                        value={shown}
                        onChange={(law) =>
                            setShown((was) => {
                                const next = new Set(was);
                                if (next.has(law) && next.size > 1) {
                                    next.delete(law);
                                } else {
                                    next.add(law);
                                }
                                return next;
                            })
                        }
                        titleOf={(law) => t(`marketdata.marketRegime.modes.${law}`)}
                    />
                </div>
                {trace === undefined ? (
                    <p className="text-[13px] text-ink-muted">{t('common.loading')}</p>
                ) : rows.length === 0 ? (
                    <p className="text-[13px] text-ink-faint">{t('marketdata.market.laws.none')}</p>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="w-full font-mono text-xs">
                            <thead>
                                <tr className="border-b border-line text-left">
                                    {(
                                        [
                                            'feed',
                                            'law',
                                            'perHour',
                                            'sensitivity',
                                            'branching',
                                            'decay',
                                            'stable',
                                            'recent',
                                        ] as const
                                    ).map((column) => (
                                        <th
                                            key={column}
                                            className={cx(
                                                'px-2 py-1.5 font-semibold',
                                                [
                                                    'perHour',
                                                    'sensitivity',
                                                    'branching',
                                                    'decay',
                                                ].includes(column) && 'text-right',
                                            )}
                                        >
                                            <MicroLabel>
                                                {t(`marketdata.market.laws.columns.${column}`)}
                                            </MicroLabel>
                                        </th>
                                    ))}
                                    <th />
                                </tr>
                            </thead>
                            <tbody>
                                {rows
                                    .filter((row) => shown.has(row.law.mode as Law))
                                    .map(({ law, feed }) => {
                                        const hawkes = law.mode === 'hawkes';
                                        const recent =
                                            feed === undefined
                                                ? []
                                                : markGroups(feed.ticks, RECENT_SECONDS, now);
                                        return (
                                            <tr
                                                key={law.sourceName}
                                                className="border-b border-line-subtle"
                                            >
                                                <td className="px-2 py-1.5">
                                                    <span className="flex items-center gap-2">
                                                        <span
                                                            className={cx(
                                                                'size-[7px] shrink-0 rounded-sm',
                                                                assetClassColour(
                                                                    feed?.assetClass ?? '',
                                                                ).dot,
                                                            )}
                                                        />
                                                        {feed?.label ?? law.sourceName}
                                                        <span className="text-[10px] text-ink-faint">
                                                            {providerOf(law.sourceName)}
                                                        </span>
                                                    </span>
                                                </td>
                                                <td className="px-2 py-1.5">
                                                    {t(`marketdata.marketRegime.modes.${law.mode}`)}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {clean(law.rate * 3600, 4)}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {clean(law.sensitivity, 3)}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {hawkes ? clean(law.branchingRatio, 3) : '—'}
                                                </td>
                                                <td className="px-2 py-1.5 text-right tabular-nums">
                                                    {hawkes ? clean(law.decay, 3) : '—'}
                                                </td>
                                                <td className="px-2 py-1.5">
                                                    {hawkes ? (
                                                        <span
                                                            className={cx(
                                                                'rounded-chip border px-1.5 text-[10px] font-semibold uppercase',
                                                                lawStable(law)
                                                                    ? 'border-up/40 text-up'
                                                                    : 'border-down/40 text-down',
                                                            )}
                                                        >
                                                            {t(
                                                                lawStable(law)
                                                                    ? 'marketdata.market.laws.stable'
                                                                    : 'marketdata.market.laws.runsAway',
                                                            )}
                                                        </span>
                                                    ) : (
                                                        <span className="text-ink-faint">
                                                            {t('marketdata.market.laws.always')}
                                                        </span>
                                                    )}
                                                </td>
                                                <td className="px-2 py-1.5">
                                                    <svg
                                                        aria-hidden
                                                        viewBox="0 0 100 16"
                                                        preserveAspectRatio="none"
                                                        className="h-4 w-32 rounded-sm bg-bg-primary"
                                                    >
                                                        {recent.map((group, index) => (
                                                            <rect
                                                                key={index}
                                                                x={group.position * 100}
                                                                width={1}
                                                                y={2}
                                                                height={12}
                                                                className={
                                                                    assetClassColour(
                                                                        feed?.assetClass ?? '',
                                                                    ).text
                                                                }
                                                                fill="currentColor"
                                                            />
                                                        ))}
                                                    </svg>
                                                </td>
                                                <td className="px-2 py-1.5 text-right whitespace-nowrap">
                                                    {feed === undefined || feed.id === '' ? null : (
                                                        <Link
                                                            to={`${feedPageOf(feed)}?tab=arrivals`}
                                                            className="text-accent hover:underline"
                                                        >
                                                            {t('marketdata.market.laws.open')}
                                                        </Link>
                                                    )}
                                                </td>
                                            </tr>
                                        );
                                    })}
                            </tbody>
                        </table>
                    </div>
                )}
            </MarketCard>
        </div>
    );
}
