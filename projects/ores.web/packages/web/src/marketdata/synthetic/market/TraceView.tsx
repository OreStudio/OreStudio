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
import type { MarketTrace, TraceEvent } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { stateName } from '../useMarketRegime.js';
import { MarketCard, MicroLabel, Segmented } from './MarketCard.js';
import { FeedLink, type FeedPages } from './FeedLink.js';
import { clean, duration } from './numbers.js';
import { KindChip } from './NowView.js';

export const KINDS = ['tick', 'transition', 'pin', 'unpin', 'configure', 'add', 'remove'] as const;

/** How many decisions the table draws; the rest are counted. */
const SHOWN = 300;

/** The decisions that match the kinds and the feed asked for, newest first. */
export function traceRows(
    events: readonly TraceEvent[],
    kinds: ReadonlySet<TraceEvent['kind']>,
    feed: string,
): readonly TraceEvent[] {
    const wanted = feed.trim().toLowerCase();
    return [...events]
        .filter((event) => kinds.has(event.kind))
        .filter((event) => wanted === '' || event.sourceName.toLowerCase().includes(wanted))
        .sort((left, right) => right.tau - left.tau);
}

/**
 * The Trace tab: every decision the loop took over the window, filtered, newest first. A trace
 * already read for one feed, as its own Arrivals tab reads it, has no feed to filter by.
 */
export function TraceView({
    trace,
    pages,
    oneFeed = false,
}: {
    readonly trace: MarketTrace | undefined;
    readonly pages: FeedPages;
    readonly oneFeed?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const [kinds, setKinds] = useState<ReadonlySet<TraceEvent['kind']>>(new Set(KINDS));
    const [feed, setFeed] = useState('');
    const rows = trace === undefined ? [] : traceRows(trace.events, kinds, feed);
    return (
        <MarketCard
            glyph="log"
            title={t('marketdata.market.trace.title')}
            lead={t('marketdata.market.trace.lead')}
        >
            <div className="flex flex-wrap items-center gap-2.5">
                <MicroLabel>{t('marketdata.market.trace.kinds')}</MicroLabel>
                <Segmented
                    label={t('marketdata.market.trace.kinds')}
                    options={KINDS}
                    value={kinds}
                    onChange={(kind) =>
                        setKinds((was) => {
                            const next = new Set(was);
                            if (next.has(kind)) {
                                next.delete(kind);
                            } else {
                                next.add(kind);
                            }
                            return next;
                        })
                    }
                    titleOf={(kind) => t(`marketdata.market.kinds.${kind}`)}
                />
                {oneFeed ? null : (
                    <input
                        aria-label={t('marketdata.market.trace.feed')}
                        placeholder={t('marketdata.market.trace.feed')}
                        value={feed}
                        onChange={(event) => setFeed(event.target.value)}
                        className="ml-auto w-60 rounded-md border border-line bg-surface-base px-2.5 py-1 font-mono text-xs"
                    />
                )}
            </div>
            <div className="max-h-[32rem] overflow-auto rounded-lg border border-line-subtle">
                <table className="w-full font-mono text-xs">
                    <thead className="sticky top-0 bg-surface-raised">
                        <tr className="border-b border-line text-left">
                            {(
                                [
                                    'when',
                                    'tau',
                                    'kind',
                                    'feed',
                                    'state',
                                    'pace',
                                    'gap',
                                    'wall',
                                ] as const
                            ).map((column) => (
                                <th
                                    key={column}
                                    className={
                                        ['tau', 'pace', 'gap', 'wall'].includes(column)
                                            ? 'px-2 py-1.5 text-right'
                                            : 'px-2 py-1.5'
                                    }
                                >
                                    <MicroLabel>
                                        {t(`marketdata.market.trace.columns.${column}`)}
                                    </MicroLabel>
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows.slice(0, SHOWN).map((event, index) => (
                            <tr key={index} className="border-b border-line-subtle">
                                <td className="px-2 py-1 text-ink-faint tabular-nums">
                                    {new Date(event.at).toLocaleTimeString()}
                                </td>
                                <td className="px-2 py-1 text-right tabular-nums">
                                    {event.tau.toFixed(3)}
                                </td>
                                <td className="px-2 py-1">
                                    <KindChip kind={event.kind} />
                                </td>
                                <td className="px-2 py-1">
                                    {event.sourceName === '' ? (
                                        '—'
                                    ) : (
                                        <FeedLink sourceName={event.sourceName} pages={pages} />
                                    )}
                                </td>
                                <td className="px-2 py-1">{stateName(event.state, t)}</td>
                                <td className="px-2 py-1 text-right tabular-nums">{`×${event.pace.toFixed(2)}`}</td>
                                <td className="px-2 py-1 text-right tabular-nums">
                                    {event.kind === 'tick' ? clean(event.operationalGap, 3) : ''}
                                </td>
                                <td className="px-2 py-1 text-right tabular-nums">
                                    {event.kind === 'tick' ? duration(event.wallGap) : ''}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <p className="font-mono text-[11px] text-ink-faint">
                {t('marketdata.market.trace.count', {
                    count: rows.length,
                    shown: Math.min(SHOWN, rows.length),
                })}
            </p>
        </MarketCard>
    );
}
