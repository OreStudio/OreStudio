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

/**
 * Keep the market data feeds running, from
 * doc/knowledge/journeys/operations/journey_keep-the-market-data-feeds-running.org.
 *
 * The board draws the tiles, the alert, the feeds table and the detail panel
 * of the signed-off prototype. Every figure is the market data service's
 * adapter status, read once and counted; the fields the status does not carry
 * are stated as gaps, never filled from another screen's read.
 */

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Metric } from '../ui/Metric.js';
import { Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshNow } from '../ui/RefreshButton.js';
import { Sparkline } from '../ui/Sparkline.js';
import { RelativeTime } from '../ui/Time.js';
import { GapPanel, type ScreenGap } from './OperationsParts.js';
import { readTime } from './ServicesPage.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';
import {
    feedFigures,
    feedRows,
    formatTickAge,
    formatVendorId,
    stateTone,
    useAdapterStatuses,
    useTickTrends,
    type FeedRow,
} from './marketDataFeeds.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = ['D46E4DEF-7431-419B-BA88-476AA7B98D24'];

/** The state of one feed, in the registry's own words, painted by what it means. */
function FeedStateTag({ state }: { readonly state: string }): ReactNode {
    const { t } = useTranslation();
    const labels: Readonly<Record<string, string>> = {
        announced: t('operations.feeds.state.announced'),
        ready: t('operations.feeds.state.ready'),
        serving: t('operations.feeds.state.serving'),
        stale: t('operations.feeds.state.stale'),
        rejected: t('operations.feeds.state.rejected'),
    };
    return <Tag tone={stateTone(state)}>{labels[state] ?? state}</Tag>;
}

/** The rejected ids of one feed, with the reason the adapter gave for each. */
export function RejectedIds({ row }: { readonly row: FeedRow }): ReactNode {
    const { t } = useTranslation();
    if (row.rejectedIds.length === 0) {
        return <p className="text-sm text-ink-muted">{t('operations.feeds.rejected.none')}</p>;
    }
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-1 font-normal">{t('operations.feeds.rejected.id')}</th>
                    <th className="py-1 font-normal">{t('operations.feeds.rejected.reason')}</th>
                </tr>
            </thead>
            <tbody className="divide-y divide-line-subtle">
                {row.rejectedIds.map((entry) => (
                    <tr key={`${formatVendorId(entry.id)}-${entry.reason}`}>
                        <td className="py-1.5 font-mono text-xs">{formatVendorId(entry.id)}</td>
                        <td className="py-1.5 font-mono text-xs">{entry.reason}</td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

export function MarketDataFeedsPage(): ReactNode {
    const { t } = useTranslation();
    const [selected, setSelected] = useState('');
    const adapters = useAdapterStatuses();
    const trends = useTickTrends(adapters.data);
    const trail = <AreaTrail area="operations" screen={t('operations.screens.feeds')} />;

    if (adapters.isPending || adapters.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.feeds.title')}
                description={t('operations.feeds.description')}
            >
                {adapters.isError ? (
                    <Notice tone="error">{adapters.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const rows = feedRows(adapters.data);
    const figures = feedFigures(rows);
    const chosen = rows.find((row) => row.name === selected) ?? rows[0];
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.feeds.gap.kind.title'),
            body: t('operations.feeds.gap.kind.body'),
            journey: t('operations.journeys.feeds'),
        },
        {
            title: t('operations.feeds.gap.drops.title'),
            body: t('operations.feeds.gap.drops.body'),
            journey: t('operations.journeys.feeds'),
        },
        {
            title: t('operations.feeds.gap.parties.title'),
            body: t('operations.feeds.gap.parties.body'),
            journey: t('operations.journeys.feeds'),
        },
        {
            title: t('operations.feeds.gap.history.title'),
            body: t('operations.feeds.gap.history.body'),
            journey: t('operations.journeys.feeds'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('operations.feeds.title')}
                    description={t('operations.feeds.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">
                                {t('operations.feeds.updated', {
                                    at: readTime(adapters.dataUpdatedAt),
                                })}
                            </span>
                            <RefreshNow
                                onClick={() => void adapters.refetch()}
                                pending={adapters.isFetching}
                            />
                        </div>
                    }
                />
            </div>

            <div className="flex flex-wrap items-center gap-2">
                {figures.withoutDemand > 0 && (
                    <Tag tone="warn">
                        {t('operations.feeds.tag.noDemand', { count: figures.withoutDemand })}
                    </Tag>
                )}
                {figures.stale > 0 && (
                    <Tag tone="down">
                        {t('operations.feeds.tag.stale', { count: figures.stale })}
                    </Tag>
                )}
                {figures.quiet > 0 && (
                    <Tag tone="down">
                        {t('operations.feeds.tag.quiet', { count: figures.quiet })}
                    </Tag>
                )}
                <span className="text-xs text-ink-faint">{t('operations.feeds.tag.lead')}</span>
            </div>

            {figures.notServed > 0 && (
                <Notice tone="warn">
                    {t('operations.feeds.alert', { count: figures.notServed })}
                </Notice>
            )}

            <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-5">
                <Metric
                    value={figures.feeds.toLocaleString()}
                    label={t('operations.feeds.tiles.feeds')}
                />
                <Metric
                    value={figures.serving.toLocaleString()}
                    label={t('operations.feeds.tiles.serving')}
                    tone={figures.serving > 0 ? 'good' : 'neutral'}
                />
                <Metric
                    value={figures.routes.toLocaleString()}
                    label={t('operations.feeds.tiles.routes')}
                />
                <Metric
                    value={figures.ticksPerMinute.toLocaleString()}
                    label={t('operations.feeds.tiles.ticks')}
                />
                <Metric
                    value={figures.quiet.toLocaleString()}
                    label={t('operations.feeds.tiles.quiet')}
                    tone={figures.quiet > 0 ? 'warn' : 'neutral'}
                />
            </div>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">{t('operations.feeds.table.title')}</h2>
                    <span className="text-xs text-ink-faint">
                        {t('operations.feeds.table.lead')}
                    </span>
                </header>
                {rows.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('operations.feeds.table.empty')}</p>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead className="text-left text-xs text-ink-faint">
                                <tr>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.feed')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.provider')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.kind')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.state')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.heartbeat')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.routes')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.demanded')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.accepted')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.rejected')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.ticks')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.feeds.columns.trend')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.feeds.columns.lastTick')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody className="divide-y divide-line-subtle">
                                {rows.map((row) => (
                                    <tr
                                        key={row.name}
                                        className={
                                            row.name === chosen?.name ? 'bg-surface-hover' : ''
                                        }
                                    >
                                        <td className="py-2">
                                            <button
                                                type="button"
                                                className="font-mono text-accent hover:text-accent-bright"
                                                onClick={() => setSelected(row.name)}
                                            >
                                                {row.name}
                                            </button>
                                        </td>
                                        <td className="py-2 font-mono text-xs">{row.provider}</td>
                                        <td className="py-2 font-mono text-ink-faint">—</td>
                                        <td className="py-2">
                                            <FeedStateTag state={row.state} />
                                        </td>
                                        <td className="py-2 text-xs text-ink-muted">
                                            {row.heartbeatAt === '' ? (
                                                '—'
                                            ) : (
                                                <RelativeTime at={row.heartbeatAt} />
                                            )}
                                        </td>
                                        <td className="py-2 text-right font-mono tabular-nums">
                                            {row.routes}
                                        </td>
                                        <td className="py-2 text-right font-mono tabular-nums">
                                            {row.demanded}
                                        </td>
                                        <td className="py-2 text-right font-mono tabular-nums">
                                            {row.accepted}
                                        </td>
                                        <td
                                            className={`py-2 text-right font-mono tabular-nums ${
                                                row.rejected > 0 ? 'text-down' : ''
                                            }`}
                                        >
                                            {row.rejected}
                                        </td>
                                        <td className="py-2 text-right font-mono tabular-nums">
                                            {row.ticksPerMinute}
                                        </td>
                                        <td className="w-16 py-2">
                                            <Sparkline points={trends.get(row.name) ?? []} />
                                        </td>
                                        <td className="py-2 text-right font-mono text-xs text-ink-muted">
                                            {formatTickAge(row.lastTickAgeMs, t)}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}
                <p className="text-xs text-ink-faint">{t('operations.feeds.table.note')}</p>
            </section>

            {chosen !== undefined && (
                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">
                            {t('operations.feeds.detail.title', { feed: chosen.name })}
                        </h2>
                        <FeedStateTag state={chosen.state} />
                    </header>
                    <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-4">
                        <Detail
                            label={t('operations.feeds.columns.feed')}
                            value={chosen.name}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.provider')}
                            value={chosen.provider}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.detail.heartbeatCounter')}
                            value={chosen.heartbeatCounter.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.routes')}
                            value={chosen.routes.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.demanded')}
                            value={chosen.demanded.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.accepted')}
                            value={chosen.accepted.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.rejected')}
                            value={chosen.rejected.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.detail.notServed')}
                            value={chosen.notServed.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.detail.served')}
                            value={chosen.served.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.detail.unknown')}
                            value={chosen.unknown.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.ticks')}
                            value={chosen.ticksPerMinute.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.feeds.columns.lastTick')}
                            value={formatTickAge(chosen.lastTickAgeMs, t)}
                            mono
                        />
                    </dl>
                    <div className="space-y-2">
                        <h3 className="text-sm font-medium">
                            {t('operations.feeds.rejected.title')}
                        </h3>
                        <RejectedIds row={chosen} />
                    </div>
                </section>
            )}

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
