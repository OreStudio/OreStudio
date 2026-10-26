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
 * Keep the market data sources running, from
 * doc/knowledge/journeys/operations/journey_keep-the-market-data-feeds-running.org.
 *
 * The board draws the tiles, the alert, the sources table and the detail panel
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
    formatTickAge,
    formatVendorId,
    sourceFigures,
    sourceRows,
    stateTone,
    useAdapterStatuses,
    useTickTrends,
    type SourceRow,
} from './marketDataSources.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = ['D46E4DEF-7431-419B-BA88-476AA7B98D24'];

/** The state of one source, in the registry's own words, painted by what it means. */
function SourceStateTag({ state }: { readonly state: string }): ReactNode {
    const { t } = useTranslation();
    const labels: Readonly<Record<string, string>> = {
        announced: t('operations.sources.state.announced'),
        ready: t('operations.sources.state.ready'),
        serving: t('operations.sources.state.serving'),
        stale: t('operations.sources.state.stale'),
        rejected: t('operations.sources.state.rejected'),
    };
    return <Tag tone={stateTone(state)}>{labels[state] ?? state}</Tag>;
}

/** The rejected ids of one source, with the reason the adapter gave for each. */
export function RejectedIds({ row }: { readonly row: SourceRow }): ReactNode {
    const { t } = useTranslation();
    if (row.rejectedIds.length === 0) {
        return <p className="text-sm text-ink-muted">{t('operations.sources.rejected.none')}</p>;
    }
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-1 font-normal">{t('operations.sources.rejected.id')}</th>
                    <th className="py-1 font-normal">{t('operations.sources.rejected.reason')}</th>
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

export function MarketDataSourcesPage(): ReactNode {
    const { t } = useTranslation();
    const [selected, setSelected] = useState('');
    const adapters = useAdapterStatuses();
    const trends = useTickTrends(adapters.data);
    const trail = <AreaTrail area="operations" screen={t('operations.screens.sources')} />;

    if (adapters.isPending || adapters.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.sources.title')}
                description={t('operations.sources.description')}
            >
                {adapters.isError ? (
                    <Notice tone="error">{adapters.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const rows = sourceRows(adapters.data);
    const figures = sourceFigures(rows);
    const chosen = rows.find((row) => row.name === selected) ?? rows[0];
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.sources.gap.kind.title'),
            body: t('operations.sources.gap.kind.body'),
            journey: t('operations.journeys.sources'),
        },
        {
            title: t('operations.sources.gap.drops.title'),
            body: t('operations.sources.gap.drops.body'),
            journey: t('operations.journeys.sources'),
        },
        {
            title: t('operations.sources.gap.parties.title'),
            body: t('operations.sources.gap.parties.body'),
            journey: t('operations.journeys.sources'),
        },
        {
            title: t('operations.sources.gap.history.title'),
            body: t('operations.sources.gap.history.body'),
            journey: t('operations.journeys.sources'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('operations.sources.title')}
                    description={t('operations.sources.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">
                                {t('operations.sources.updated', {
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
                        {t('operations.sources.tag.noDemand', { count: figures.withoutDemand })}
                    </Tag>
                )}
                {figures.stale > 0 && (
                    <Tag tone="down">
                        {t('operations.sources.tag.stale', { count: figures.stale })}
                    </Tag>
                )}
                {figures.quiet > 0 && (
                    <Tag tone="down">
                        {t('operations.sources.tag.quiet', { count: figures.quiet })}
                    </Tag>
                )}
                <span className="text-xs text-ink-faint">{t('operations.sources.tag.lead')}</span>
            </div>

            {figures.notServed > 0 && (
                <Notice tone="warn">
                    {t('operations.sources.alert', { count: figures.notServed })}
                </Notice>
            )}

            <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-5">
                <Metric
                    value={figures.sources.toLocaleString()}
                    label={t('operations.sources.tiles.sources')}
                />
                <Metric
                    value={figures.serving.toLocaleString()}
                    label={t('operations.sources.tiles.serving')}
                    tone={figures.serving > 0 ? 'good' : 'neutral'}
                />
                <Metric
                    value={figures.routes.toLocaleString()}
                    label={t('operations.sources.tiles.routes')}
                />
                <Metric
                    value={figures.ticksPerMinute.toLocaleString()}
                    label={t('operations.sources.tiles.ticks')}
                />
                <Metric
                    value={figures.quiet.toLocaleString()}
                    label={t('operations.sources.tiles.quiet')}
                    tone={figures.quiet > 0 ? 'warn' : 'neutral'}
                />
            </div>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">{t('operations.sources.table.title')}</h2>
                    <span className="text-xs text-ink-faint">
                        {t('operations.sources.table.lead')}
                    </span>
                </header>
                {rows.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('operations.sources.table.empty')}</p>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead className="text-left text-xs text-ink-faint">
                                <tr>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.source')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.provider')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.kind')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.state')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.heartbeat')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.routes')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.demanded')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.accepted')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.rejected')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.ticks')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('operations.sources.columns.trend')}
                                    </th>
                                    <th className="py-1 text-right font-normal">
                                        {t('operations.sources.columns.lastTick')}
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
                                            <SourceStateTag state={row.state} />
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
                <p className="text-xs text-ink-faint">{t('operations.sources.table.note')}</p>
            </section>

            {chosen !== undefined && (
                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">
                            {t('operations.sources.detail.title', { source: chosen.name })}
                        </h2>
                        <SourceStateTag state={chosen.state} />
                    </header>
                    <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-4">
                        <Detail
                            label={t('operations.sources.columns.source')}
                            value={chosen.name}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.provider')}
                            value={chosen.provider}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.heartbeatCounter')}
                            value={chosen.heartbeatCounter.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.routes')}
                            value={chosen.routes.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.demanded')}
                            value={chosen.demanded.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.accepted')}
                            value={chosen.accepted.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.rejected')}
                            value={chosen.rejected.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.notServed')}
                            value={chosen.notServed.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.served')}
                            value={chosen.served.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.detail.unknown')}
                            value={chosen.unknown.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.ticks')}
                            value={chosen.ticksPerMinute.toLocaleString()}
                            mono
                        />
                        <Detail
                            label={t('operations.sources.columns.lastTick')}
                            value={formatTickAge(chosen.lastTickAgeMs, t)}
                            mono
                        />
                    </dl>
                    <div className="space-y-2">
                        <h3 className="text-sm font-medium">
                            {t('operations.sources.rejected.title')}
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
