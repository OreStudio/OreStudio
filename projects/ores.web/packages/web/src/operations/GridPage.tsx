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
 * Watch the compute grid, from
 * doc/knowledge/journeys/operations/journey_watch_the_compute_grid.org.
 *
 * The node table is the whole installation's: the node read carries no tenant
 * predicate, so every session sees the whole fleet. The counters above it are
 * not: the poller computes them for one tenant, so the panel says whose they
 * are rather than presenting them as every tenant's work.
 *
 * One row per machine carries the runner that reports for it, because one
 * runner runs on each node: the state, version and instance are columns of the
 * node rather than a second table restating the node rows in another order. A
 * node whose runner never reported keeps its row and says missing rather than
 * leaving the table.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';
import type { GridNodeRow, GridView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Button, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import {
    GapPanel,
    InstanceStateTag,
    InstanceVersion,
    OperationsBack,
    SERVICE_RUNNING_WINDOW_MINUTES,
    newestVersionOf,
    type ScreenGap,
} from './OperationsParts.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';
import { AreaTrail } from '../shell/AreaTrail.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    'EE26E58C-F389-4E82-BF96-EC5324F9F795',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
];

/** Where the grid read's answer is cached, so a test can seed it. */
export const GRID_QUERY_KEY = ['operations-grid'] as const;

/** The instant a read answered, as the wall clock a person reads, in UTC. */
export function readTime(at: number): string {
    return `${new Date(at).toISOString().slice(11, 19)} UTC`;
}

/**
 * The instant a stored sample was taken, as a wall clock a person reads, in UTC.
 *
 * Nothing when no sample is stored, or the time is unreadable: a summary
 * without its age cannot be trusted, so an unreadable time is an absent one
 * rather than a made-up one.
 */
export function sampleTime(at: string | null): string | undefined {
    if (at === null || !isWireTimestamp(at)) {
        return undefined;
    }
    return `${fromWireTimestamp(at).toISOString().slice(11, 19)} UTC`;
}

/** A byte count in GiB, as the node table states what a node fetched. */
export function asGiB(bytes: number, t: Translator['t']): string {
    return t('operations.grid.units.gib', { value: (bytes / 1024 / 1024 / 1024).toFixed(2) });
}

/** A byte count in MiB, as the node table states what a node uploaded. */
export function asMiB(bytes: number, t: Translator['t']): string {
    return t('operations.grid.units.mib', { value: Math.round(bytes / 1024 / 1024) });
}

/** A task duration in whole seconds, as the prototype states it. */
export function asSeconds(milliseconds: number, t: Translator['t']): string {
    return t('operations.grid.units.seconds', { seconds: Math.round(milliseconds / 1000) });
}

/**
 * How long ago a node heartbeated, in the units a person reads.
 *
 * The unit words come from the catalogue rather than from here, so a French or
 * Portuguese reader does not read English abbreviations inside their own
 * sentence.
 */
export function formatAge(seconds: number, t: Translator['t']): string {
    if (seconds < 60) {
        return t('operations.grid.age.seconds', { seconds });
    }
    const minutes = Math.floor(seconds / 60);
    if (minutes < 60) {
        return t('operations.grid.age.minutesSeconds', {
            minutes,
            seconds: seconds % 60,
        });
    }
    return t('operations.grid.age.hoursMinutes', {
        hours: Math.floor(minutes / 60),
        minutes: minutes % 60,
    });
}

/** The grid's counters, and the sample time they were computed at. */
function SummaryPanel({ view }: { readonly view: GridView }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.grid.summary.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('operations.grid.summary.sampled', {
                        at: sampleTime(view.sampled_at) ?? '',
                    })}
                </span>
            </header>
            <div className="grid gap-x-8 gap-y-3 text-sm sm:grid-cols-3">
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.grid.summary.hosts')}
                    </span>
                    <span className="flex flex-wrap items-center gap-2">
                        <span className="font-mono">{view.total_hosts}</span>
                        <Tag tone="neutral">
                            {t('operations.grid.summary.online', { count: view.online_hosts })}
                        </Tag>
                        <Tag tone="neutral">
                            {t('operations.grid.summary.idle', { count: view.idle_hosts })}
                        </Tag>
                    </span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.grid.summary.work')}
                    </span>
                    <span className="flex flex-wrap items-center gap-2">
                        <span className="font-mono">
                            {t('operations.grid.summary.workunits', {
                                workunits: view.total_workunits,
                                batches: view.total_batches,
                            })}
                        </span>
                        <Tag tone="accent">
                            {t('operations.grid.summary.active', { count: view.active_batches })}
                        </Tag>
                    </span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.grid.summary.outcomes')}
                    </span>
                    <span className="flex flex-wrap items-center gap-2">
                        <Tag tone="neutral">
                            {t('operations.grid.summary.success', {
                                count: view.outcomes_success,
                            })}
                        </Tag>
                        <Tag tone="warn">
                            {t('operations.grid.summary.clientError', {
                                count: view.outcomes_client_error,
                            })}
                        </Tag>
                        <Tag tone="warn">
                            {t('operations.grid.summary.noReply', {
                                count: view.outcomes_no_reply,
                            })}
                        </Tag>
                    </span>
                </div>
            </div>
            <p className="text-xs text-ink-faint">{t('operations.grid.summary.oneTenant')}</p>
        </section>
    );
}

/**
 * The last eight characters of an instance id, as the row states them.
 *
 * The ids are UUIDv7, so they begin with a millisecond timestamp: agents
 * started in the same moment share their leading hex digits, which makes the
 * head say "when this process started" rather than which process it is. The
 * tail is the random part, so it is the part that tells two runners apart. The
 * full id stays in the cell's title.
 */
export function instanceTail(instanceId: string): string {
    return instanceId.slice(-8);
}

/** One node's name: the hostname, or the id beside a plain statement it is one. */
function NodeName({ node }: { readonly node: GridNodeRow }): ReactNode {
    const { t } = useTranslation();
    if (node.host === null) {
        return (
            <span className="flex flex-wrap items-center gap-2">
                <span className="font-mono">{node.host_id}</span>
                <Tag tone="muted">{t('operations.grid.nodes.noHost')}</Tag>
            </span>
        );
    }
    return <span className="font-mono">{node.host}</span>;
}

/** The runner's instance, shown by the tail with the full id in the title. */
function RunnerInstance({ node }: { readonly node: GridNodeRow }): ReactNode {
    if (node.instance_id === null) {
        return <span className="font-mono text-ink-faint">—</span>;
    }
    return (
        <span className="font-mono" title={node.instance_id}>
            {instanceTail(node.instance_id)}
        </span>
    );
}

function NodesPanel({ nodes }: { readonly nodes: readonly GridNodeRow[] }): ReactNode {
    const { t } = useTranslation();
    const running = nodes.filter((node) => node.state === 'running');
    const lost = nodes.filter((node) => node.state === 'lost');
    const missing = nodes.filter((node) => node.state === 'missing');
    const newestVersion = newestVersionOf(running);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.grid.nodes.title')}</h2>
                <div className="flex flex-wrap items-center gap-2 text-xs text-ink-faint">
                    <span>{t('operations.grid.nodes.rows', { count: nodes.length })}</span>
                    <span>
                        {t('operations.grid.nodes.reported', {
                            running: running.length,
                            total: nodes.length,
                            minutes: SERVICE_RUNNING_WINDOW_MINUTES,
                        })}
                    </span>
                    {lost.length > 0 && (
                        <Tag tone="muted">
                            {t('operations.grid.nodes.lost', { count: lost.length })}
                        </Tag>
                    )}
                    {missing.length > 0 && (
                        <Tag tone="warn">
                            {t('operations.grid.nodes.missing', { count: missing.length })}
                        </Tag>
                    )}
                </div>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.node')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.status')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.version')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.instance')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksCompleted')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksFailed')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksSinceLast')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.meanTime')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.slowest')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.fetched')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.uploaded')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.sinceHeartbeat')}
                        </th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {nodes.map((node) => {
                        const quiet = node.seconds_since_hb > SERVICE_RUNNING_WINDOW_MINUTES * 60;
                        return (
                            <tr key={node.host_id}>
                                <td className="py-2">
                                    <NodeName node={node} />
                                </td>
                                <td className="py-2">
                                    <InstanceStateTag state={node.state} />
                                </td>
                                <td className="py-2">
                                    <InstanceVersion
                                        instance={node}
                                        newestVersion={newestVersion}
                                    />
                                </td>
                                <td className="py-2">
                                    <RunnerInstance node={node} />
                                </td>
                                <td className="py-2 font-mono">{node.tasks_completed}</td>
                                <td className="py-2 font-mono">
                                    {node.tasks_failed > 0 ? (
                                        <Tag tone="warn">{node.tasks_failed}</Tag>
                                    ) : (
                                        node.tasks_failed
                                    )}
                                </td>
                                <td className="py-2 font-mono">{node.tasks_since_last}</td>
                                <td className="py-2 font-mono">
                                    {asSeconds(node.avg_task_duration_ms, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asSeconds(node.max_task_duration_ms, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asGiB(node.input_bytes_fetched, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asMiB(node.output_bytes_uploaded, t)}
                                </td>
                                <td className="py-2">
                                    {quiet ? (
                                        <Tag tone="warn">{formatAge(node.seconds_since_hb, t)}</Tag>
                                    ) : (
                                        <span className="font-mono">
                                            {formatAge(node.seconds_since_hb, t)}
                                        </span>
                                    )}
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">{t('operations.grid.nodes.hint')}</p>
            <div className="flex flex-wrap items-center gap-3">
                <Button variant="secondary" disabled title={t('operations.grid.nodes.openHint')}>
                    {t('operations.grid.nodes.open')}
                </Button>
                <span className="text-xs text-ink-faint">
                    {t('operations.grid.nodes.openHint')}
                </span>
            </div>
        </section>
    );
}

export function GridPage(): ReactNode {
    const { t } = useTranslation();
    const grid = useQuery({ queryKey: GRID_QUERY_KEY, queryFn: api.grid });

    if (grid.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (grid.isError) {
        return <Notice tone="error">{grid.error.message}</Notice>;
    }

    const view = grid.data;
    const hasSample = sampleTime(view.sampled_at) !== undefined;
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.grid.gap.history.title'),
            body: t('operations.grid.gap.history.body'),
            journey: t('operations.journeys.grid'),
        },
        {
            title: t('operations.grid.gap.noHost.title'),
            body: t('operations.grid.gap.noHost.body'),
            journey: t('operations.journeys.grid'),
        },
        {
            title: t('operations.grid.gap.oneTenant.title'),
            body: t('operations.grid.gap.oneTenant.body'),
            journey: t('operations.journeys.grid'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.grid')} />
                <PageHeader
                    title={t('operations.grid.title')}
                    description={t('operations.grid.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">
                                {t('operations.grid.updated', { at: readTime(grid.dataUpdatedAt) })}
                            </span>
                            <Button variant="secondary" onClick={() => void grid.refetch()}>
                                {t('operations.grid.refresh')}
                            </Button>
                            <OperationsBack />
                        </div>
                    }
                />
            </div>

            {hasSample ? (
                <SummaryPanel view={view} />
            ) : (
                <section className="card space-y-2 p-6">
                    <h2 className="text-lg font-medium">{t('operations.grid.summary.title')}</h2>
                    <Notice tone="warn">{t('operations.grid.summary.noSample')}</Notice>
                </section>
            )}

            <NodesPanel nodes={view.nodes} />

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
