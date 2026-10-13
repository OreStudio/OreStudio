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
 * One compute node: who it is, what state its runner is in, and how its work
 * moved over a range.
 *
 * The page opens from a row of the node table or the heatmap. The facts come
 * from the grid read, which holds the node's newest sample and its runner. The
 * charts come from the grid series, where this node is one row of the node
 * matrix, so the page needs no read of its own. A node that reported no sample
 * in the range says so, and a bucket with no sample is a gap and never a zero.
 */

import { useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useParams } from 'react-router';
import type { GridNodeRow, GridSeriesView, SeriesRange } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { AreaTrail, type TrailStep } from '../shell/AreaTrail.js';
import { gridMachinePath } from '../shell/paths.js';
import { Notice, PageHeader } from '../ui/Primitives.js';
import { SeriesChart, type ChartSeries } from '../ui/SeriesChart.js';
import { GRID_QUERY_KEY, asGiB, asMiB, formatAge, instanceTail } from './GridPage.js';
import { RangeBar, SeriesStatus, axisOf, changeOf, rowOf, useGridSeries } from './GridSeries.js';
import { InstanceStateTag } from './OperationsParts.js';
import { LinkButton } from '../ui/Primitives.js';

/** The figures of one node over the range, each a column on the series' time axis. */
export interface NodeColumns {
    readonly samples: readonly number[];
    readonly tasksDone: readonly number[];
    readonly failures: readonly (number | null)[];
    readonly meanMs: readonly (number | null)[];
    readonly longestMs: readonly (number | null)[];
    readonly fetched: readonly number[];
    readonly uploaded: readonly number[];
}

/**
 * The columns of the node at an index of the series' node list.
 *
 * The failures are the change in the wrapper's failure count, so a bucket next
 * to a gap is unknown. The task times are unknown in a bucket that finished no
 * task, because a mean of nothing is not zero.
 */
export function nodeColumns(view: GridSeriesView, index: number): NodeColumns {
    const count = view.bucket_count;
    const samples = rowOf(view.nodes.samples, index, count);
    const done = rowOf(view.nodes.tasks_since_last, index, count);
    const mean = rowOf(view.nodes.avg_task_duration_ms, index, count);
    const longest = rowOf(view.nodes.max_task_duration_ms, index, count);
    return {
        samples,
        tasksDone: done,
        failures: changeOf(rowOf(view.nodes.tasks_failed, index, count), samples),
        meanMs: mean.map((value, bucket) => ((done[bucket] ?? 0) > 0 ? value : null)),
        longestMs: longest.map((value, bucket) => ((done[bucket] ?? 0) > 0 ? value : null)),
        fetched: rowOf(view.nodes.input_bytes_fetched, index, count),
        uploaded: rowOf(view.nodes.output_bytes_uploaded, index, count),
    };
}

/** A duration in milliseconds as a person reads it: milliseconds, or seconds from a second up. */
export function formatMs(value: number): string {
    return value >= 1000 ? `${(value / 1000).toFixed(1)} s` : `${String(Math.round(value))} ms`;
}

/** A byte count as mebibytes. */
export function formatMiB(value: number): string {
    return `${(value / 1024 / 1024).toFixed(value >= 10 * 1024 * 1024 ? 0 : 1)} MiB`;
}

function Fact({
    label,
    children,
}: {
    readonly label: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <div className="space-y-1">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="font-mono text-sm">{children}</dd>
        </div>
    );
}

/** What the grid read holds about one node. */
function NodeFacts({ node }: { readonly node: GridNodeRow }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card p-6">
            <dl className="grid gap-x-8 gap-y-4 sm:grid-cols-2 lg:grid-cols-4">
                <Fact label={t('operations.grid.node.machine')}>
                    {node.machine === '' ? '—' : node.machine}
                </Fact>
                <Fact label={t('operations.grid.node.host')}>{node.host ?? '—'}</Fact>
                <Fact label={t('operations.grid.node.status')}>
                    <InstanceStateTag state={node.state} />
                </Fact>
                <Fact label={t('operations.grid.node.heartbeat')}>
                    {formatAge(node.seconds_since_hb, t)}
                </Fact>
                <Fact label={t('operations.grid.node.version')}>{node.version ?? '—'}</Fact>
                <Fact label={t('operations.grid.node.instance')}>
                    {node.instance_id === null ? '—' : instanceTail(node.instance_id)}
                </Fact>
                <Fact label={t('operations.grid.node.done')}>{node.tasks_completed}</Fact>
                <Fact label={t('operations.grid.node.failed')}>{node.tasks_failed}</Fact>
                <Fact label={t('operations.grid.node.fetched')}>
                    {asGiB(node.input_bytes_fetched, t)}
                </Fact>
                <Fact label={t('operations.grid.node.uploaded')}>
                    {asMiB(node.output_bytes_uploaded, t)}
                </Fact>
                <Fact label={t('operations.grid.node.hostId')}>
                    <span className="break-all text-xs">{node.host_id}</span>
                </Fact>
            </dl>
        </section>
    );
}

/** The four charts of one node, from its row of the node matrix. */
export function NodeCharts({
    view,
    index,
    t,
}: {
    readonly view: GridSeriesView;
    readonly index: number;
    readonly t: Translator['t'];
}): ReactNode {
    const columns = nodeColumns(view, index);
    const axis = axisOf(view);
    const tone = (
        key: string,
        label: string,
        values: readonly (number | null)[],
        kind: ChartSeries['tone'],
    ) => ({ key, label, values, tone: kind }) satisfies ChartSeries;
    const panel = (title: string, chart: ReactNode): ReactNode => (
        <div className="space-y-2 rounded-md border border-line-subtle p-4">
            <h3 className="text-sm font-medium">{title}</h3>
            {chart}
        </div>
    );
    return (
        <div className="grid gap-4 lg:grid-cols-2">
            {panel(
                t('operations.grid.node.tasks.title'),
                <SeriesChart
                    kind="stacked-bars"
                    axis={axis}
                    samples={columns.samples}
                    series={[
                        tone(
                            'done',
                            t('operations.grid.node.tasks.done'),
                            columns.tasksDone,
                            'accent',
                        ),
                    ]}
                    caption={t('operations.grid.node.tasks.caption')}
                />,
            )}
            {panel(
                t('operations.grid.node.failures.title'),
                <SeriesChart
                    kind="stacked-bars"
                    axis={axis}
                    samples={columns.samples}
                    series={[
                        tone(
                            'failed',
                            t('operations.grid.node.failures.failed'),
                            columns.failures,
                            'warn',
                        ),
                    ]}
                    caption={t('operations.grid.node.failures.caption')}
                />,
            )}
            {panel(
                t('operations.grid.node.time.title'),
                <SeriesChart
                    kind="lines"
                    axis={axis}
                    samples={columns.samples}
                    series={[
                        tone('mean', t('operations.grid.node.time.mean'), columns.meanMs, 'accent'),
                        tone(
                            'longest',
                            t('operations.grid.node.time.longest'),
                            columns.longestMs,
                            'warn',
                        ),
                    ]}
                    caption={t('operations.grid.node.time.caption')}
                    format={formatMs}
                />,
            )}
            {panel(
                t('operations.grid.node.bytes.title'),
                <SeriesChart
                    kind="stacked-bars"
                    axis={axis}
                    samples={columns.samples}
                    series={[
                        tone(
                            'fetched',
                            t('operations.grid.node.bytes.fetched'),
                            columns.fetched,
                            'accent',
                        ),
                        tone(
                            'uploaded',
                            t('operations.grid.node.bytes.uploaded'),
                            columns.uploaded,
                            'up',
                        ),
                    ]}
                    caption={t('operations.grid.node.bytes.caption')}
                    format={formatMiB}
                />,
            )}
        </div>
    );
}

export function GridNodePage(): ReactNode {
    const { t } = useTranslation();
    const { hostId = '' } = useParams();
    const [range, setRange] = useState<SeriesRange>('1h');
    const grid = useQuery({ queryKey: GRID_QUERY_KEY, queryFn: api.grid });
    const series = useGridSeries(range);

    const node = grid.data?.nodes.find((candidate) => candidate.host_id === hostId);
    const label = node?.host ?? hostId.slice(0, 8);
    const trail: readonly TrailStep[] = [
        { label: t('operations.screens.grid'), to: '/operations/grid' },
        ...(node !== undefined && node.machine !== ''
            ? [{ label: node.machine, to: gridMachinePath(node.machine) }]
            : []),
    ];

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" steps={trail} screen={label} />
                <PageHeader
                    title={label}
                    description={t('operations.grid.node.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            <LinkButton to="/operations/grid?tab=nodes" variant="secondary">
                                {t('operations.grid.node.back')}
                            </LinkButton>
                        </div>
                    }
                />
            </div>
            {grid.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {grid.isError && <Notice tone="error">{grid.error.message}</Notice>}
            {grid.isSuccess && node === undefined && (
                <Notice tone="warn">{t('operations.grid.node.notFound')}</Notice>
            )}
            {node !== undefined && <NodeFacts node={node} />}
            {node !== undefined && (
                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-3">
                        <div className="space-y-1">
                            <h2 className="text-lg font-medium">
                                {t('operations.grid.node.charts')}
                            </h2>
                            <p className="text-xs text-ink-faint">
                                {t('operations.grid.series.lead')}
                            </p>
                        </div>
                        <RangeBar range={range} onChange={setRange} />
                    </header>
                    <SeriesStatus series={series}>
                        {(view) => {
                            const index = view.nodes.list.findIndex(
                                (candidate) => candidate.host_id === hostId,
                            );
                            return index < 0 ? (
                                <Notice tone="info">{t('operations.grid.node.silent')}</Notice>
                            ) : (
                                <NodeCharts view={view} index={index} t={t} />
                            );
                        }}
                    </SeriesStatus>
                </section>
            )}
        </div>
    );
}
