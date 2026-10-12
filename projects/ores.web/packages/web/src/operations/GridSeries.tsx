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
 * How the compute grid moved over a range, from
 * doc/knowledge/journeys/compute/journey_watch_the_grid.org.
 *
 * Four panels share one time axis, so a spike in one lines up with the same
 * moment in the next: the work in flight, the size of the fleet, what finished
 * in each interval, and how it finished. They read the grid series, which the
 * server serves as columns, and they draw those columns as they are.
 *
 * A bucket with no sample is a gap and is never a zero. The charts mark it, and
 * a range with no sample at all says so rather than drawing a flat line.
 */

import { useState, type ReactNode } from 'react';
import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import { Link } from 'react-router';
import { SERIES_RANGES } from '@ores/wire-protocol/browser';
import type { GridSeriesView, SeriesRange } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Notice, cx } from '../ui/Primitives.js';
import { gridNodePath } from '../shell/paths.js';
import {
    SeriesChart,
    axisLabel,
    type ChartAxis,
    type ChartSeries,
    type ChartTone,
} from '../ui/SeriesChart.js';

/** Where the grid series answer is cached, so a test can seed it per range. */
export const GRID_SERIES_QUERY_KEY = 'operations-grid-series';

/**
 * The change in a total since the previous bucket, or null where it is unknown.
 *
 * The result and outcome counts only grow, so what finished in an interval is
 * the difference between two buckets. The first bucket has no previous one, and
 * a bucket beside a gap has a neighbour that holds no figure, so both are
 * unknown and not zero. A total that falls, which a database reset causes, is
 * drawn as no change rather than as negative work.
 */
export function changeOf(
    totals: readonly number[],
    samples: readonly number[],
): readonly (number | null)[] {
    return totals.map((total, index) => {
        if (index === 0 || (samples[index] ?? 0) === 0 || (samples[index - 1] ?? 0) === 0) {
            return null;
        }
        return Math.max(0, total - (totals[index - 1] ?? 0));
    });
}

/** The axis the columns of a series share. */
export function axisOf(view: GridSeriesView): ChartAxis {
    return {
        startTime: view.start_time,
        bucketSeconds: view.bucket_seconds,
        count: view.bucket_count,
    };
}

/**
 * The work still in flight, by result state.
 *
 * The finished count only grows, so stacking it would flatten the rest; it is
 * drawn as throughput instead.
 */
export function loadSeries(view: GridSeriesView, t: Translator['t']): readonly ChartSeries[] {
    return [
        {
            key: 'inactive',
            label: t('operations.grid.series.load.inactive'),
            values: view.grid.results_inactive,
            tone: 'muted',
        },
        {
            key: 'unsent',
            label: t('operations.grid.series.load.unsent'),
            values: view.grid.results_unsent,
            tone: 'warn',
        },
        {
            key: 'progress',
            label: t('operations.grid.series.load.inProgress'),
            values: view.grid.results_in_progress,
            tone: 'accent',
        },
    ];
}

/** The hosts the grid has, and how many were online and idle. */
export function fleetSeries(view: GridSeriesView, t: Translator['t']): readonly ChartSeries[] {
    return [
        {
            key: 'total',
            label: t('operations.grid.series.fleet.total'),
            values: view.grid.total_hosts,
            tone: 'muted',
        },
        {
            key: 'online',
            label: t('operations.grid.series.fleet.online'),
            values: view.grid.online_hosts,
            tone: 'up',
        },
        {
            key: 'idle',
            label: t('operations.grid.series.fleet.idle'),
            values: view.grid.idle_hosts,
            tone: 'accent',
        },
    ];
}

/** The results that finished in each interval. */
export function throughputSeries(view: GridSeriesView, t: Translator['t']): readonly ChartSeries[] {
    return [
        {
            key: 'finished',
            label: t('operations.grid.series.throughput.finished'),
            values: changeOf(view.grid.results_done, view.grid.samples),
            tone: 'accent',
        },
    ];
}

/**
 * How long a job waits in the queue before a storage token for it stops working.
 *
 * A node takes the storage token with the assignment, and the token lives one
 * hour. A job that waits longer fails on its first step.
 */
export const QUEUE_WAIT_LIMIT_MINUTES = 60;

/** The queue is worth a warning when it needs more than this share of the wait limit. */
const QUEUE_CAUTION_MINUTES = QUEUE_WAIT_LIMIT_MINUTES / 2;

/** The known intervals the rate is read over, newest last. */
const OUTLOOK_INTERVALS = 10;

/** What the queue will do at the rate the grid finishes work. */
export interface QueueOutlook {
    /** The jobs waiting at the newest sample. */
    readonly waiting: number;
    /** The jobs a minute the grid finished over the last intervals, or null when unknown. */
    readonly perMinute: number | null;
    /**
     * The minutes the waiting jobs need at that rate: 0 when nothing waits, null
     * when the rate is unknown, and Infinity when jobs wait and none finish.
     */
    readonly drainMinutes: number | null;
    /** How serious it is: a queue that clears well within the limit says nothing. */
    readonly level: 'ok' | 'caution' | 'over';
}

/**
 * The queue's outlook from the series: the jobs waiting now against the rate
 * the grid finished jobs over the last intervals.
 *
 * It reads the newest bucket that holds a sample, and a rate over the known
 * intervals only, so a gap in the samples is never read as a stretch where
 * nothing finished.
 */
export function queueOutlook(view: GridSeriesView): QueueOutlook {
    const samples = view.grid.samples;
    let newest = -1;
    for (let index = samples.length - 1; index >= 0; index -= 1) {
        if ((samples[index] ?? 0) > 0) {
            newest = index;
            break;
        }
    }
    if (newest < 0) {
        return { waiting: 0, perMinute: null, drainMinutes: null, level: 'ok' };
    }
    const waiting = view.grid.results_unsent[newest] ?? 0;
    const changes = changeOf(view.grid.results_done, samples)
        .slice(0, newest + 1)
        .filter((change): change is number => change !== null)
        .slice(-OUTLOOK_INTERVALS);
    const minutes = (changes.length * view.bucket_seconds) / 60;
    const perMinute =
        changes.length === 0 || minutes <= 0
            ? null
            : changes.reduce((sum, change) => sum + change, 0) / minutes;
    if (waiting === 0) {
        return { waiting, perMinute, drainMinutes: 0, level: 'ok' };
    }
    if (perMinute === null) {
        return { waiting, perMinute, drainMinutes: null, level: 'ok' };
    }
    const drainMinutes = perMinute <= 0 ? Infinity : waiting / perMinute;
    const level =
        drainMinutes > QUEUE_WAIT_LIMIT_MINUTES
            ? 'over'
            : drainMinutes > QUEUE_CAUTION_MINUTES
              ? 'caution'
              : 'ok';
    return { waiting, perMinute, drainMinutes, level };
}

/** Minutes as a person reads them: minutes up to an hour and a half, hours after. */
export function formatWait(minutes: number): string {
    if (minutes < 90) {
        return `${Math.max(1, Math.round(minutes))} min`;
    }
    const hours = minutes / 60;
    return `${hours < 10 ? hours.toFixed(1) : Math.round(hours)} h`;
}

/** The warning for a queue that outgrows the grid, or nothing while it clears in good time. */
export function QueueOutlookNotice({ view }: { readonly view: GridSeriesView }): ReactNode {
    const { t } = useTranslation();
    const outlook = queueOutlook(view);
    if (outlook.level === 'ok') {
        return null;
    }
    const params = {
        count: outlook.waiting,
        rate: outlook.perMinute === null ? 0 : Math.round(outlook.perMinute * 10) / 10,
        time: formatWait(outlook.drainMinutes ?? 0),
    };
    const stalled = outlook.drainMinutes === Infinity;
    const key = stalled ? 'stalled' : outlook.level === 'over' ? 'over' : 'caution';
    return (
        <Notice tone={outlook.level === 'over' ? 'error' : 'warn'}>
            {t(`operations.grid.series.outlook.${key}`, params)}
        </Notice>
    );
}

/** How the results finished in each interval. */
export function outcomeSeries(view: GridSeriesView, t: Translator['t']): readonly ChartSeries[] {
    const samples = view.grid.samples;
    return [
        {
            key: 'success',
            label: t('operations.grid.series.outcomes.success'),
            values: changeOf(view.grid.outcomes_success, samples),
            tone: 'up',
        },
        {
            key: 'clientError',
            label: t('operations.grid.series.outcomes.clientError'),
            values: changeOf(view.grid.outcomes_client_error, samples),
            tone: 'warn',
        },
        {
            key: 'noReply',
            label: t('operations.grid.series.outcomes.noReply'),
            values: changeOf(view.grid.outcomes_no_reply, samples),
            tone: 'down',
        },
    ];
}

/** Whether any bucket of the series holds a sample. */
export function hasAnySample(view: GridSeriesView): boolean {
    return view.grid.samples.some((count) => count > 0);
}

const RANGE_WORDS: Readonly<Record<SeriesRange, 'hour' | 'day' | 'week'>> = {
    '1h': 'hour',
    '24h': 'day',
    '7d': 'week',
};

/** The range choice: three presets, and the one in force is pressed. */
export function RangeBar({
    range,
    onChange,
}: {
    readonly range: SeriesRange;
    readonly onChange: (range: SeriesRange) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div
            role="group"
            aria-label={t('operations.grid.series.rangeLabel')}
            className="inline-flex overflow-hidden rounded-md border border-line"
        >
            {SERIES_RANGES.map((preset) => (
                <button
                    key={preset}
                    type="button"
                    aria-pressed={range === preset}
                    className={cx(
                        'px-3 py-1 text-sm',
                        range === preset ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                    )}
                    onClick={() => onChange(preset)}
                >
                    {t(`operations.grid.series.ranges.${RANGE_WORDS[preset]}`)}
                </button>
            ))}
        </div>
    );
}

/** One titled chart in the grid of panels. */
function Panel({
    title,
    children,
}: {
    readonly title: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <div className="space-y-2 rounded-md border border-line-subtle p-4">
            <h3 className="text-sm font-medium">{title}</h3>
            {children}
        </div>
    );
}

/**
 * The cells of one node in a node-major column.
 *
 * The node series lays each column out row-major, nodes times buckets, so node
 * i owns the entries from i times the bucket count for one bucket count.
 */
export function rowOf<T>(column: readonly T[], node: number, count: number): readonly T[] {
    return column.slice(node * count, (node + 1) * count);
}

/** What a node's cell can be coloured by. */
export const HEATMAP_METRICS = ['work', 'failures', 'bytes'] as const;
export type HeatmapMetric = (typeof HEATMAP_METRICS)[number];

const HEATMAP_TONE: Readonly<Record<HeatmapMetric, ChartTone>> = {
    work: 'accent',
    failures: 'warn',
    bytes: 'accent',
};

/**
 * One node's figure per bucket for a heatmap metric, null where it is unknown.
 *
 * A bucket with no sample is unknown and is never a zero. The failures are the
 * change in the count since the wrapper started, so a bucket next to a gap is
 * unknown too, and a count that falls, as a wrapper restart makes it, is no
 * change.
 */
export function heatmapRow(
    view: GridSeriesView,
    node: number,
    metric: HeatmapMetric,
): readonly (number | null)[] {
    const count = view.bucket_count;
    const samples = rowOf(view.nodes.samples, node, count);
    if (metric === 'failures') {
        return changeOf(rowOf(view.nodes.tasks_failed, node, count), samples);
    }
    const work = rowOf(view.nodes.tasks_since_last, node, count);
    const fetched = rowOf(view.nodes.input_bytes_fetched, node, count);
    const uploaded = rowOf(view.nodes.output_bytes_uploaded, node, count);
    return samples.map((held, index) => {
        if (held === 0) {
            return null;
        }
        return metric === 'work'
            ? (work[index] ?? 0)
            : (fetched[index] ?? 0) + (uploaded[index] ?? 0);
    });
}

/** The name a node row carries: the host registry's, or the start of its id. */
export function nodeLabel(node: GridSeriesView['nodes']['list'][number]): string {
    return node.host ?? node.host_id.slice(0, 8);
}

/** The metric choice of the heatmap: three buttons, the one in force pressed. */
function MetricBar({
    metric,
    onChange,
}: {
    readonly metric: HeatmapMetric;
    readonly onChange: (metric: HeatmapMetric) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div
            role="group"
            aria-label={t('operations.grid.heatmap.metricLabel')}
            className="inline-flex overflow-hidden rounded-md border border-line"
        >
            {HEATMAP_METRICS.map((choice) => (
                <button
                    key={choice}
                    type="button"
                    aria-pressed={metric === choice}
                    className={cx(
                        'px-3 py-1 text-xs',
                        metric === choice ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                    )}
                    onClick={() => onChange(choice)}
                >
                    {t(`operations.grid.heatmap.metrics.${choice}`)}
                </button>
            ))}
        </div>
    );
}

/**
 * One row per node and one column per bucket, coloured by the work the node did.
 *
 * The nodes arrive ordered by machine, so the rows of one computer sit under a
 * heading of its name. A grey cell holds no sample and is not a measured zero;
 * a sampled cell with nothing to show is left clear. Each row's name opens the
 * node's own page.
 */
export function NodeHeatmap({ view }: { readonly view: GridSeriesView }): ReactNode {
    const { t } = useTranslation();
    const [metric, setMetric] = useState<HeatmapMetric>('work');
    const nodes = view.nodes.list;
    const count = view.bucket_count;
    const rows = nodes.map((_, index) => heatmapRow(view, index, metric));
    const maximum = rows.reduce((top, row) => Math.max(top, ...row.map((value) => value ?? 0)), 0);
    const tone = HEATMAP_TONE[metric];
    const axis = axisOf(view);
    const toneClass = tone === 'warn' ? 'text-warn' : 'text-accent';

    return (
        <div className="space-y-3 rounded-md border border-line-subtle p-4">
            <MetricBar metric={metric} onChange={setMetric} />
            {nodes.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('operations.grid.heatmap.none')}</p>
            ) : (
                <ul className="space-y-0.5">
                    {nodes.map((node, index) => {
                        const row = rows[index] ?? [];
                        const newMachine =
                            index === 0 || nodes[index - 1]?.machine !== node.machine;
                        return (
                            <li key={node.host_id}>
                                {newMachine && (
                                    <p className="pt-2 text-xs text-ink-faint">
                                        {node.machine === ''
                                            ? t('operations.grid.machines.unnamed')
                                            : node.machine}
                                    </p>
                                )}
                                <div className="flex items-center gap-2">
                                    <Link
                                        to={gridNodePath(node.host_id)}
                                        className="w-44 shrink-0 truncate text-xs underline"
                                        title={node.host_id}
                                    >
                                        {nodeLabel(node)}
                                    </Link>
                                    <svg
                                        viewBox={`0 0 ${count} 1`}
                                        preserveAspectRatio="none"
                                        shapeRendering="crispEdges"
                                        className={`h-4 w-full rounded-sm bg-line-subtle/40 ${toneClass}`}
                                        role="img"
                                        aria-label={nodeLabel(node)}
                                    >
                                        {row.map((value, bucket) => {
                                            if (value === null) {
                                                return (
                                                    <rect
                                                        key={bucket}
                                                        data-gap=""
                                                        x={bucket}
                                                        y={0}
                                                        width={1}
                                                        height={1}
                                                        className="fill-current text-ink-faint"
                                                        opacity="0.3"
                                                    />
                                                );
                                            }
                                            if (value <= 0 || maximum <= 0) {
                                                return null;
                                            }
                                            return (
                                                <rect
                                                    key={bucket}
                                                    data-cell=""
                                                    x={bucket}
                                                    y={0}
                                                    width={1}
                                                    height={1}
                                                    className="fill-current"
                                                    opacity={(
                                                        0.2 +
                                                        0.8 * (value / maximum)
                                                    ).toFixed(2)}
                                                />
                                            );
                                        })}
                                    </svg>
                                </div>
                            </li>
                        );
                    })}
                </ul>
            )}
            {nodes.length > 0 && (
                <div className="flex items-center justify-between pl-[calc(11rem+0.5rem)] text-[10px] text-ink-faint">
                    <span>{axisLabel(axis, 0)}</span>
                    <span>{axisLabel(axis, Math.floor((count - 1) / 2))}</span>
                    <span>{axisLabel(axis, Math.max(count - 1, 0))}</span>
                </div>
            )}
            <p className="text-xs text-ink-faint">{t('operations.grid.heatmap.legend')}</p>
        </div>
    );
}

/** The four panels of a series that holds at least one sample. */
export function GridSeriesPanels({ view }: { readonly view: GridSeriesView }): ReactNode {
    const { t } = useTranslation();
    const axis = axisOf(view);
    const samples = view.grid.samples;
    return (
        <div className="space-y-4">
            <QueueOutlookNotice view={view} />
            <div className="grid gap-4 lg:grid-cols-2">
            <Panel title={t('operations.grid.series.load.title')}>
                <SeriesChart
                    kind="stacked-area"
                    axis={axis}
                    samples={samples}
                    series={loadSeries(view, t)}
                    caption={t('operations.grid.series.load.caption')}
                />
            </Panel>
            <Panel title={t('operations.grid.series.fleet.title')}>
                <SeriesChart
                    kind="lines"
                    axis={axis}
                    samples={samples}
                    series={fleetSeries(view, t)}
                    caption={t('operations.grid.series.fleet.caption')}
                />
            </Panel>
            <Panel title={t('operations.grid.series.throughput.title')}>
                <SeriesChart
                    kind="stacked-bars"
                    axis={axis}
                    samples={samples}
                    series={throughputSeries(view, t)}
                    caption={t('operations.grid.series.throughput.caption')}
                />
            </Panel>
            <Panel title={t('operations.grid.series.outcomes.title')}>
                <SeriesChart
                    kind="stacked-bars"
                    axis={axis}
                    samples={samples}
                    series={outcomeSeries(view, t)}
                    caption={t('operations.grid.series.outcomes.caption')}
                />
            </Panel>
            </div>
        </div>
    );
}

/** The grid series for a range, read from the server. */
export function useGridSeries(range: SeriesRange): UseQueryResult<GridSeriesView> {
    return useQuery({
        queryKey: [GRID_SERIES_QUERY_KEY, range],
        queryFn: () => api.gridSeries(range),
    });
}

/**
 * What a read of the series shows before it has data: loading, the error, or a
 * range with no sample at all. A range with no sample says so and draws no chart.
 */
export function SeriesStatus({
    series,
    children,
}: {
    readonly series: UseQueryResult<GridSeriesView>;
    readonly children: (view: GridSeriesView) => ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    if (series.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (series.isError) {
        return <Notice tone="error">{series.error.message}</Notice>;
    }
    if (!hasAnySample(series.data)) {
        return <Notice tone="info">{t('operations.grid.series.empty')}</Notice>;
    }
    return <>{children(series.data)}</>;
}

/** The Load tab: a range choice over the four panels. */
export function GridLoadTab({
    range,
    onRange,
}: {
    readonly range: SeriesRange;
    readonly onRange: (range: SeriesRange) => void;
}): ReactNode {
    const { t } = useTranslation();
    const series = useGridSeries(range);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="space-y-1">
                    <h2 className="text-lg font-medium">{t('operations.grid.series.title')}</h2>
                    <p className="text-xs text-ink-faint">{t('operations.grid.series.lead')}</p>
                </div>
                <RangeBar range={range} onChange={onRange} />
            </header>
            <SeriesStatus series={series}>
                {(view) => (
                    <>
                        <GridSeriesPanels view={view} />
                        <p className="text-xs text-ink-faint">
                            {t('operations.grid.series.gapNote')}
                        </p>
                    </>
                )}
            </SeriesStatus>
        </section>
    );
}

/** The heatmap block of the Nodes tab: a range choice over one row per node. */
export function GridNodeLoad({
    range,
    onRange,
}: {
    readonly range: SeriesRange;
    readonly onRange: (range: SeriesRange) => void;
}): ReactNode {
    const { t } = useTranslation();
    const series = useGridSeries(range);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="space-y-1">
                    <h2 className="text-lg font-medium">{t('operations.grid.heatmap.title')}</h2>
                    <p className="text-xs text-ink-faint">{t('operations.grid.heatmap.lead')}</p>
                </div>
                <RangeBar range={range} onChange={onRange} />
            </header>
            <SeriesStatus series={series}>{(view) => <NodeHeatmap view={view} />}</SeriesStatus>
        </section>
    );
}
