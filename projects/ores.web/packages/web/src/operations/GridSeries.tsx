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
import { useQuery } from '@tanstack/react-query';
import { SERIES_RANGES } from '@ores/wire-protocol/browser';
import type { GridSeriesView, SeriesRange } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Notice, cx } from '../ui/Primitives.js';
import { SeriesChart, type ChartAxis, type ChartSeries } from '../ui/SeriesChart.js';

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
export function throughputSeries(
    view: GridSeriesView,
    t: Translator['t'],
): readonly ChartSeries[] {
    return [
        {
            key: 'finished',
            label: t('operations.grid.series.throughput.finished'),
            values: changeOf(view.grid.results_done, view.grid.samples),
            tone: 'accent',
        },
    ];
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
function RangeBar({
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

/** The four panels of a series that holds at least one sample. */
export function GridSeriesPanels({ view }: { readonly view: GridSeriesView }): ReactNode {
    const { t } = useTranslation();
    const axis = axisOf(view);
    const samples = view.grid.samples;
    return (
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
    );
}

/** The section: a range choice over the four panels, read from the server. */
export function GridSeriesSection(): ReactNode {
    const { t } = useTranslation();
    const [range, setRange] = useState<SeriesRange>('1h');
    const series = useQuery({
        queryKey: [GRID_SERIES_QUERY_KEY, range],
        queryFn: () => api.gridSeries(range),
    });

    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="space-y-1">
                    <h2 className="text-lg font-medium">{t('operations.grid.series.title')}</h2>
                    <p className="text-xs text-ink-faint">{t('operations.grid.series.lead')}</p>
                </div>
                <RangeBar range={range} onChange={setRange} />
            </header>
            {series.isPending && (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            )}
            {series.isError && <Notice tone="error">{series.error.message}</Notice>}
            {series.isSuccess &&
                (hasAnySample(series.data) ? (
                    <>
                        <GridSeriesPanels view={series.data} />
                        <p className="text-xs text-ink-faint">
                            {t('operations.grid.series.gapNote')}
                        </p>
                    </>
                ) : (
                    <Notice tone="info">{t('operations.grid.series.empty')}</Notice>
                ))}
        </section>
    );
}
