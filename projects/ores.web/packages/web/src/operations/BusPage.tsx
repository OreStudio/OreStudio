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
 * Watch the message bus, from
 * doc/knowledge/journeys/operations/journey_watch_the_message_bus.org.
 *
 * The vitals are one reading of the newest sample and one movement across the
 * range; the counters are running totals since the NATS server started, so the
 * movement is a subtraction the screen makes rather than a number the server
 * sends. The range is a preset the deployment turns into a window, because the
 * read's start is inclusive and its end exclusive and the deployment's clock
 * is the one the samples were stamped against.
 *
 * The stream table draws one row per stream the route read. No operation lists
 * the streams that exist or have samples, so the streams are named rather than
 * discovered, and the screen states that as an open gap rather than pretending
 * the table is the whole bus.
 */

import { useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';
import type { BusView } from '@ores/wire-protocol/browser';
import { api, type BusRange } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';
import { AreaTrail } from '../shell/AreaTrail.js';

/** The range presets the screen offers, in the order it shows them. */
const RANGES: readonly BusRange[] = ['15m', '1h', '6h'];

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    '7B820710-161C-4926-B5AA-5EF2772A3652',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
];

/** Where the bus read's answer is cached, by the range it was read for. */
export const BUS_QUERY_KEY = 'operations-bus' as const;

/**
 * The instant a stored sample was taken, as a wall clock a person reads, in UTC.
 *
 * Nothing when no sample is stored, or the time is unreadable: a reading
 * without its age cannot be trusted, so an unreadable time is an absent one
 * rather than a made-up one.
 */
export function sampleTime(at: string | null): string | undefined {
    if (at === null || !isWireTimestamp(at)) {
        return undefined;
    }
    return `${fromWireTimestamp(at).toISOString().slice(11, 19)} UTC`;
}

/** A byte count in MB, as the prototype states memory and stored bytes. */
export function asMiB(bytes: number, t: Translator['t']): string {
    return t('operations.bus.units.mib', { value: Math.round(bytes / 1024 / 1024) });
}

/** The newest sample's reading of the server, and the movement across the range. */
function VitalsPanel({ view }: { readonly view: BusView }): ReactNode {
    const { t } = useTranslation();
    const newest = view.samples[0];
    const oldest = view.samples[view.samples.length - 1];
    if (newest === undefined || oldest === undefined) {
        return null;
    }
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.bus.vitals.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('operations.bus.vitals.newestSample', {
                        at: sampleTime(newest.sampled_at) ?? '',
                    })}
                </span>
            </header>
            <div className="grid gap-x-8 gap-y-3 text-sm sm:grid-cols-4">
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.bus.vitals.connections')}
                    </span>
                    <span className="font-mono">{newest.connections}</span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.bus.vitals.memory')}
                    </span>
                    <span className="font-mono">{asMiB(newest.mem_bytes, t)}</span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.bus.vitals.slowConsumers')}
                    </span>
                    <Tag tone={newest.slow_consumers > 0 ? 'warn' : 'neutral'}>
                        {newest.slow_consumers}
                    </Tag>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">
                        {t('operations.bus.vitals.totals')}
                    </span>
                    <span className="font-mono">
                        {t('operations.bus.vitals.totalsValue', {
                            inMsgs: newest.in_msgs,
                            outMsgs: newest.out_msgs,
                        })}
                    </span>
                </div>
            </div>
            <p className="text-xs text-ink-faint">
                {t('operations.bus.vitals.movement', {
                    from: sampleTime(oldest.sampled_at) ?? '',
                    to: sampleTime(newest.sampled_at) ?? '',
                    inMsgs: newest.in_msgs - oldest.in_msgs,
                    outMsgs: newest.out_msgs - oldest.out_msgs,
                    inBytes: asMiB(newest.in_bytes - oldest.in_bytes, t),
                    outBytes: asMiB(newest.out_bytes - oldest.out_bytes, t),
                })}
            </p>
        </section>
    );
}

/** One row per stream the route read: its name, what it stores and its consumers. */
function StreamsPanel({ streams }: { readonly streams: BusView['streams'] }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.bus.streams.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {streams.length === 0
                        ? t('operations.bus.streams.noSamples')
                        : t('operations.bus.streams.count', { count: streams.length })}
                </span>
            </header>
            {streams.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('operations.bus.streams.empty')}</p>
            ) : (
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">
                                {t('operations.bus.streams.columns.stream')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.bus.streams.columns.messages')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.bus.streams.columns.bytes')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('operations.bus.streams.columns.consumers')}
                            </th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {streams.map((row) => (
                            <tr key={row.stream_name}>
                                <td className="py-2 font-mono">{row.stream_name}</td>
                                <td className="py-2 font-mono">{row.messages}</td>
                                <td className="py-2 font-mono">{asMiB(row.bytes, t)}</td>
                                <td className="py-2 font-mono">{row.consumer_count}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

/** The messages-in series the range returned, drawn oldest first. */
function TrendPanel({ samples }: { readonly samples: BusView['samples'] }): ReactNode {
    const { t } = useTranslation();
    const values = samples.map((sample) => sample.in_msgs).reverse();
    const points = sparkPoints(values, 640, 80);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.bus.trend.title')}</h2>
                <span className="text-xs text-ink-faint">{t('operations.bus.trend.lead')}</span>
            </header>
            <svg viewBox="0 0 640 80" className="h-24 w-full text-accent" role="img">
                <title>{t('operations.bus.trend.caption')}</title>
                <polyline points={points} fill="none" stroke="currentColor" strokeWidth="2" />
            </svg>
            <p className="text-xs text-ink-faint">{t('operations.bus.trend.note')}</p>
        </section>
    );
}

/** The polyline of a series, scaled into the given box. */
export function sparkPoints(values: readonly number[], width: number, height: number): string {
    if (values.length === 0) {
        return '';
    }
    const lowest = Math.min(...values);
    const highest = Math.max(...values);
    const span = highest - lowest === 0 ? 1 : highest - lowest;
    return values
        .map((value, index) => {
            const x = values.length === 1 ? width / 2 : (index / (values.length - 1)) * width;
            const y = height - ((value - lowest) / span) * (height - 8) - 4;
            return `${x.toFixed(1)},${y.toFixed(1)}`;
        })
        .join(' ');
}

export function BusPage(): ReactNode {
    const { t } = useTranslation();
    const [range, setRange] = useState<BusRange>('1h');
    const [applied, setApplied] = useState<BusRange>('1h');
    const bus = useQuery({
        queryKey: [BUS_QUERY_KEY, applied],
        queryFn: () => api.bus(applied),
    });

    /*
     * Apply re-reads: a new range moves the query to its own key, and the same
     * range asks the current one again, so the person can refresh a live
     * reading without changing the window.
     */
    const applyRange = (): void => {
        if (range === applied) {
            void bus.refetch();
        } else {
            setApplied(range);
        }
    };

    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.bus.gap.names.title'),
            body: t('operations.bus.gap.names.body'),
            journey: t('operations.journeys.bus'),
        },
        {
            title: t('operations.bus.gap.rates.title'),
            body: t('operations.bus.gap.rates.body'),
            journey: t('operations.journeys.bus'),
        },
        {
            title: t('operations.bus.gap.limit.title'),
            body: t('operations.bus.gap.limit.body'),
            journey: t('operations.journeys.bus'),
        },
        {
            title: t('operations.bus.gap.slowConsumer.title'),
            body: t('operations.bus.gap.slowConsumer.body'),
            journey: t('operations.journeys.bus'),
        },
        {
            title: t('operations.bus.gap.permission.title'),
            body: t('operations.bus.gap.permission.body'),
            journey: t('operations.journeys.bus'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.bus')} />
                <PageHeader
                    title={t('operations.bus.title')}
                    description={t('operations.bus.description')}
                    actions={
                        <div className="flex items-end gap-3">
                            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                                {t('operations.bus.range.label')}
                                <Select
                                    value={range}
                                    onChange={(event) => setRange(event.target.value as BusRange)}
                                >
                                    {RANGES.map((option) => (
                                        <option key={option} value={option}>
                                            {t(`operations.bus.range.${option}`)}
                                        </option>
                                    ))}
                                </Select>
                            </label>
                            <Button variant="secondary" onClick={applyRange}>
                                {t('operations.bus.range.apply')}
                            </Button>
                            <OperationsBack />
                        </div>
                    }
                />
            </div>

            {bus.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : bus.isError ? (
                <Notice tone="error">{bus.error.message}</Notice>
            ) : (
                <BusBody view={bus.data} />
            )}

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}

function BusBody({ view }: { readonly view: BusView }): ReactNode {
    const { t } = useTranslation();
    if (view.samples.length === 0) {
        return (
            <>
                <section className="card space-y-2 p-6">
                    <h2 className="text-lg font-medium">{t('operations.bus.vitals.title')}</h2>
                    <Notice tone="warn">{t('operations.bus.vitals.noSamples')}</Notice>
                </section>
                <StreamsPanel streams={view.streams} />
            </>
        );
    }
    return (
        <>
            <VitalsPanel view={view} />
            <StreamsPanel streams={view.streams} />
            <TrendPanel samples={view.samples} />
        </>
    );
}
