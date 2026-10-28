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

import { useState, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import type { PublishedCurvePoint, PublishedCurveSet } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { LineChart } from './LineChart.js';
import { curveStyle } from './synthetic/IrTicksPanel.js';
import { Stat } from './synthetic/TicksPanel.js';
import { Input, Notice } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';
import { PersonCell } from '../access/PersonRef.js';
import {
    DEFAULT_OVERLAY_SETS,
    MAX_OVERLAY_SETS,
    alignOnPillars,
    overlayCaption,
    publishedSetsOf,
    quoted,
} from './publishedCurves.js';

/**
 * The last published sets of one curve and party, overlaid.
 *
 * The tab follows the IR feed's ticks panel: the sets are drawn together, so a
 * move between publishes is visible as a shape and not only as a table of
 * numbers. The newest set is bold green, the older ones grey-blue and thinner,
 * fading with age, and the newest is always the strongest line. The count names
 * how many sets to draw, from one to twenty and starting at five.
 *
 * The newest set's pillar table stands beside the chart: each pillar, the rate,
 * the discount factor it implies and the instant the value was observed at. The
 * chart is the tab's shape and the table is its values, so neither replaces the
 * other.
 */

/** A rate, as a share of one, drawn to three places as a percentage. */
function rate(value: number): string {
    return `${(value * 100).toFixed(3)}%`;
}

/** A value as a number, drawn with the places a published datum keeps. */
function decimal(value: number | undefined, places = 6): string {
    return value === undefined ? '—' : value.toFixed(places);
}

/** The count the tab draws, from the address or the default, held between one and twenty. */
function useCount(): { readonly count: number; readonly set: (next: string) => void } {
    const [search, setSearch] = useSearchParams();
    const asked = Number(search.get('count') ?? DEFAULT_OVERLAY_SETS);
    const count = Math.min(
        MAX_OVERLAY_SETS,
        Math.max(1, Number.isFinite(asked) ? Math.round(asked) : DEFAULT_OVERLAY_SETS),
    );
    const set = (next: string): void => {
        const parsed = Number(next);
        if (!Number.isFinite(parsed)) return;
        const held = Math.min(MAX_OVERLAY_SETS, Math.max(1, Math.round(parsed)));
        const parameters = new URLSearchParams(search);
        parameters.set('count', String(held));
        setSearch(parameters, { replace: true });
    };
    return { count, set };
}

export function PublishedPointsTab({
    sets,
    publishedCount,
}: {
    /** The sets of this curve, oldest first, with the points each holds. */
    readonly sets: readonly PublishedCurveSet[];
    /** How many sets the series holds in all, so the caption can say so. */
    readonly publishedCount: number;
}): ReactNode {
    const { t, language } = useTranslation();
    const { count, set } = useCount();
    const published = publishedSetsOf(sets);
    const drawn = published.slice(-count);
    const newest = published.at(-1);
    const { pillars, values } = alignOnPillars(drawn);
    const caption = overlayCaption(count, publishedCount, drawn.length);

    return (
        <div className="grid items-start gap-4 xl:grid-cols-4">
            <section className="space-y-3 xl:col-span-3">
                <p className="text-sm text-ink-muted">{t('marketdata.published.points.lead')}</p>
                <div className="flex flex-wrap items-end gap-x-6 gap-y-3 rounded-md border border-line p-3">
                    <label className="block">
                        <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                            {t('marketdata.published.points.count')}
                        </span>
                        <Input
                            type="number"
                            min={1}
                            max={MAX_OVERLAY_SETS}
                            className="w-20"
                            value={String(count)}
                            title={t('marketdata.published.points.countHelp')}
                            onChange={(event) => set(event.target.value)}
                        />
                    </label>
                    {newest !== undefined && (
                        <>
                            <Stat label={t('marketdata.published.points.asOf')}>
                                {formatDateTime(newest.as_of, language)}
                            </Stat>
                            <Stat label={t('marketdata.published.points.published')}>
                                {newest.published_at === null
                                    ? '—'
                                    : formatDateTime(newest.published_at, language)}
                            </Stat>
                            <Stat label={t('marketdata.published.points.publisher')}>
                                <PersonCell who={newest.published_by} />
                            </Stat>
                            <Stat label={t('marketdata.published.points.request')}>
                                {newest.request_id === '' ? '—' : newest.request_id.slice(0, 8)}
                            </Stat>
                            <Stat label={t('marketdata.published.points.points')}>
                                {String(newest.point_count)}
                            </Stat>
                        </>
                    )}
                </div>
                <ChartOf newest={newest} pillars={pillars} values={values} drawn={drawn.length} />
                {newest !== undefined && (
                    <p className="text-xs text-ink-faint italic">{t(caption.key, caption.values)}</p>
                )}
            </section>
            {newest !== undefined && <PillarTable points={newest.points} />}
        </div>
    );
}

/** The chart, or the words that say why it cannot be drawn. */
function ChartOf({
    newest,
    pillars,
    values,
    drawn,
}: {
    readonly newest: PublishedCurveSet | undefined;
    readonly pillars: readonly string[];
    readonly values: readonly (number | null)[][];
    readonly drawn: number;
}): ReactNode {
    const { t } = useTranslation();
    if (newest === undefined) {
        return <Notice tone="info">{t('marketdata.published.chart.noPublished')}</Notice>;
    }
    if (newest.points.length === 0) {
        return <Notice tone="info">{t('marketdata.published.chart.noPoints')}</Notice>;
    }
    if (pillars.length < 2) {
        return <Notice tone="info">{t('marketdata.published.chart.onePoint')}</Notice>;
    }
    const plural = drawn === 1 ? 'one' : 'many';
    return (
        <div className="space-y-2 rounded-md border border-line p-3">
            <h3 className="font-mono text-xs font-semibold text-ink">
                {t('marketdata.published.points.chart')}
            </h3>
            <LineChart
                label={t('marketdata.published.points.chart')}
                format={(value) => (value < 1 ? rate(value) : value.toFixed(4))}
                categories={pillars}
                padding={0.1}
                series={values.map((points, index) => ({
                    points,
                    dots: true,
                    strong: index === values.length - 1,
                    ...curveStyle(index, values.length),
                }))}
            />
            <p className="text-xs text-ink-faint italic">
                {t(`marketdata.published.points.legend.${plural}`)}
            </p>
        </div>
    );
}

/** The newest set's pillars: the value, the other side it implies, and when it was observed. */
function PillarTable({ points }: { readonly points: readonly PublishedCurvePoint[] }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <aside className="space-y-2 rounded-md border border-line p-3">
            <h3 className="text-xs font-semibold tracking-wide uppercase">
                {t('marketdata.published.points.pillars')}
            </h3>
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="py-1 font-medium">
                            {t('marketdata.published.points.pillar')}
                        </th>
                        <th className="py-1 font-medium">
                            {t('marketdata.published.points.rate')}
                        </th>
                        <th className="py-1 font-medium">
                            {t('marketdata.published.points.discount')}
                        </th>
                        <th className="py-1 font-medium">
                            {t('marketdata.published.points.observed')}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {points.map((point) => {
                        const { rate: quotedRate, discount } = quoted(point);
                        return (
                            <tr key={point.oresmd_uri} className="border-b border-line-subtle">
                                <td className="py-1 font-mono">{point.pillar}</td>
                                <td className="py-1 font-mono">
                                    {quotedRate === undefined ? '—' : rate(quotedRate)}
                                </td>
                                <td className="py-1 font-mono">{decimal(discount)}</td>
                                <td className="py-1 font-mono text-xs">
                                    {formatDateTime(point.observation_datetime, language)}
                                </td>
                            </tr>
                        );
                    })}
                    {points.length === 0 && (
                        <tr>
                            <td colSpan={4} className="py-1 text-ink-muted">
                                {t('marketdata.published.points.noPillars')}
                            </td>
                        </tr>
                    )}
                </tbody>
            </table>
            {points[0] !== undefined && points[0].value_kind !== '' && (
                <p className="text-xs text-ink-faint">
                    {t('marketdata.published.points.valueNote', { kind: points[0].value_kind })}
                </p>
            )}
        </aside>
    );
}
