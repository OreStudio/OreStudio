/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { z } from 'zod';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Notice } from '../../ui/Primitives.js';
import { formatDateTime } from '../../ui/Time.js';
import { garchText } from './VarianceCalibration.js';

export const CALIBRATIONS = 'variance-calibrations';
export const INPUTS = 'variance-calibration-inputs';
export const POINTS = 'variance-calibration-input-points';
export const CALIBRATION_ENTITY = 'ores.synthetic.variance_calibration';
export const INPUT_ENTITY = 'ores.synthetic.variance_calibration_input';

const number = z.number().nullish();

/** One stored variance calibration of a pair, as its record holds it. */
const storedCalibrationSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
    state: z.string(),
    state_reason: z.string().default(''),
    method: z.string(),
    input_id: z.string().nullish(),
    alpha: number,
    gamma: number,
    beta: number,
    persistence: number,
    long_run_variance: number,
    starting_variance_ratio: number,
    returns_used: z.int().default(0),
    recorded_at: z.string().default(''),
});

export type StoredCalibration = z.infer<typeof storedCalibrationSchema>;

/** One captured fixing of an input: its date, and the return it gave when it was used. */
const pointSchema = z.looseObject({
    fixing_date: z.string(),
    log_return: number,
    used: z.boolean(),
});

export type CapturedPoint = z.infer<typeof pointSchema>;

/** Every stored variance calibration of a pair, oldest first. */
export function usePairCalibrations(base: string, quote: string) {
    return useQuery({
        queryKey: ['records', CALIBRATIONS, base, quote],
        enabled: base !== '' && quote !== '',
        queryFn: async () => {
            const page = await api.syntheticPage(CALIBRATIONS, {
                offset: 0,
                limit: 500,
                search: '',
                sort: '',
                descending: false,
                filters: { base_currency_code: base, quote_currency_code: quote },
            });
            return z
                .array(storedCalibrationSchema)
                .parse(page.rows)
                .toSorted((a, b) => a.recorded_at.localeCompare(b.recorded_at));
        },
    });
}

/**
 * A pair's calibrations in the order of the fixings each was fitted to: an authored shape,
 * which read none, first; then each fit by its input's window end, and by its last change
 * where two fits read to the same day.
 */
export function orderByWindow(
    calibrations: readonly StoredCalibration[],
    windowEnd: ReadonlyMap<string, string>,
): StoredCalibration[] {
    const key = (c: StoredCalibration): string => windowEnd.get(c.input_id ?? '') ?? '';
    return calibrations.toSorted(
        (a, b) => key(a).localeCompare(key(b)) || a.recorded_at.localeCompare(b.recorded_at),
    );
}

/** Every stored input of a pair, as the feed history reads them. */
export function usePairInputs(base: string, quote: string) {
    return useQuery({
        queryKey: ['records', INPUTS, base, quote],
        enabled: base !== '' && quote !== '',
        queryFn: async () =>
            (
                await api.syntheticPage(INPUTS, {
                    offset: 0,
                    limit: 500,
                    search: '',
                    sort: '',
                    descending: false,
                    filters: { base_currency_code: base, quote_currency_code: quote },
                })
            ).rows,
    });
}

/** The points of one input, in date order. */
export function useInputPoints(inputId: string) {
    return useQuery({
        queryKey: ['records', POINTS, inputId],
        enabled: inputId !== '',
        queryFn: async () =>
            z
                .array(pointSchema)
                .parse(await api.syntheticRecords(POINTS, inputId))
                .toSorted((a, b) => a.fixing_date.localeCompare(b.fixing_date)),
    });
}

/** One day of a fitted variance: the day, the volatility it expected, and the move it saw. */
export interface VolatilityDay {
    readonly date: string;
    readonly volatility: number;
    readonly move: number;
}

/**
 * The daily volatility a GJR-GARCH calibration expected over the input it was fitted from.
 *
 * The recursion is the fit's: the used returns less their mean are the shocks, the first
 * day starts at the long-run variance, and each day's variance follows from the shock
 * before it. The variance after the last shock over the long-run level is the calibration's
 * starting ratio, so a caller can check the series against the stored record.
 */
export function conditionalVolatility(
    points: readonly CapturedPoint[],
    calibration: {
        readonly alpha: number;
        readonly gamma: number;
        readonly beta: number;
        readonly persistence: number;
        readonly long_run_variance: number;
    },
): { readonly days: readonly VolatilityDay[]; readonly endRatio: number } {
    const used = points.filter(
        (p): p is CapturedPoint & { log_return: number } =>
            p.used && p.log_return !== null && p.log_return !== undefined,
    );
    if (used.length === 0) {
        return { days: [], endRatio: Number.NaN };
    }
    const mean = used.reduce((sum, p) => sum + p.log_return, 0) / used.length;
    const { alpha, gamma, beta, persistence, long_run_variance: longRun } = calibration;
    const omega = longRun * (1 - persistence);
    let variance = longRun;
    const days: VolatilityDay[] = [];
    for (const point of used) {
        const shock = point.log_return - mean;
        days.push({
            date: point.fixing_date,
            volatility: Math.sqrt(variance),
            move: Math.abs(shock),
        });
        const reaction = shock < 0 ? alpha + gamma : alpha;
        variance = omega + reaction * shock * shock + beta * variance;
    }
    return { days, endRatio: variance / longRun };
}

const percent = (value: number): string => `${(value * 100).toFixed(2)}%`;

const CHART_WIDTH = 720;
const CHART_HEIGHT = 200;

/**
 * The fitted daily volatility over the input window, drawn over the size of each day's move,
 * with the long-run level as a dashed line. Both are daily percentages, so one axis serves.
 */
export function VolatilityChart({
    days,
    longRun,
}: {
    readonly days: readonly VolatilityDay[];
    readonly longRun: number;
}): ReactNode {
    const { t } = useTranslation();
    const [hover, setHover] = useState<number | null>(null);
    if (days.length < 2) {
        return null;
    }
    const top = Math.max(...days.map((d) => Math.max(d.volatility, d.move)), longRun) * 1.05;
    const plotWidth = CHART_WIDTH;
    const plotHeight = CHART_HEIGHT;
    const x = (i: number): number => (i / (days.length - 1)) * plotWidth;
    const y = (value: number): number => plotHeight - (value / top) * plotHeight;
    const line = days
        .map((d, i) => `${i === 0 ? 'M' : 'L'} ${x(i).toFixed(1)} ${y(d.volatility).toFixed(1)}`)
        .join(' ');
    const barWidth = Math.max(1, plotWidth / days.length - 0.5);
    const ticks = [0, top / 2, top];
    const shown = hover === null ? undefined : days[hover];
    return (
        <figure className="space-y-2" data-volatility-chart>
            <div className="flex flex-wrap items-center gap-4 text-xs text-ink-muted">
                <span className="flex items-center gap-1.5">
                    <span className="inline-block h-0.5 w-4 bg-accent" />
                    {t('marketdata.calibration.variance.chart.volatility')}
                </span>
                <span className="flex items-center gap-1.5">
                    <span className="inline-block h-2 w-1 bg-ink-faint" />
                    {t('marketdata.calibration.variance.chart.move')}
                </span>
                <span className="flex items-center gap-1.5">
                    <span className="inline-block w-4 border-t border-dashed border-ink-muted" />
                    {t('marketdata.calibration.variance.chart.longRun', {
                        value: percent(Math.sqrt(longRun)),
                    })}
                </span>
            </div>
            <div className="relative pl-12">
                {ticks.map((tick) => (
                    <span
                        key={`tick-${String(tick)}`}
                        className="absolute left-0 w-10 -translate-y-1/2 text-right text-[10px] tabular-nums text-ink-faint"
                        style={{ top: `${String((y(tick) / CHART_HEIGHT) * 224)}px` }}
                    >
                        {percent(tick)}
                    </span>
                ))}
                <svg
                    className="h-56 w-full"
                    viewBox={`0 0 ${CHART_WIDTH} ${CHART_HEIGHT}`}
                    preserveAspectRatio="none"
                    role="img"
                    aria-label={t('marketdata.calibration.variance.chart.label')}
                    onMouseLeave={() => setHover(null)}
                    onMouseMove={(event) => {
                        const box = event.currentTarget.getBoundingClientRect();
                        const at = ((event.clientX - box.left) / box.width) * CHART_WIDTH;
                        const index = Math.round((at / plotWidth) * (days.length - 1));
                        setHover(Math.min(days.length - 1, Math.max(0, index)));
                    }}
                >
                    {ticks.map((tick) => (
                        <g key={tick}>
                            <line
                                x1={0}
                                x2={CHART_WIDTH}
                                y1={y(tick)}
                                y2={y(tick)}
                                className="stroke-line-subtle"
                                strokeWidth="1"
                                vectorEffect="non-scaling-stroke"
                            />
                        </g>
                    ))}
                    {days.map((d, i) => (
                        <rect
                            key={d.date}
                            x={x(i) - barWidth / 2}
                            y={y(d.move)}
                            width={barWidth}
                            height={plotHeight - y(d.move)}
                            className="fill-ink-faint"
                            opacity="0.45"
                        />
                    ))}
                    <line
                        x1={0}
                        x2={CHART_WIDTH}
                        y1={y(Math.sqrt(longRun))}
                        y2={y(Math.sqrt(longRun))}
                        className="stroke-ink-muted"
                        strokeDasharray="4 4"
                        strokeWidth="1"
                        vectorEffect="non-scaling-stroke"
                    />
                    <path
                        d={line}
                        fill="none"
                        className="stroke-accent"
                        strokeWidth="2"
                        vectorEffect="non-scaling-stroke"
                    />
                    {hover !== null && (
                        <line
                            x1={x(hover)}
                            x2={x(hover)}
                            y1={0}
                            y2={plotHeight}
                            className="stroke-ink-muted"
                            strokeWidth="1"
                            vectorEffect="non-scaling-stroke"
                        />
                    )}
                </svg>
                <div className="mt-1 flex justify-between text-[10px] tabular-nums text-ink-faint">
                    <span>{days[0]?.date}</span>
                    <span>{days[Math.floor((days.length - 1) / 2)]?.date}</span>
                    <span>{days[days.length - 1]?.date}</span>
                </div>
                {shown !== undefined && (
                    <div
                        className="pointer-events-none absolute top-1 right-2 rounded-chip border border-line bg-surface-raised px-2 py-1 text-xs shadow"
                        role="status"
                    >
                        <div className="text-ink">{shown.date}</div>
                        <div className="tabular-nums text-ink-muted">
                            {t('marketdata.calibration.variance.chart.volatility')}{' '}
                            {percent(shown.volatility)}
                        </div>
                        <div className="tabular-nums text-ink-muted">
                            {t('marketdata.calibration.variance.chart.move')} {percent(shown.move)}
                        </div>
                    </div>
                )}
            </div>
            <figcaption className="text-xs text-ink-faint">
                {t('marketdata.calibration.variance.chart.caption', {
                    days: String(days.length),
                })}
            </figcaption>
        </figure>
    );
}

/** One parameter across a pair's calibrations, oldest first, as a small chart of its own. */
function ParameterMultiple({
    label,
    values,
}: {
    readonly label: string;
    readonly values: readonly { readonly at: string; readonly value: number }[];
}): ReactNode {
    const width = 160;
    const height = 48;
    const low = Math.min(...values.map((v) => v.value));
    const high = Math.max(...values.map((v) => v.value));
    const span = high - low;
    const x = (i: number): number =>
        values.length === 1 ? width / 2 : 6 + (i / (values.length - 1)) * (width - 12);
    const y = (value: number): number =>
        span === 0 ? height / 2 : height - 6 - ((value - low) / span) * (height - 12);
    const last = values[values.length - 1];
    return (
        <div className="rounded-chip border border-line-subtle bg-surface-base px-2.5 py-2">
            <div className="flex items-baseline justify-between gap-2">
                <span className="text-xs text-ink-muted">{label}</span>
                <span className="font-mono text-sm tabular-nums">
                    {last === undefined ? '' : garchText(last.value)}
                </span>
            </div>
            <svg className="mt-1 h-12 w-full overflow-visible" viewBox={`0 0 ${width} ${height}`}>
                {values.length > 1 && (
                    <path
                        d={values
                            .map(
                                (v, i) =>
                                    `${i === 0 ? 'M' : 'L'} ${x(i).toFixed(1)} ${y(v.value).toFixed(1)}`,
                            )
                            .join(' ')}
                        fill="none"
                        className="stroke-accent"
                        strokeWidth="2"
                        vectorEffect="non-scaling-stroke"
                    />
                )}
                {values.map((v, i) => (
                    <circle
                        key={v.at}
                        cx={x(i)}
                        cy={y(v.value)}
                        r="4"
                        className="fill-accent stroke-surface-base"
                        strokeWidth="2"
                    >
                        <title>{`${v.at}: ${garchText(v.value)}`}</title>
                    </circle>
                ))}
            </svg>
        </div>
    );
}

/**
 * The pair's calibrations over time: a small chart for each parameter, since alpha, gamma,
 * beta and persistence live on different scales and one axis would flatten all but beta.
 */
export function ParameterHistory({
    calibrations,
}: {
    readonly calibrations: readonly StoredCalibration[];
}): ReactNode {
    const { t, language } = useTranslation();
    const shaped = calibrations.filter(
        (c) => c.alpha != null && c.gamma != null && c.beta != null && c.persistence != null,
    );
    if (shaped.length === 0) {
        return null;
    }
    const series = (pick: (c: StoredCalibration) => number | null | undefined) =>
        shaped.map((c) => ({ at: formatDateTime(c.recorded_at, language), value: pick(c) ?? 0 }));
    return (
        <div className="grid gap-2 sm:grid-cols-2 lg:grid-cols-4" data-parameter-history>
            <ParameterMultiple
                label={t('marketdata.calibration.variance.alpha')}
                values={series((c) => c.alpha)}
            />
            <ParameterMultiple
                label={t('marketdata.calibration.variance.gamma')}
                values={series((c) => c.gamma)}
            />
            <ParameterMultiple
                label={t('marketdata.calibration.variance.beta')}
                values={series((c) => c.beta)}
            />
            <ParameterMultiple
                label={t('marketdata.calibration.variance.persistence')}
                values={series((c) => c.persistence)}
            />
        </div>
    );
}

/**
 * Says that a fit of the pair waits for review, with the way to it. A proposed calibration
 * changes nothing until a person approves it, so a page that shows the pair says so.
 */
export function AwaitingReview({
    calibrations,
    to,
}: {
    readonly calibrations: readonly StoredCalibration[];
    readonly to?: string;
}): ReactNode {
    const { t } = useTranslation();
    const waiting = calibrations.filter((c) => c.state === 'proposed');
    if (waiting.length === 0) {
        return null;
    }
    const newest = waiting[waiting.length - 1];
    return (
        <Notice tone="warn">
            <span data-awaiting-review>
                {t('marketdata.calibration.variance.waiting', {
                    count: String(waiting.length),
                    id: newest?.id.slice(0, 8) ?? '',
                    persistence: garchText(newest?.persistence),
                })}{' '}
                {to !== undefined && (
                    <Link className="font-medium underline" to={to}>
                        {t('marketdata.calibration.variance.reviewIt')}
                    </Link>
                )}
            </span>
        </Notice>
    );
}

/**
 * A fitted calibration's volatility over the input it was fitted from, read from the stored
 * points and the stored shape. The series must end at the stored starting ratio, so a series
 * that does not is said to disagree rather than drawn as if it matched.
 */
export function FittedVolatility({
    calibration,
}: {
    readonly calibration: StoredCalibration;
}): ReactNode {
    const { t } = useTranslation();
    const points = useInputPoints(calibration.input_id ?? '');
    const { alpha, gamma, beta, persistence, long_run_variance: longRun } = calibration;
    if (alpha == null || gamma == null || beta == null || persistence == null || longRun == null) {
        return null;
    }
    if (points.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const { days, endRatio } = conditionalVolatility(points.data, {
        alpha,
        gamma,
        beta,
        persistence,
        long_run_variance: longRun,
    });
    const stored = calibration.starting_variance_ratio;
    const agrees =
        stored != null && Math.abs(endRatio - stored) <= 1e-6 * Math.max(1, Math.abs(stored));
    return (
        <div className="space-y-2">
            <VolatilityChart days={days} longRun={longRun} />
            {!agrees && (
                <Notice tone="warn">{t('marketdata.calibration.variance.chart.disagrees')}</Notice>
            )}
        </div>
    );
}
