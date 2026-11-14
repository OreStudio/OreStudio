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
import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import { api, type CurveAnalytics } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Fact, Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { LineChart, SERIES_COLOURS, type ChartSeries } from './LineChart.js';
import { HEAD_CELL, DATA_CELL, asOfInstant, oneOf, todayIso, useReadParty } from './readScreen.js';

/**
 * One curve's analytics, from
 * doc/prototypes/market_data/inspect-a-curves-analytics/index.html.
 *
 * A market pillar and a bootstrapped point for the same tenor are a pair: the
 * left table says what the market quoted and the right says what the bootstrap
 * made of it. The zero rate is continuously compounded, the implied forward is
 * the rate the segment ending at the pillar implies, and the shift is the zero
 * rate less the market rate, not a parallel shift somebody applied.
 */

const CONFIGS = 'ir-curve-bootstrap-configs';

/** What the chart draws: the curve's own numbers, one metric at a time, as the prototype does. */
const METRICS = ['zero', 'forward', 'discount'] as const;

/// What the chart's x axis counts: the tenor code, or the days to maturity.
const AXES = ['tenor', 'maturity'] as const;

type Metric = (typeof METRICS)[number];
type ChartAxis = (typeof AXES)[number];

function percent(value: number, language: string): string {
    return (value * 100).toLocaleString(language, {
        minimumFractionDigits: 4,
        maximumFractionDigits: 4,
    });
}

function basisPoints(value: number): string {
    return `${value >= 0 ? '+' : ''}${value.toFixed(1)}`;
}

export function CurveAnalyticsPage(): ReactNode {
    const { t, language } = useTranslation();
    const party = useReadParty();
    const [search, setSearch] = useSearchParams();
    const set = (values: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(search);
        Object.entries(values).forEach(([key, value]) => {
            next.set(key, value);
        });
        setSearch(next, { replace: true });
    };

    const configs = useQuery({
        queryKey: ['records', CONFIGS],
        queryFn: async () => api.records(CONFIGS),
        retry: false,
    });
    const options = (configs.data ?? []).map((row) => ({
        id: String(row['id']),
        name: String(row['name']),
    }));
    const stated = search.get('curve') ?? '';
    const chosen =
        options.find((option) => option.id === stated || option.name === stated) ?? options[0];
    const asOf = search.get('asof') ?? todayIso();
    const metric = oneOf(search.get('m'), METRICS, 'zero');
    const axis = oneOf(search.get('axis'), AXES, 'tenor');
    const compare = search.get('compare') ?? '';

    const key = ['curve-analytics', chosen?.id ?? '', asOf, compare] as const;
    const analytics = useQuery({
        queryKey: key,
        queryFn: () =>
            api.curveAnalytics({
                instrumentId: chosen?.id ?? '',
                asOf: asOfInstant(asOf),
                compareAt: compare === '' ? '' : asOfInstant(compare),
            }),
        enabled: chosen !== undefined,
        retry: false,
    });

    const data: CurveAnalytics | undefined = analytics.data;
    const market = data?.market_pillars ?? [];
    const bootstrapped = data?.bootstrapped ?? [];
    const head = HEAD_CELL;
    const cell = DATA_CELL;

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.curveAnalytics.title')} />
            <div className="flex items-start justify-between gap-3">
                <PageHeader
                    title={t('marketdata.curveAnalytics.title')}
                    description={t('marketdata.curveAnalytics.lead')}
                />
                {party !== '' && (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.curveAnalytics.readingFor', { party })}
                    </p>
                )}
            </div>
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.curveAnalytics.curve')}>
                    <Select
                        value={chosen?.id ?? ''}
                        onChange={(event) => {
                            set({ curve: event.target.value });
                        }}
                    >
                        {options.map((option) => (
                            <option key={option.id} value={option.id}>
                                {option.name}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.curveAnalytics.compare')}>
                    <Input
                        type="date"
                        value={compare}
                        onChange={(event) => {
                            set({ compare: event.target.value });
                        }}
                    />
                </Field>
                <Field label={t('marketdata.curveAnalytics.asOf')}>
                    <Input
                        type="date"
                        value={asOf}
                        onChange={(event) => {
                            if (event.target.value !== '') set({ asof: event.target.value });
                        }}
                    />
                </Field>
                {data !== undefined && (
                    <>
                        <Fact label={t('marketdata.curveAnalytics.index')}>
                            {data.conventions.index_name}
                        </Fact>
                        <Fact label={t('marketdata.curveAnalytics.dayCount')}>
                            {data.conventions.day_count}
                        </Fact>
                        <Fact label={t('marketdata.curveAnalytics.compounding')}>
                            {data.conventions.compounding}
                        </Fact>
                        <Fact label={t('marketdata.curveAnalytics.interpolation')}>
                            {data.conventions.interpolation}
                        </Fact>
                        <Fact label={t('marketdata.curveAnalytics.calendar')}>
                            {data.conventions.calendar}
                        </Fact>
                        <Fact label={t('marketdata.curveAnalytics.pillars')}>
                            {t('marketdata.curveAnalytics.pillarCount', {
                                count: data.conventions.pillar_count,
                            })}
                        </Fact>
                    </>
                )}
            </section>
            {configs.isSuccess && options.length === 0 && (
                <Notice tone="info">{t('marketdata.curveAnalytics.noCurves')}</Notice>
            )}
            {analytics.isError && <Notice tone="error">{analytics.error.message}</Notice>}
            {data !== undefined && compare !== '' && data.compare_at === '' && (
                <Notice tone="info">{t('marketdata.curveAnalytics.noPriorSet')}</Notice>
            )}
            {data !== undefined && data.stale && (
                <Notice tone="warn">{t('marketdata.curveAnalytics.stale')}</Notice>
            )}
            {data !== undefined && data.bootstrapped.length >= 2 && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <div className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-sm font-semibold">
                            {t('marketdata.curveAnalytics.chart')}
                        </h2>
                        <div className="flex items-end gap-2">
                            <Select
                                value={metric}
                                onChange={(event) => {
                                    set({ m: event.target.value });
                                }}
                            >
                                {METRICS.map((option) => (
                                    <option key={option} value={option}>
                                        {t(`marketdata.curveAnalytics.metrics.${option}`)}
                                    </option>
                                ))}
                            </Select>
                            <Select
                                value={axis}
                                onChange={(event) => {
                                    set({ axis: event.target.value });
                                }}
                            >
                                {AXES.map((option) => (
                                    <option key={option} value={option}>
                                        {t(`marketdata.curveAnalytics.axes.${option}`)}
                                    </option>
                                ))}
                            </Select>
                        </div>
                    </div>
                    <p className="text-xs text-ink-faint">
                        {t(`marketdata.curveAnalytics.chartLegend.${metric}`, {
                            dayCount: data.conventions.day_count,
                        })}
                    </p>
                    <LineChart
                        series={chartSeries(data, metric)}
                        categories={data.bootstrapped.map((point) =>
                            axis === 'tenor' ? point.tenor : point.maturity_date,
                        )}
                        format={(value) =>
                            metric === 'discount' ? value.toFixed(4) : `${value.toFixed(3)}%`
                        }
                        label={t(`marketdata.curveAnalytics.metrics.${metric}`)}
                        padding={0.08}
                    />
                </section>
            )}
            {data !== undefined && (
                <>
                    <section className="space-y-2 rounded-md border border-line p-4">
                        <h2 className="text-sm font-semibold">
                            {t('marketdata.curveAnalytics.marketPillars')}
                        </h2>
                        <div className="overflow-x-auto">
                            <table className="w-full text-sm">
                                <thead>
                                    <tr className="border-b border-line">
                                        <th className={cx(head, 'text-left')}>
                                            {t('marketdata.curveAnalytics.tenor')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.maturity')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.tau')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.marketRate')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.provider')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {market.map((pillar) => (
                                        <tr
                                            key={`${pillar.tenor}-${pillar.maturity_date}`}
                                            className="border-b border-line-subtle"
                                        >
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {pillar.tenor}
                                            </td>
                                            <td className={cell}>{pillar.maturity_date}</td>
                                            <td className={cell}>
                                                {pillar.year_fraction.toFixed(4)}
                                            </td>
                                            <td className={cell}>
                                                {percent(pillar.rate, language)}
                                            </td>
                                            <td className={cell}>{pillar.provider_code}</td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </section>
                    <section className="space-y-2 rounded-md border border-line p-4">
                        <h2 className="text-sm font-semibold">
                            {t('marketdata.curveAnalytics.bootstrapped')}
                        </h2>
                        <div className="overflow-x-auto">
                            <table className="w-full text-sm">
                                <thead>
                                    <tr className="border-b border-line">
                                        <th className={cx(head, 'text-left')}>
                                            {t('marketdata.curveAnalytics.tenor')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.maturity')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.tau')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.zeroRate')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.impliedForward')}
                                        </th>
                                        {data.compare_at !== '' && (
                                            <>
                                                <th className={head}>
                                                    {t('marketdata.curveAnalytics.priorZero')}
                                                </th>
                                                <th className={head}>
                                                    {t('marketdata.curveAnalytics.change')}
                                                </th>
                                            </>
                                        )}
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.discountFactor')}
                                        </th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.shift')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {bootstrapped.map((point) => (
                                        <tr
                                            key={`${point.tenor}-${point.maturity_date}`}
                                            className="border-b border-line-subtle"
                                        >
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {point.tenor}
                                            </td>
                                            <td className={cell}>{point.maturity_date}</td>
                                            <td className={cell}>
                                                {point.year_fraction.toFixed(4)}
                                            </td>
                                            <td className={cell}>
                                                {percent(point.zero_rate, language)}
                                            </td>
                                            <td className={cell}>
                                                {percent(point.implied_forward, language)}
                                            </td>
                                            {data.compare_at !== '' && (
                                                <>
                                                    <td className={cell}>
                                                        {point.prior_zero_rate === null
                                                            ? '—'
                                                            : percent(
                                                                  point.prior_zero_rate,
                                                                  language,
                                                              )}
                                                    </td>
                                                    <td className={cell}>
                                                        {point.change_bps === null
                                                            ? '—'
                                                            : basisPoints(point.change_bps)}
                                                    </td>
                                                </>
                                            )}
                                            <td className={cell}>
                                                {point.discount_factor.toFixed(6)}
                                            </td>
                                            <td className={cell}>
                                                <Tag
                                                    tone={point.shift_bps >= 0 ? 'up' : 'down'}
                                                    small
                                                >
                                                    {basisPoints(point.shift_bps)}
                                                </Tag>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </section>
                    {data.compare_at !== '' && (
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.curveAnalytics.comparedWith', {
                                at: data.compare_at,
                            })}
                        </p>
                    )}
                    <Notice tone="info">{t('marketdata.curveAnalytics.shiftNote')}</Notice>
                    {data.compare_at === '' && (
                        <Notice tone="info">{t('marketdata.curveAnalytics.noPriorClose')}</Notice>
                    )}
                </>
            )}
        </div>
    );
}

/**
 * The metric's own numbers, and for a rate the market's beside them.
 *
 * The market overlay is dashed because it is a different kind of fact: the
 * curve is what the bootstrap made of the quotes, and the overlay is what the
 * quotes said. A discount, factor by factor, has no market counterpart to draw.
 */
function chartSeries(data: CurveAnalytics, metric: Metric): readonly ChartSeries[] {
    const percent = (value: number): number => value * 100;
    const curve = data.bootstrapped.map((point) =>
        metric === 'zero'
            ? percent(point.zero_rate)
            : metric === 'forward'
              ? percent(point.implied_forward)
              : point.discount_factor,
    );
    const series: ChartSeries[] = [
        { points: curve, colour: SERIES_COLOURS[0], fill: metric !== 'discount', strong: true },
    ];
    if (metric !== 'discount') {
        // The market's own quote at each pillar, matched by tenor so the two lines
        // share an x. A pillar the market did not quote is a gap, not a zero.
        const market = new Map(
            data.market_pillars.map((pillar) => [pillar.tenor, percent(pillar.rate)]),
        );
        series.push({
            points: data.bootstrapped.map((point) => market.get(point.tenor) ?? null),
            colour: SERIES_COLOURS[2],
            dashed: true,
            dots: true,
        });
    }
    return series;
}
