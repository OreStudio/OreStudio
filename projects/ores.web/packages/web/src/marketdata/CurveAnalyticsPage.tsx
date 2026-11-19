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
import { show } from '../refdata/records.js';
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

/** What the address says when the reader has asked for no curve beside this one. */
const NO_SECOND_CURVE = 'none';

/** What the chart draws: the curve's own numbers, one metric at a time, as the prototype does. */
const METRICS = ['zero', 'forward', 'discount'] as const;

/// What the chart's x axis counts: the tenor code, or the days to maturity.
const AXES = ['tenor', 'maturity'] as const;

/**
 * Which of the two curves the chart draws. One at a time is the prototype's own
 * offer: two curves that sit far apart on one axis can hide the shape of either.
 */
const VIEWS = ['both', 'curve', 'beside'] as const;

type Metric = (typeof METRICS)[number];
type ChartAxis = (typeof AXES)[number];
type PairView = (typeof VIEWS)[number];

/**
 * One curve configuration, as the picker and the pair both need it.
 *
 * A name alone does not say what a curve is: USD-SOFR-FOMC and USD-LIBOR-1M are
 * two names in a list until the role and the index tenor beside them say which
 * one is the overnight and which one is a tenor of LIBOR projected off it.
 */
interface CurveOption {
    readonly id: string;
    readonly name: string;
    readonly role: string;
    readonly indexTenor: string;
    /** The funding curve a projection names as the one it discounts off. */
    readonly discountId: string;
}

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
    const set = (values: Readonly<Record<string, string | undefined>>): void => {
        const next = new URLSearchParams(search);
        Object.entries(values).forEach(([key, value]) => {
            if (value === undefined) next.delete(key);
            else next.set(key, value);
        });
        setSearch(next, { replace: true });
    };

    const configs = useQuery({
        queryKey: ['records', CONFIGS],
        queryFn: async () => api.records(CONFIGS),
        retry: false,
    });
    const options: readonly CurveOption[] = (configs.data ?? []).map((row) => ({
        id: show(row['id']),
        name: show(row['name']),
        role: show(row['curve_family_role']),
        indexTenor: show(row['index_tenor']),
        discountId: show(row['discount_curve_config_id']),
    }));
    const labelOf = (option: CurveOption): string => {
        const role = option.role === '' ? '' : t(`marketdata.curveBlueprints.roles.${option.role}`);
        return [option.name, role, option.indexTenor].filter((part) => part !== '').join(' · ');
    };
    const stated = search.get('curve') ?? '';
    const chosen =
        options.find((option) => option.id === stated || option.name === stated) ?? options[0];

    /**
     * The curve drawn beside the chosen one: for a projection, the funding curve
     * it names as the one it discounts off, which is the currency's overnight
     * curve. Anything else starts with nothing beside it.
     */
    const fundingOf = (curve: CurveOption | undefined): CurveOption | undefined =>
        curve === undefined || curve.discountId === ''
            ? undefined
            : options.find((option) => option.id === curve.discountId);
    const statedAgainst = search.get('against');
    const namedAgainst =
        statedAgainst === null
            ? fundingOf(chosen)
            : statedAgainst === NO_SECOND_CURVE
              ? undefined
              : options.find(
                    (option) => option.id === statedAgainst || option.name === statedAgainst,
                );
    /** A curve is not drawn beside itself. */
    const beside = namedAgainst === chosen ? undefined : namedAgainst;

    const asOf = search.get('asof') ?? todayIso();
    const metric = oneOf(search.get('m'), METRICS, beside === undefined ? 'zero' : 'forward');
    const axis = oneOf(search.get('axis'), AXES, 'tenor');
    const compare = search.get('compare') ?? '';
    const view = oneOf(search.get('view'), VIEWS, 'both');

    const key = ['curve-analytics', chosen?.id ?? '', asOf, compare] as const;
    const analytics = useQuery({
        queryKey: key,
        queryFn: () =>
            api.curveAnalytics({
                instrumentId: chosen?.id ?? '',
                asOf: asOfInstant(asOf),
                compareAt: compare === '' ? null : asOfInstant(compare),
            }),
        enabled: chosen !== undefined,
        retry: false,
    });
    const besideAnalytics = useQuery({
        // The empty fourth slot is "no prior set": the second curve is read at
        // one instant, since a pair of changes over time is not this screen.
        queryKey: ['curve-analytics', beside?.id ?? '', asOf, ''] as const,
        queryFn: () =>
            api.curveAnalytics({
                instrumentId: beside?.id ?? '',
                asOf: asOfInstant(asOf),
                compareAt: null,
            }),
        enabled: beside !== undefined,
        retry: false,
    });

    const data: CurveAnalytics | undefined = analytics.data;
    const besideData: CurveAnalytics | undefined = besideAnalytics.data;
    const market = data?.market_pillars ?? [];
    const bootstrapped = data?.bootstrapped ?? [];
    const labels = data === undefined ? [] : pairLabels(data, besideData, axis);
    const showing = data === undefined ? [] : alignedCurve(data, metric, axis, labels);
    const sideBySide =
        besideData === undefined ? [] : alignedCurve(besideData, metric, axis, labels);
    const quoted = data === undefined ? [] : marketCurve(data, axis, labels);

    /**
     * The curve that discounts leads the pair, so the difference reads the same
     * way whichever curve the reader opened: what the term curve adds to the
     * overnight it is projected off, not a sign that depends on the order of two
     * selections.
     */
    const discounts = (option: CurveOption | undefined): boolean => option?.role === 'FUNDING';
    const fundingLeads = data !== undefined && discounts(beside) && !discounts(chosen);
    const leftValues = fundingLeads ? sideBySide : showing;
    const rightValues = fundingLeads ? showing : sideBySide;
    const leftName = (fundingLeads ? besideData?.name : data?.name) ?? '';
    const rightName = (fundingLeads ? data?.name : besideData?.name) ?? '';
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
                            // The pair belongs to the curve, so a new curve starts from
                            // its own funding curve rather than carrying the last one.
                            set({ curve: event.target.value, against: undefined });
                        }}
                    >
                        {options.map((option) => (
                            <option key={option.id} value={option.id}>
                                {labelOf(option)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.curveAnalytics.besideCurve')}>
                    <Select
                        value={beside?.id ?? NO_SECOND_CURVE}
                        onChange={(event) => {
                            set({ against: event.target.value });
                        }}
                    >
                        <option value={NO_SECOND_CURVE}>
                            {t('marketdata.curveAnalytics.compareNone')}
                        </option>
                        {options
                            .filter((option) => option.id !== chosen?.id)
                            .map((option) => (
                                <option key={option.id} value={option.id}>
                                    {labelOf(option)}
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
            {data !== undefined && compare !== '' && data.compare_at === null && (
                <Notice tone="info">{t('marketdata.curveAnalytics.noPriorSet')}</Notice>
            )}
            {besideAnalytics.isError && (
                <Notice tone="error">
                    {t('marketdata.curveAnalytics.besideFailed', {
                        curve: beside?.name ?? '',
                        reason: besideAnalytics.error.message,
                    })}
                </Notice>
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
                            {besideData !== undefined && (
                                <Select
                                    value={view}
                                    onChange={(event) => {
                                        set({ view: event.target.value });
                                    }}
                                >
                                    {VIEWS.map((option) => (
                                        <option key={option} value={option}>
                                            {t(`marketdata.curveAnalytics.views.${option}`, {
                                                curve: data.name,
                                                beside: besideData.name,
                                            })}
                                        </option>
                                    ))}
                                </Select>
                            )}
                        </div>
                    </div>
                    <p className="text-xs text-ink-faint">
                        {besideData === undefined
                            ? t(`marketdata.curveAnalytics.chartLegend.${metric}`, {
                                  dayCount: data.conventions.day_count,
                              })
                            : t('marketdata.curveAnalytics.pairLegend', {
                                  curve: data.name,
                                  beside: besideData.name,
                              })}
                    </p>
                    <LineChart
                        series={chartSeries(showing, sideBySide, quoted, metric, view)}
                        categories={labels}
                        format={(value) =>
                            metric === 'discount' ? value.toFixed(4) : `${value.toFixed(3)}%`
                        }
                        label={t(`marketdata.curveAnalytics.metrics.${metric}`)}
                        padding={0.08}
                    />
                </section>
            )}
            {data !== undefined &&
                besideData !== undefined &&
                besideData.bootstrapped.length > 0 && (
                    <section className="space-y-2 rounded-md border border-line p-4">
                        <div className="flex flex-wrap items-baseline justify-between gap-2">
                            <h2 className="text-sm font-semibold">
                                {t('marketdata.curveAnalytics.pairTable')}
                            </h2>
                            <p className="text-xs text-ink-faint">
                                {t(`marketdata.curveAnalytics.metrics.${metric}`)}
                            </p>
                        </div>
                        <div className="overflow-x-auto">
                            <table className="w-full text-sm">
                                <thead>
                                    <tr className="border-b border-line">
                                        <th className={cx(head, 'text-left')}>
                                            {t('marketdata.curveAnalytics.tenor')}
                                        </th>
                                        <th className={head}>{leftName}</th>
                                        <th className={head}>{rightName}</th>
                                        <th className={head}>
                                            {t('marketdata.curveAnalytics.difference')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {labels.map((label, at) => (
                                        <tr key={label} className="border-b border-line-subtle">
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {label}
                                            </td>
                                            <td className={cell}>
                                                {written(metric, leftValues[at] ?? null, language)}
                                            </td>
                                            <td className={cell}>
                                                {written(metric, rightValues[at] ?? null, language)}
                                            </td>
                                            <td className={cell}>
                                                {gapOf(
                                                    metric,
                                                    leftValues[at] ?? null,
                                                    rightValues[at] ?? null,
                                                )}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
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
                                        {data.compare_at !== null && (
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
                                            {data.compare_at !== null && (
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
                    {data.compare_at !== null && (
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.curveAnalytics.comparedWith', {
                                at: data.compare_at,
                            })}
                        </p>
                    )}
                    <Notice tone="info">{t('marketdata.curveAnalytics.shiftNote')}</Notice>
                    {data.compare_at === null && (
                        <Notice tone="info">{t('marketdata.curveAnalytics.noPriorClose')}</Notice>
                    )}
                </>
            )}
            <WhatThePairTeaches pair={pairOf(chosen, beside)} />
        </div>
    );
}

/** A funding curve and a projection curve drawn together, or undefined for any other pair. */
export function pairOf(
    chosen: CurveOption | undefined,
    beside: CurveOption | undefined,
): { readonly funding: CurveOption; readonly projection: CurveOption } | undefined {
    if (chosen === undefined || beside === undefined) return undefined;
    if (chosen.role === 'FUNDING' && beside.role === 'PROJECTION') {
        return { funding: chosen, projection: beside };
    }
    if (chosen.role === 'PROJECTION' && beside.role === 'FUNDING') {
        return { funding: beside, projection: chosen };
    }
    return undefined;
}

/**
 * What reading an overnight curve beside a term curve teaches: the two answer
 * different questions, so the gap between them is not only expectation. Drawn
 * only when the pair on screen is a funding curve and a projection curve.
 */
function WhatThePairTeaches({ pair }: { readonly pair: ReturnType<typeof pairOf> }): ReactNode {
    const { t } = useTranslation();
    if (pair === undefined) return null;
    const names = {
        funding: pair.funding.name,
        projection: pair.projection.name,
        tenor: pair.projection.indexTenor,
    };
    const points = ['underlying', 'fixing', 'use', 'shape'] as const;
    return (
        <section
            className="space-y-3 rounded-md border border-line p-4"
            aria-label={t('marketdata.curveAnalytics.teaches.title')}
            data-pair-teaches
        >
            <h2 className="font-medium">{t('marketdata.curveAnalytics.teaches.title')}</h2>
            <p className="text-sm text-ink-muted">
                {t('marketdata.curveAnalytics.teaches.lead', names)}
            </p>
            <dl className="grid gap-3 text-sm md:grid-cols-2">
                {points.map((point) => (
                    <div key={point}>
                        <dt className="font-medium">
                            {t(`marketdata.curveAnalytics.teaches.${point}.title`)}
                        </dt>
                        <dd className="text-ink-muted">
                            {t(`marketdata.curveAnalytics.teaches.${point}.body`, names)}
                        </dd>
                    </div>
                ))}
            </dl>
            <p className="text-sm text-ink-muted">
                {t('marketdata.curveAnalytics.teaches.lesson')}
            </p>
        </section>
    );
}

/**
 * The x positions the chart draws: every tenor either curve holds, in maturity
 * order and once each. A tenor only one curve has stays a gap in the other, so
 * the two lines share one axis rather than being drawn over each other's labels.
 */
function pairLabels(
    data: CurveAnalytics,
    beside: CurveAnalytics | undefined,
    axis: ChartAxis,
): readonly string[] {
    const first = new Map<string, number>();
    for (const point of [...data.bootstrapped, ...(beside?.bootstrapped ?? [])]) {
        const label = axis === 'tenor' ? point.tenor : point.maturity_date;
        const seen = first.get(label);
        if (seen === undefined || point.year_fraction < seen) first.set(label, point.year_fraction);
    }
    return [...first.entries()].sort((one, other) => one[1] - other[1]).map(([label]) => label);
}

/** One curve's numbers, one for each x, with null where the curve holds no point. */
function alignedCurve(
    data: CurveAnalytics,
    metric: Metric,
    axis: ChartAxis,
    labels: readonly string[],
): readonly (number | null)[] {
    const at = new Map(
        data.bootstrapped.map((point) => [
            axis === 'tenor' ? point.tenor : point.maturity_date,
            metric === 'zero'
                ? point.zero_rate
                : metric === 'forward'
                  ? point.implied_forward
                  : point.discount_factor,
        ]),
    );
    return labels.map((label) => at.get(label) ?? null);
}

/**
 * What the market quoted at each pillar, matched to the same x as the curve.
 *
 * The quote is per tenor, so a point the market did not quote is a gap rather
 * than a fallback to the curve's own number.
 */
function marketCurve(
    data: CurveAnalytics,
    axis: ChartAxis,
    labels: readonly string[],
): readonly (number | null)[] {
    const quote = new Map(data.market_pillars.map((pillar) => [pillar.tenor, pillar.rate]));
    const at = new Map(
        data.bootstrapped.map((point) => [
            axis === 'tenor' ? point.tenor : point.maturity_date,
            quote.get(point.tenor) ?? null,
        ]),
    );
    return labels.map((label) => at.get(label) ?? null);
}

/**
 * The series the chart draws: the curve, then either the curve beside it or the
 * market it was bootstrapped from.
 *
 * The second curve is dashed and dotted for the same reason the market overlay
 * always was: it is a different kind of fact. A chart of two curves leaves the
 * quotes out, because three lines say nothing clearly.
 */
function chartSeries(
    showing: readonly (number | null)[],
    sideBySide: readonly (number | null)[],
    quoted: readonly (number | null)[],
    metric: Metric,
    view: PairView,
): readonly ChartSeries[] {
    const scale = (points: readonly (number | null)[]): (number | null)[] =>
        points.map((value) => (value === null || metric === 'discount' ? value : value * 100));
    const series: ChartSeries[] = [
        {
            points: scale(showing),
            colour: SERIES_COLOURS[0],
            fill: metric !== 'discount',
            strong: true,
        },
    ];
    if (sideBySide.length > 0) {
        const beside: ChartSeries = {
            points: scale(sideBySide),
            colour: SERIES_COLOURS[1],
            dashed: true,
            dots: true,
        };
        if (view === 'curve') return series;
        if (view === 'beside') {
            return [{ ...beside, dashed: false, strong: true, fill: metric !== 'discount' }];
        }
        series.push(beside);
        return series;
    }
    if (metric !== 'discount') {
        series.push({
            points: scale(quoted),
            colour: SERIES_COLOURS[2],
            dashed: true,
            dots: true,
        });
    }
    return series;
}

/** One curve's number at one pillar, written the way the metric reads. */
function written(metric: Metric, value: number | null, language: string): string {
    if (value === null) return '—';
    return metric === 'discount' ? value.toFixed(6) : percent(value, language);
}

/** The two curves' difference at one pillar, in the metric's own unit. */
function gapOf(metric: Metric, left: number | null, right: number | null): string {
    if (left === null || right === null) return '—';
    const gap = right - left;
    return metric === 'discount'
        ? `${gap >= 0 ? '+' : ''}${gap.toFixed(6)}`
        : basisPoints(gap * 10000);
}
