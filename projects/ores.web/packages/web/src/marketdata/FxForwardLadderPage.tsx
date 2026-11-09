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
import { Link, useSearchParams } from 'react-router';
import { api, type FxForwardCurveSource, type FxForwardLadder } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { publishedCurvePath } from '../shell/paths.js';
import {
    Button,
    Fact,
    Field,
    Input,
    Notice,
    PageHeader,
    Select,
    Tag,
    cx,
} from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';
import { formatDateTime } from '../ui/Time.js';
import {
    LADDER_MEASURES,
    LADDER_STARTS,
    asOfInstant,
    daysBetween,
    forwardForwardPoints,
    forwardForwardTenors,
    rowsFromStart,
    todayIso,
    usdPairs,
    type LadderMeasure,
    type LadderRow,
    type LadderStart,
} from './fxForwardLadder.js';
import { LineChart, SERIES_COLOURS, type ChartSeries } from './LineChart.js';

/**
 * The FX forward ladder, from doc/prototypes/market_data/fx-forward-ladder.org.
 *
 * One read answers a pair at an as-of, and the rows hold every measure. The
 * start, the measure and the forward-forward change nothing on the server, so
 * they read nothing. The address holds them all.
 */

const LADDER_KEY = 'fx-forward-ladder';

function oneOf<T extends string>(value: string | null, allowed: readonly T[], fallback: T): T {
    return allowed.find((candidate) => candidate === value) ?? fallback;
}

function fixed(value: number, places: number, language: string): string {
    return value.toLocaleString(language, {
        minimumFractionDigits: places,
        maximumFractionDigits: places,
    });
}

function points(value: number, language: string): ReactNode {
    const text = fixed(value, 2, language);
    return (
        <span className={value < 0 ? 'text-down' : undefined}>{value > 0 ? `+${text}` : text}</span>
    );
}

function Segments<T extends string>({
    label,
    options,
    value,
    titleOf,
    onChoose,
}: {
    readonly label: string;
    readonly options: readonly T[];
    readonly value: T;
    readonly titleOf: (option: T) => string;
    readonly onChoose: (option: T) => void;
}): ReactNode {
    return (
        <div>
            <div className="mb-1 text-xs text-ink-faint">{label}</div>
            <div role="group" aria-label={label} className="inline-flex flex-wrap gap-1">
                {options.map((option) => (
                    <Button
                        key={option}
                        aria-pressed={option === value}
                        variant={option === value ? 'primary' : 'secondary'}
                        onClick={() => {
                            onChoose(option);
                        }}
                    >
                        {titleOf(option)}
                    </Button>
                ))}
            </div>
        </div>
    );
}

function Curve({ source }: { readonly source: FxForwardCurveSource }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <Fact label={t('marketdata.fxForwards.fundingCurve', { currency: source.currency })}>
            <Link className="text-accent" to={publishedCurvePath(source.curve_name)}>
                {source.curve_name}
            </Link>
            <div className="text-xs text-ink-faint">
                {t('marketdata.fxForwards.publishedSet', {
                    asOf: formatDateTime(source.as_of, language),
                    by: source.published_by,
                })}
            </div>
        </Fact>
    );
}

function Inputs({ ladder }: { readonly ladder: FxForwardLadder }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <section className="space-y-3 rounded-md border border-line p-4">
            <h2 className="text-sm font-semibold">{t('marketdata.fxForwards.builtFrom')}</h2>
            <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-5">
                <Fact label={t('marketdata.fxForwards.spot')}>
                    <span className="font-mono">
                        {fixed(ladder.spot, ladder.decimal_places + 1, language)}
                    </span>
                    <div className="text-xs text-ink-faint">
                        {t('marketdata.fxForwards.observedAt', {
                            at: formatDateTime(ladder.spot_observed_at, language),
                        })}
                    </div>
                </Fact>
                <Fact label={t('marketdata.fxForwards.spotDate')}>
                    <span className="font-mono">{ladder.spot_date}</span>
                    <div className="text-xs text-ink-faint">T+{ladder.spot_days}</div>
                </Fact>
                <Fact label={t('marketdata.fxForwards.pipFactor')}>
                    <span className="font-mono">{ladder.pip_factor}</span>
                </Fact>
                <Curve source={ladder.base_curve} />
                <Curve source={ladder.quote_curve} />
            </div>
            <Notice tone="info">{t('marketdata.fxForwards.noBasis')}</Notice>
        </section>
    );
}

function seriesOf(rows: readonly LadderRow[], measure: LadderMeasure): readonly ChartSeries[] {
    const line = (values: readonly number[], colour: string): ChartSeries => ({
        points: values,
        colour,
        dots: true,
    });
    const [first = '', second = ''] = SERIES_COLOURS;
    if (measure === 'discount') {
        return [
            line(
                rows.map((r) => r.base_discount),
                first,
            ),
            line(
                rows.map((r) => r.quote_discount),
                second,
            ),
        ];
    }
    const of = {
        outright: 'forward_rate',
        forward: 'forward_points',
        swap: 'swap_points',
    } as const;
    return [
        line(
            rows.map((r) => r[of[measure]]),
            first,
        ),
    ];
}

function Chart({
    ladder,
    rows,
    measure,
}: {
    readonly ladder: FxForwardLadder;
    readonly rows: readonly LadderRow[];
    readonly measure: LadderMeasure;
}): ReactNode {
    const { t, language } = useTranslation();
    const title = t(`marketdata.fxForwards.measure.${measure}`);
    const places =
        measure === 'outright' ? ladder.decimal_places + 1 : measure === 'discount' ? 4 : 0;
    return (
        <section className="space-y-2 rounded-md border border-line p-4">
            <h2 className="text-sm font-semibold">
                {t('marketdata.fxForwards.byTenor', { measure: title })}
            </h2>
            <LineChart
                series={seriesOf(rows, measure)}
                categories={rows.map((row) => row.tenor)}
                format={(value) => fixed(value, places, language)}
                label={t('marketdata.fxForwards.byTenor', { measure: title })}
                padding={0.08}
            />
            {measure === 'discount' && (
                <p className="text-xs text-ink-faint">
                    {ladder.base_curve.currency} · {ladder.quote_curve.currency}
                </p>
            )}
        </section>
    );
}

function Ladder({
    ladder,
    rows,
    measure,
    start,
}: {
    readonly ladder: FxForwardLadder;
    readonly rows: readonly LadderRow[];
    readonly measure: LadderMeasure;
    readonly start: LadderStart;
}): ReactNode {
    const { t, language } = useTranslation();
    const mark = (which: LadderMeasure): string | undefined =>
        which === measure ? 'bg-accent/10' : undefined;
    const head = 'px-2 py-1 text-right text-xs font-semibold text-ink-faint';
    const cell = 'px-2 py-1 text-right font-mono whitespace-nowrap';
    return (
        <section className="space-y-2 rounded-md border border-line p-4">
            <h2 className="text-sm font-semibold">{t('marketdata.fxForwards.ladder')}</h2>
            <div className="overflow-x-auto">
                <table className="w-full text-sm">
                    <thead>
                        <tr className="border-b border-line">
                            <th className={cx(head, 'text-left')}>
                                {t('marketdata.fxForwards.tenor')}
                            </th>
                            <th className={head}>{t('marketdata.fxForwards.nearDate')}</th>
                            <th className={head}>{t('marketdata.fxForwards.valueDate')}</th>
                            <th className={cx(head, mark('outright'))}>
                                {t('marketdata.fxForwards.measure.outright')}
                            </th>
                            <th className={cx(head, mark('forward'))}>
                                {t('marketdata.fxForwards.measure.forward')}
                            </th>
                            <th className={cx(head, mark('swap'))}>
                                {t('marketdata.fxForwards.measure.swap')}
                            </th>
                            <th className={cx(head, mark('discount'))}>
                                {t('marketdata.fxForwards.discountOf', {
                                    currency: ladder.base_curve.currency,
                                })}
                            </th>
                            <th className={cx(head, mark('discount'))}>
                                {t('marketdata.fxForwards.discountOf', {
                                    currency: ladder.quote_curve.currency,
                                })}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {rows.map((row) => (
                            <tr key={row.tenor} className="border-b border-line-subtle">
                                <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                    {row.tenor}{' '}
                                    {row.moved && (
                                        <Tag tone="accent" small>
                                            {t(`marketdata.fxForwards.from.${start}`)}
                                        </Tag>
                                    )}
                                </td>
                                <td className={cell}>{row.near_date}</td>
                                <td className={cell}>{row.value_date}</td>
                                <td className={cx(cell, mark('outright'))}>
                                    {fixed(row.forward_rate, ladder.decimal_places + 1, language)}
                                </td>
                                <td className={cx(cell, mark('forward'))}>
                                    {points(row.forward_points, language)}
                                </td>
                                <td className={cx(cell, mark('swap'))}>
                                    {points(row.swap_points, language)}
                                </td>
                                <td className={cx(cell, mark('discount'))}>
                                    {fixed(row.base_discount, 6, language)}
                                </td>
                                <td className={cx(cell, mark('discount'))}>
                                    {fixed(row.quote_discount, 6, language)}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <p className="text-xs text-ink-faint">{t('marketdata.fxForwards.ladderNote')}</p>
        </section>
    );
}

function ForwardForward({
    ladder,
    rows,
    near,
    far,
    onChoose,
}: {
    readonly ladder: FxForwardLadder;
    readonly rows: readonly LadderRow[];
    readonly near: string;
    readonly far: string;
    readonly onChoose: (which: 'near' | 'far', tenor: string) => void;
}): ReactNode {
    const { t, language } = useTranslation();
    const tenors = forwardForwardTenors(rows);
    const nearRow = tenors.find((row) => row.tenor === near) ?? tenors[0];
    const farRow = tenors.find((row) => row.tenor === far) ?? tenors[1];
    if (nearRow === undefined || farRow === undefined) return null;
    const swap = forwardForwardPoints(nearRow, farRow, ladder.pip_factor);
    const picker = (which: 'near' | 'far', value: string): ReactNode => (
        <Field label={t(`marketdata.fxForwards.${which}Tenor`)}>
            <Select
                value={value}
                onChange={(event) => {
                    onChoose(which, event.target.value);
                }}
            >
                {tenors.map((row) => (
                    <option key={row.tenor}>{row.tenor}</option>
                ))}
            </Select>
        </Field>
    );
    return (
        <section className="space-y-3 rounded-md border border-line p-4">
            <h2 className="text-sm font-semibold">{t('marketdata.fxForwards.forwardForward')}</h2>
            <div className="flex flex-wrap gap-3">
                {picker('near', nearRow.tenor)}
                {picker('far', farRow.tenor)}
            </div>
            {swap === undefined ? (
                <p className="text-sm text-ink-muted">{t('marketdata.fxForwards.farAfterNear')}</p>
            ) : (
                <div className="grid gap-4 sm:grid-cols-4">
                    <Fact label={t('marketdata.fxForwards.nearRate')}>
                        <span className="font-mono">
                            {fixed(nearRow.forward_rate, ladder.decimal_places + 1, language)}
                        </span>
                    </Fact>
                    <Fact label={t('marketdata.fxForwards.farRate')}>
                        <span className="font-mono">
                            {fixed(farRow.forward_rate, ladder.decimal_places + 1, language)}
                        </span>
                    </Fact>
                    <Fact
                        label={t('marketdata.fxForwards.swapBetween', {
                            near: nearRow.tenor,
                            far: farRow.tenor,
                        })}
                    >
                        <span className="font-mono">{points(swap, language)}</span>
                    </Fact>
                    <Fact label={t('marketdata.fxForwards.days')}>
                        <span className="font-mono">
                            {daysBetween(nearRow.value_date, farRow.value_date)}
                        </span>
                    </Fact>
                </div>
            )}
        </section>
    );
}

export function FxForwardLadderPage(): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const set = (values: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(search);
        Object.entries(values).forEach(([key, value]) => {
            next.set(key, value);
        });
        setSearch(next, { replace: true });
    };

    const pairs = useQuery({
        queryKey: ['currency-pairs', 'usd'],
        queryFn: async () =>
            usdPairs((await api.records('currency-pairs')).map((row) => String(row['pair_code']))),
    });
    const options = pairs.data ?? [];
    const pair =
        search.get('pair') ?? (options.includes('EUR/USD') ? 'EUR/USD' : (options[0] ?? ''));
    const asOf = search.get('asof') ?? todayIso();
    const start = oneOf(search.get('start'), LADDER_STARTS, 'spot');
    const measure = oneOf(search.get('measure'), LADDER_MEASURES, 'outright');

    const key = [LADDER_KEY, pair, asOf] as const;
    const ladder = useQuery({
        queryKey: key,
        queryFn: () => api.fxForwardLadder({ pairCode: pair, asOf: asOfInstant(asOf) }),
        enabled: pair !== '',
        retry: false,
    });
    const rows = ladder.data === undefined ? [] : rowsFromStart(ladder.data, start);

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.fxForwards.title')} />
            <div className="flex items-start justify-between gap-3">
                <PageHeader
                    title={t('marketdata.fxForwards.title')}
                    description={t('marketdata.fxForwards.lead')}
                />
                <RefreshQueries keys={[key]} />
            </div>
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.fxForwards.pair')}>
                    <Select
                        value={pair}
                        onChange={(event) => {
                            set({ pair: event.target.value });
                        }}
                    >
                        {options.map((option) => (
                            <option key={option}>{option}</option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.fxForwards.asOf')}>
                    <Input
                        type="date"
                        value={asOf}
                        onChange={(event) => {
                            if (event.target.value !== '') set({ asof: event.target.value });
                        }}
                    />
                </Field>
                <Segments
                    label={t('marketdata.fxForwards.startsFrom')}
                    options={LADDER_STARTS}
                    value={start}
                    titleOf={(option) => t(`marketdata.fxForwards.start.${option}`)}
                    onChoose={(option) => {
                        set({ start: option });
                    }}
                />
                <Segments
                    label={t('marketdata.fxForwards.chart')}
                    options={LADDER_MEASURES}
                    value={measure}
                    titleOf={(option) => t(`marketdata.fxForwards.measure.${option}`)}
                    onChoose={(option) => {
                        set({ measure: option });
                    }}
                />
            </section>
            {ladder.isError && <Notice tone="error">{ladder.error.message}</Notice>}
            {pairs.isSuccess && options.length === 0 && (
                <Notice tone="info">{t('marketdata.fxForwards.noPairs')}</Notice>
            )}
            {ladder.data !== undefined && (
                <>
                    <Inputs ladder={ladder.data} />
                    <Chart ladder={ladder.data} rows={rows} measure={measure} />
                    <Ladder ladder={ladder.data} rows={rows} measure={measure} start={start} />
                    <ForwardForward
                        ladder={ladder.data}
                        rows={rows}
                        near={search.get('near') ?? '3M'}
                        far={search.get('far') ?? '6M'}
                        onChoose={(which, tenor) => {
                            set({ [which]: tenor });
                        }}
                    />
                </>
            )}
        </div>
    );
}
