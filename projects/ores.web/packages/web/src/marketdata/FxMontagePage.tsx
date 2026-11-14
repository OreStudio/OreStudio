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
import { api, type FxMontagePair } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { formatDateTime, relativeTime } from '../ui/Time.js';
import { DATA_CELL, HEAD_CELL, asOfInstant, oneOf, todayIso, useReadParty } from './readScreen.js';

/**
 * One base currency's crosses, from
 * doc/prototypes/market_data/read-the-fx-montage/index.html.
 *
 * The base is named once and every row is a cross against it, whichever side of
 * the pair the base sits on. The instant a row's change is measured from is the
 * server's, stated once for the whole answer, so the screen does not decide what
 * "today" means.
 */

const BASES = ['EUR', 'USD', 'GBP', 'AUD', 'JPY', 'CHF', 'CAD', 'NZD'] as const;
const SORTS = ['pair', 'move'] as const;

type Sort = (typeof SORTS)[number];

function fixed(value: number, places: number, language: string): string {
    return value.toLocaleString(language, {
        minimumFractionDigits: places,
        maximumFractionDigits: places,
    });
}

function sorted(rows: readonly FxMontagePair[], sort: Sort): readonly FxMontagePair[] {
    if (sort === 'pair') return rows;
    return [...rows].sort((a, b) => (b.change_pct ?? -Infinity) - (a.change_pct ?? -Infinity));
}

function rowAge(pair: FxMontagePair, language: string): string {
    const newest = [...pair.spot_by_provider].sort((a, b) => a.age_seconds - b.age_seconds)[0];
    if (newest === undefined) return '—';
    const now = new Date();
    return relativeTime(new Date(now.getTime() - newest.age_seconds * 1000), now, language);
}

/**
 * The day's range as a bar with the consensus on it.
 *
 * The read states the low, the high and the consensus, and no path, so the bar
 * is what the data supports: where the pair has traded today, and where it
 * stands now.
 */
function RangeBar({
    low,
    high,
    last,
    up,
    label,
}: {
    readonly low: number | null;
    readonly high: number | null;
    readonly last: number;
    readonly up: boolean;
    readonly label: string;
}): ReactNode {
    if (low === null || high === null || !(high > low)) return <span>—</span>;
    const width = 120;
    const height = 12;
    const share = Math.min(1, Math.max(0, (last - low) / (high - low)));
    const at = 4 + share * (width - 8);
    return (
        <svg
            width={width}
            height={height}
            viewBox={`0 0 ${width} ${height}`}
            role="img"
            aria-label={label}
        >
            <line
                x1="4"
                y1={height / 2}
                x2={width - 4}
                y2={height / 2}
                stroke="currentColor"
                strokeWidth="1"
                opacity="0.35"
            />
            <circle
                cx={at}
                cy={height / 2}
                r="2.5"
                fill={up ? 'var(--color-up)' : 'var(--color-down)'}
            />
        </svg>
    );
}

export function FxMontagePage(): ReactNode {
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

    const base = (search.get('base') ?? 'EUR').toUpperCase();
    const asOf = search.get('asof') ?? todayIso();
    const sort = oneOf(search.get('sort'), SORTS, 'pair');
    const key = ['fx-montage', base, asOf] as const;

    const montage = useQuery({
        queryKey: key,
        queryFn: () => api.fxMontage({ baseCurrency: base, asOf: asOfInstant(asOf) }),
        retry: false,
    });

    const rows = montage.data === undefined ? [] : sorted(montage.data.pairs, sort);
    const places = 4;
    const head = HEAD_CELL;
    const cell = DATA_CELL;
    const providers = [
        ...new Set(rows.flatMap((pair) => pair.spot_by_provider.map((spot) => spot.provider_code))),
    ];

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.fxMontage.title')} />
            <div className="flex items-start justify-between gap-3">
                <PageHeader
                    title={t('marketdata.fxMontage.title')}
                    description={t('marketdata.fxMontage.lead')}
                />
                {party !== '' && (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.fxMontage.readingFor', { party })}
                    </p>
                )}
            </div>
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.fxMontage.base')}>
                    <Select
                        value={base}
                        onChange={(event) => {
                            set({ base: event.target.value });
                        }}
                    >
                        {BASES.map((code) => (
                            <option key={code}>{code}</option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.fxMontage.asOf')}>
                    <Input
                        type="date"
                        value={asOf}
                        onChange={(event) => {
                            if (event.target.value !== '') set({ asof: event.target.value });
                        }}
                    />
                </Field>
                <Field label={t('marketdata.fxMontage.sort')}>
                    <Select
                        value={sort}
                        onChange={(event) => {
                            set({ sort: event.target.value });
                        }}
                    >
                        {SORTS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.fxMontage.sorts.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                {montage.data !== undefined && (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.fxMontage.measuredFrom', {
                            at: formatDateTime(montage.data.change_since, language),
                        })}
                    </p>
                )}
            </section>
            {montage.isError && <Notice tone="error">{montage.error.message}</Notice>}
            {montage.isSuccess && rows.length === 0 && (
                <Notice tone="info">{t('marketdata.fxMontage.nothingQuoted')}</Notice>
            )}
            {rows.length > 0 && montage.data !== undefined && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-b border-line">
                                    <th className={cx(head, 'text-left')}>
                                        {t('marketdata.fxMontage.pair')}
                                    </th>
                                    {providers.map((provider) => (
                                        <th className={head} key={provider}>
                                            {provider}
                                        </th>
                                    ))}
                                    <th className={head}>{t('marketdata.fxMontage.last')}</th>
                                    <th className={head}>{t('marketdata.fxMontage.change')}</th>
                                    <th className={head}>{t('marketdata.fxMontage.low')}</th>
                                    <th className={head}>{t('marketdata.fxMontage.high')}</th>
                                    <th className={head}>{t('marketdata.fxMontage.age')}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((pair) => {
                                    const quoted = pair.spot_by_provider.length;
                                    return (
                                        <tr
                                            key={pair.pair_name}
                                            className="border-b border-line-subtle"
                                        >
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {pair.pair_name}
                                                {quoted === 0 && (
                                                    <Tag tone="muted" small>
                                                        {t('marketdata.fxMontage.unquoted')}
                                                    </Tag>
                                                )}
                                                <div className="text-ink-faint">
                                                    <RangeBar
                                                        low={pair.day_low}
                                                        high={pair.day_high}
                                                        last={pair.consensus}
                                                        up={(pair.change_pct ?? 0) >= 0}
                                                        label={t(
                                                            'marketdata.fxMontage.rangeLabel',
                                                            {
                                                                low: pair.day_low ?? '',
                                                                high: pair.day_high ?? '',
                                                                last: pair.consensus,
                                                            },
                                                        )}
                                                    />
                                                </div>
                                            </td>
                                            {providers.map((provider) => {
                                                const spot = pair.spot_by_provider.find(
                                                    (one) => one.provider_code === provider,
                                                );
                                                return (
                                                    <td
                                                        className={cx(
                                                            cell,
                                                            spot === undefined
                                                                ? 'text-ink-faint'
                                                                : undefined,
                                                        )}
                                                        key={provider}
                                                    >
                                                        {spot === undefined
                                                            ? '—'
                                                            : fixed(spot.value, places, language)}
                                                    </td>
                                                );
                                            })}
                                            <td className={cell}>
                                                {fixed(pair.consensus, places, language)}
                                            </td>
                                            <td
                                                className={cx(
                                                    cell,
                                                    pair.change_pct === null
                                                        ? 'text-ink-faint'
                                                        : pair.change_pct >= 0
                                                          ? 'text-up'
                                                          : 'text-down',
                                                )}
                                            >
                                                {pair.change_pct === null
                                                    ? '—'
                                                    : `${pair.change_pct >= 0 ? '+' : ''}${pair.change_pct.toFixed(2)}%`}
                                            </td>
                                            <td className={cell}>
                                                {pair.day_low === null
                                                    ? '—'
                                                    : fixed(pair.day_low, places, language)}
                                            </td>
                                            <td className={cell}>
                                                {pair.day_high === null
                                                    ? '—'
                                                    : fixed(pair.day_high, places, language)}
                                            </td>
                                            <td className={cx(cell, 'text-ink-muted')}>
                                                {rowAge(pair, language)}
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    </div>
                </section>
            )}
        </div>
    );
}
