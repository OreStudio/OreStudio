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
import { api, type RateLadderRung } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Fact, Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { relativeTime } from '../ui/Time.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';

/**
 * One currency's rate ladder, from
 * doc/prototypes/market_data/read-a-rate-ladder/index.html.
 *
 * The rungs, their maturity dates, their year fractions and the consensus are
 * the server's, because the calendar, the day count and the tenor convention are
 * server facts. The screen adds only what the prototype asked for and no read
 * states: the best quote of a rung and its distance in basis points from the
 * consensus.
 */

const CURRENCIES = ['USD', 'EUR', 'GBP', 'JPY', 'CHF', 'AUD', 'CAD', 'NZD'] as const;

/// The forward start a ladder is quoted from: spot, or a broken date.
const STARTS = ['0D', '3M', '1Y'] as const;

/// How much of the curve the ladder shows, by the year fraction the server states.
const WINDOWS = ['short', 'all', 'long'] as const;

type Window = (typeof WINDOWS)[number];

function oneOf<T extends string>(value: string | null, allowed: readonly T[], fallback: T): T {
    return allowed.find((candidate) => candidate === value) ?? fallback;
}

/** Whether a rung falls in the window, by the year fraction the server dated it with. */
function inWindow(rung: RateLadderRung, window: Window): boolean {
    if (window === 'short') return rung.year_fraction < 1.0 / 12.0;
    if (window === 'long') return rung.year_fraction >= 1.0 && rung.year_fraction <= 10.0;
    return true;
}

function asOfInstant(date: string): string {
    return `${date} 23:59:59Z`;
}

function todayIso(): string {
    return new Date().toISOString().slice(0, 10);
}

function percent(value: number, language: string): string {
    return (value * 100).toLocaleString(language, {
        minimumFractionDigits: 4,
        maximumFractionDigits: 4,
    });
}

function distance(best: number | undefined, consensus: number): string {
    if (best === undefined) return '—';
    const bp = (best - consensus) * 10000;
    return `${bp >= 0 ? '+' : ''}${bp.toFixed(1)} bp`;
}

function ageOf(rung: RateLadderRung, language: string): string {
    const newest = [...rung.quotes].sort((a, b) => a.age_seconds - b.age_seconds)[0];
    if (newest === undefined) return '—';
    const now = new Date();
    return relativeTime(new Date(now.getTime() - newest.age_seconds * 1000), now, language);
}

export function RateLadderPage(): ReactNode {
    const { t, language } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const set = (values: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(search);
        Object.entries(values).forEach(([key, value]) => {
            next.set(key, value);
        });
        setSearch(next, { replace: true });
    };

    const currency = (search.get('ccy') ?? 'USD').toUpperCase();
    const asOf = search.get('asof') ?? todayIso();
    const providers = search.get('providers') ?? '';
    const start = oneOf(search.get('start'), STARTS, '0D');
    const window = oneOf(search.get('window'), WINDOWS, 'all');
    const key = ['rate-ladder', currency, asOf, providers, start] as const;

    const ladder = useQuery({
        queryKey: key,
        queryFn: () =>
            api.rateLadder({
                currency,
                asOf: asOfInstant(asOf),
                providers: providers === '' ? [] : providers.split(','),
                forwardStart: start,
            }),
        retry: false,
    });

    const rows = (ladder.data?.rungs ?? []).filter((rung) => inWindow(rung, window));
    // One column per provider, in the order the server lists them, so a gap on a
    // rung is a gap in a column a person can see.
    const quoted = [
        ...new Set(
            (ladder.data?.rungs ?? []).flatMap((rung) =>
                rung.quotes.map((quote) => quote.provider_code),
            ),
        ),
    ];
    const quotedBy = (rung: RateLadderRung): string =>
        rung.quotes.length === 0
            ? t('marketdata.rateLadder.noQuotes')
            : rung.quotes.map((quote) => quote.provider_code).join(', ');
    const thin = rows.filter((rung) => rung.quotes.length < (ladder.data?.provider_count ?? 0));

    const head = 'px-2 py-1 text-right text-xs font-semibold text-ink-faint';
    const cell = 'px-2 py-1 text-right font-mono whitespace-nowrap';

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.rateLadder.title')} />
            <PageHeader
                title={t('marketdata.rateLadder.title')}
                description={t('marketdata.rateLadder.lead')}
            />
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.rateLadder.currency')}>
                    <Select
                        value={currency}
                        onChange={(event) => {
                            set({ ccy: event.target.value });
                        }}
                    >
                        {CURRENCIES.map((code) => (
                            <option key={code}>{code}</option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.rateLadder.asOf')}>
                    <Input
                        type="date"
                        value={asOf}
                        onChange={(event) => {
                            if (event.target.value !== '') set({ asof: event.target.value });
                        }}
                    />
                </Field>
                <Field label={t('marketdata.rateLadder.forwardStart')}>
                    <Select
                        value={start}
                        onChange={(event) => {
                            set({ start: event.target.value });
                        }}
                    >
                        {STARTS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.rateLadder.starts.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.rateLadder.window')}>
                    <Select
                        value={window}
                        onChange={(event) => {
                            set({ window: event.target.value });
                        }}
                    >
                        {WINDOWS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.rateLadder.windows.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.rateLadder.providers')}>
                    <Input
                        value={providers}
                        placeholder={t('marketdata.rateLadder.allProviders')}
                        onChange={(event) => {
                            set({ providers: event.target.value });
                        }}
                    />
                </Field>
                {ladder.data !== undefined && (
                    <>
                        <Fact label={t('marketdata.rateLadder.tenorConvention')}>
                            {ladder.data.tenor_convention}
                        </Fact>
                        <Fact label={t('marketdata.rateLadder.dayCount')}>
                            {ladder.data.day_count_convention}
                        </Fact>
                        <Fact label={t('marketdata.rateLadder.providerCount')}>
                            {t('marketdata.rateLadder.providersQuoting', {
                                count: ladder.data.provider_count,
                            })}
                        </Fact>
                    </>
                )}
            </section>
            {ladder.isError && <Notice tone="error">{ladder.error.message}</Notice>}
            {ladder.isSuccess && rows.length === 0 && (
                <Notice tone="info">{t('marketdata.rateLadder.noRungs')}</Notice>
            )}
            {rows.length >= 2 && ladder.data !== undefined && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <h2 className="text-sm font-semibold">{t('marketdata.rateLadder.curve')}</h2>
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.rateLadder.curveLegend', {
                            tenor: ladder.data.tenor_convention,
                            dayCount: ladder.data.day_count_convention,
                        })}
                    </p>
                    <LineChart
                        series={[
                            {
                                points: rows.map((rung) => rung.consensus.value * 100),
                                colour: SERIES_COLOURS[0],
                                fill: true,
                                dots: true,
                                strong: true,
                            },
                        ]}
                        categories={rows.map((rung) => rung.tenor)}
                        format={(value) => `${value.toFixed(3)}%`}
                        label={t('marketdata.rateLadder.curveLabel', {
                            currency: ladder.data.currency,
                        })}
                        padding={0.08}
                    />
                </section>
            )}
            {rows.length > 0 && ladder.data !== undefined && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-b border-line">
                                    <th className={cx(head, 'text-left')}>
                                        {t('marketdata.rateLadder.rung')}
                                    </th>
                                    <th className={head}>{t('marketdata.rateLadder.maturity')}</th>
                                    <th className={head}>{t('marketdata.rateLadder.consensus')}</th>
                                    {quoted.map((provider) => (
                                        <th className={head} key={provider}>
                                            {provider}
                                        </th>
                                    ))}
                                    <th className={head}>{t('marketdata.rateLadder.best')}</th>
                                    <th className={head}>{t('marketdata.rateLadder.distance')}</th>
                                    <th className={head}>{t('marketdata.rateLadder.age')}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((rung) => {
                                    const best =
                                        rung.quotes.length === 0
                                            ? undefined
                                            : Math.max(...rung.quotes.map((q) => q.value));
                                    const isThin = rung.quotes.length < ladder.data.provider_count;
                                    return (
                                        <tr
                                            key={rung.tenor}
                                            className="border-b border-line-subtle"
                                        >
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {rung.tenor}
                                                {isThin && (
                                                    <Tag tone="warn" small>
                                                        {t('marketdata.rateLadder.thin')}
                                                    </Tag>
                                                )}
                                            </td>
                                            <td className={cell}>{rung.maturity_date}</td>
                                            <td className={cell}>
                                                {percent(rung.consensus.value, language)}
                                            </td>
                                            {quoted.map((provider) => {
                                                const quote = rung.quotes.find(
                                                    (one) => one.provider_code === provider,
                                                );
                                                return (
                                                    <td
                                                        className={cx(
                                                            cell,
                                                            quote === undefined
                                                                ? 'text-ink-faint'
                                                                : undefined,
                                                        )}
                                                        key={provider}
                                                    >
                                                        {quote === undefined
                                                            ? '—'
                                                            : percent(quote.value, language)}
                                                    </td>
                                                );
                                            })}
                                            <td className={cell}>
                                                {best === undefined ? '—' : percent(best, language)}
                                            </td>
                                            <td className={cell}>
                                                {distance(best, rung.consensus.value)}
                                            </td>
                                            <td className={cx(cell, 'text-ink-muted')}>
                                                {ageOf(rung, language)}
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    </div>
                </section>
            )}
            {thin.length > 0 && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <h2 className="text-sm font-semibold">
                        {t('marketdata.rateLadder.whereThin')}
                    </h2>
                    <ul className="space-y-1 text-sm">
                        {thin.map((rung) => (
                            <li key={rung.tenor}>
                                {t('marketdata.rateLadder.thinLine', {
                                    rung: rung.tenor,
                                    quoted: quotedBy(rung),
                                    resting: rung.consensus.method,
                                })}
                            </li>
                        ))}
                    </ul>
                </section>
            )}
        </div>
    );
}
