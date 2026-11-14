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
import { DATA_CELL, HEAD_CELL, asOfInstant, oneOf, todayIso, useReadParty } from './readScreen.js';

/**
 * One currency's rate ladder, from
 * doc/prototypes/market_data/read-a-rate-ladder/index.html and the journey that
 * answers to it.
 *
 * The rungs, their maturity dates, their year fractions and the consensus are the
 * server's, because the calendar, the day count and the tenor convention are
 * server facts. The screen adds what the prototype asked for and no read states:
 * the best quote of a rung, its distance in basis points from the consensus, and
 * the spread the same quotes draw between two rungs.
 */

const CURRENCIES = ['USD', 'EUR', 'GBP', 'JPY', 'CHF', 'AUD', 'CAD', 'NZD'] as const;

/// The forward start a ladder is quoted from: spot, or a broken date.
const STARTS = ['0D', '3M', '1Y'] as const;

/// How much of the curve the ladder shows, by the year fraction the server states.
const WINDOWS = ['short', 'all', 'long'] as const;

/// The two rungs whose distance the journey calls the spread.
const SPREAD_NEAR = '2Y';
const SPREAD_FAR = '30Y';

type LadderWindow = (typeof WINDOWS)[number];

/** Whether a rung falls in the window, by the year fraction the server dated it with. */
function inWindow(rung: RateLadderRung, window: LadderWindow): boolean {
    if (window === 'short') return rung.year_fraction < 1.0 / 12.0;
    if (window === 'long') return rung.year_fraction >= 1.0 && rung.year_fraction <= 10.0;
    return true;
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

/** The providers that quote any of the rungs, in the order the server lists them. */
function providersOf(rungs: readonly RateLadderRung[]): readonly string[] {
    return [...new Set(rungs.flatMap((rung) => rung.quotes.map((quote) => quote.provider_code)))];
}

/**
 * The spread the market quotes between two rungs, per provider and across the
 * consensus, in basis points. A spread is drawn only where both rungs are
 * quoted: a gap at either end is a spread nobody quoted, not a zero.
 */
function spreads(
    rungs: readonly RateLadderRung[],
    consensusLabel: string,
): readonly { readonly provider: string; readonly value: string }[] {
    const near = rungs.find((rung) => rung.tenor === SPREAD_NEAR);
    const far = rungs.find((rung) => rung.tenor === SPREAD_FAR);
    if (near === undefined || far === undefined) return [];
    const at = (rung: RateLadderRung, provider: string): number | undefined =>
        rung.quotes.find((quote) => quote.provider_code === provider)?.value;
    const rows: { provider: string; value: string }[] = [];
    for (const provider of providersOf([near, far])) {
        const low = at(near, provider);
        const high = at(far, provider);
        if (low === undefined || high === undefined) continue;
        rows.push({ provider, value: `${((high - low) * 10000).toFixed(1)} bp` });
    }
    rows.push({
        provider: consensusLabel,
        value: `${((far.consensus.value - near.consensus.value) * 10000).toFixed(1)} bp`,
    });
    return rows;
}

export function RateLadderPage(): ReactNode {
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

    const currency = (search.get('ccy') ?? 'USD').toUpperCase();
    const asOf = search.get('asof') ?? todayIso();
    const providers = search.get('providers') ?? '';
    const start = oneOf(search.get('start'), STARTS, '0D');
    const window = oneOf(search.get('window'), WINDOWS, 'all');

    // The instrument types the currency's curves hold, so the screen offers the
    // ones the service can answer for rather than a list it invented.
    const catalogue = useQuery({
        queryKey: ['market-instruments', currency, 'curve', ''],
        queryFn: () => api.marketInstruments({ query: currency, kinds: ['curve'], limit: 100 }),
        retry: false,
    });
    const offered = [
        ...new Set((catalogue.data?.instruments ?? []).map((row) => row.instrument_type)),
    ];
    const offeredTypes = offered.length === 0 ? ['discount'] : offered;
    const instrument = oneOf(search.get('instrument'), offeredTypes, offeredTypes[0] ?? 'discount');

    const ladder = useQuery({
        queryKey: ['rate-ladder', currency, asOf, providers, start, instrument],
        queryFn: () =>
            api.rateLadder({
                currency,
                asOf: asOfInstant(asOf),
                instrumentType: instrument,
                providers: providers === '' ? [] : providers.split(','),
                forwardStart: start,
            }),
        retry: false,
    });

    const allRungs = ladder.data?.rungs ?? [];
    const rows = allRungs.filter((rung) => inWindow(rung, window));
    const quoted = providersOf(allRungs);
    const providerCount = ladder.data?.provider_count ?? 0;
    const isThin = (rung: RateLadderRung): boolean => rung.quotes.length < providerCount;
    const quotedBy = (rung: RateLadderRung): string =>
        rung.quotes.length === 0
            ? t('marketdata.rateLadder.noQuotes')
            : rung.quotes.map((quote) => quote.provider_code).join(', ');
    const thin = rows.filter(isThin);
    const spread = spreads(allRungs, t('marketdata.rateLadder.spreadConsensus'));

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.rateLadder.title')} />
            <div className="flex items-start justify-between gap-3">
                <PageHeader
                    title={t('marketdata.rateLadder.title')}
                    description={t('marketdata.rateLadder.lead')}
                />
                {party !== '' && (
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.rateLadder.readingFor', { party })}
                    </p>
                )}
            </div>
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.rateLadder.currency')}>
                    <Select
                        value={currency}
                        onChange={(event) => {
                            set({ ccy: event.target.value, instrument: '' });
                        }}
                    >
                        {CURRENCIES.map((code) => (
                            <option key={code}>{code}</option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.rateLadder.instrument')}>
                    <Select
                        value={instrument}
                        onChange={(event) => {
                            set({ instrument: event.target.value });
                        }}
                    >
                        {(offered.length === 0 ? [instrument] : offered).map((type) => (
                            <option key={type} value={type}>
                                {type}
                            </option>
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
                                    <th className={cx(HEAD_CELL, 'text-left')}>
                                        {t('marketdata.rateLadder.rung')}
                                    </th>
                                    <th className={HEAD_CELL}>
                                        {t('marketdata.rateLadder.maturity')}
                                    </th>
                                    <th className={HEAD_CELL}>{t('marketdata.rateLadder.tau')}</th>
                                    <th className={HEAD_CELL}>
                                        {t('marketdata.rateLadder.consensus')}
                                    </th>
                                    {quoted.map((provider) => (
                                        <th className={HEAD_CELL} key={provider}>
                                            {provider}
                                        </th>
                                    ))}
                                    <th className={HEAD_CELL}>{t('marketdata.rateLadder.best')}</th>
                                    <th className={HEAD_CELL}>
                                        {t('marketdata.rateLadder.distance')}
                                    </th>
                                    <th className={HEAD_CELL}>{t('marketdata.rateLadder.age')}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {rows.map((rung) => {
                                    const best =
                                        rung.quotes.length === 0
                                            ? undefined
                                            : Math.max(...rung.quotes.map((q) => q.value));
                                    return (
                                        <tr
                                            key={rung.tenor}
                                            className="border-b border-line-subtle"
                                        >
                                            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                                                {rung.tenor}
                                                {isThin(rung) && (
                                                    <Tag tone="warn" small>
                                                        {t('marketdata.rateLadder.thin')}
                                                    </Tag>
                                                )}
                                            </td>
                                            <td className={DATA_CELL}>{rung.maturity_date}</td>
                                            <td className={DATA_CELL}>
                                                {rung.year_fraction.toFixed(4)}
                                            </td>
                                            <td className={DATA_CELL}>
                                                {percent(rung.consensus.value, language)}
                                            </td>
                                            {quoted.map((provider) => {
                                                const quote = rung.quotes.find(
                                                    (one) => one.provider_code === provider,
                                                );
                                                return (
                                                    <td
                                                        className={cx(
                                                            DATA_CELL,
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
                                            <td className={DATA_CELL}>
                                                {best === undefined ? '—' : percent(best, language)}
                                            </td>
                                            <td className={DATA_CELL}>
                                                {distance(best, rung.consensus.value)}
                                            </td>
                                            <td className={cx(DATA_CELL, 'text-ink-muted')}>
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
            {spread.length > 0 && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <h2 className="text-sm font-semibold">
                        {t('marketdata.rateLadder.spreadTitle', {
                            near: SPREAD_NEAR,
                            far: SPREAD_FAR,
                        })}
                    </h2>
                    <table className="text-sm">
                        <tbody>
                            {spread.map((row) => (
                                <tr key={row.provider} className="border-b border-line-subtle">
                                    <td className="px-2 py-1 text-ink-muted">{row.provider}</td>
                                    <td className={DATA_CELL}>{row.value}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </section>
            )}
            {allRungs.length > 0 && spread.length === 0 && (
                <Notice tone="info">
                    {t('marketdata.rateLadder.noSpread', { near: SPREAD_NEAR, far: SPREAD_FAR })}
                </Notice>
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
