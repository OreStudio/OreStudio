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

import { useMemo, useState, type ReactNode } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
    api,
    type PublicRatesFeed,
    type PublicRatesProvider,
    type SeriesCoverage,
} from '../api/client.js';
import { useHeld } from '../access/holds.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { currenciesOf, pairValues, planPair, pivotOf, seriesOf } from './publicRates.js';

/** How many of a series' latest fixings the table lists. */
const LATEST = 15;

/** The tabs of the screen, in the order the prototype states them. */
type RatesTab = 'publishers' | 'availability' | 'compare' | 'fetch';

/** How a publisher is arriving, from the last fixing it holds. */
type Freshness = 'fresh' | 'late' | 'no-data';

function dateLabel(epoch: number): string {
    return new Date(epoch).toISOString().slice(0, 10);
}

function today(): string {
    return new Date().toISOString().slice(0, 10);
}

function daysBefore(date: string, days: number): string {
    const at = Date.parse(`${date}T12:00:00Z`) - days * 86_400_000;
    return new Date(at).toISOString().slice(0, 10);
}

/** The last fixing a publisher holds, across every series it fills. */
function lastFixingOf(
    provider: PublicRatesProvider,
    coverage: Readonly<Record<string, SeriesCoverage>>,
): string {
    return (
        provider.series
            .map((series) => coverage[series.id]?.last ?? '')
            .filter((date) => date !== '')
            .sort()
            .at(-1) ?? ''
    );
}

/** Fresh when the last fixing is today or yesterday; no data when none is stored. */
function freshnessOf(last: string, now: string): Freshness {
    if (last === '') {
        return 'no-data';
    }
    return last >= daysBefore(now, 1) ? 'fresh' : 'late';
}

function FreshnessTag({ state }: { readonly state: Freshness }): ReactNode {
    const { t } = useTranslation();
    const tone = state === 'fresh' ? 'up' : state === 'late' ? 'warn' : 'muted';
    return (
        <Tag tone={tone} small>
            {t(`marketdata.publicRates.freshness.${state}`)}
        </Tag>
    );
}

function TermsCell({
    provider,
    mayAccept,
    onAccept,
    pending,
}: {
    readonly provider: PublicRatesProvider;
    readonly mayAccept: boolean;
    readonly onAccept: () => void;
    readonly pending: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (provider.terms_accepted) {
        return (
            <div className="space-y-0.5">
                <Tag tone="up" small>
                    {t('marketdata.publicRates.terms.accepted')}
                </Tag>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.publicRates.terms.acceptedBy', {
                        who: provider.terms_accepted_by,
                        when: (provider.terms_accepted_at ?? '').slice(0, 10),
                    })}
                </p>
            </div>
        );
    }
    return (
        <div className="flex flex-wrap items-center gap-2">
            <Tag tone="warn" small>
                {t('marketdata.publicRates.terms.notAccepted')}
            </Tag>
            {mayAccept && (
                <Button onClick={onAccept} disabled={pending}>
                    {t('marketdata.publicRates.terms.accept')}
                </Button>
            )}
        </div>
    );
}

function SeriesChart({
    series,
    label,
}: {
    readonly series: readonly {
        readonly name: string;
        readonly values: ReadonlyMap<string, number>;
    }[];
    readonly label: string;
}): ReactNode {
    const dates = [...new Set(series.flatMap((s) => [...s.values.keys()]))].sort();
    if (dates.length === 0) {
        return null;
    }
    return (
        <LineChart
            label={label}
            xs={dates.map((d) => Date.parse(`${d}T12:00:00Z`))}
            xFormat={dateLabel}
            format={(v) => (Math.abs(v) >= 100 ? v.toFixed(2) : v.toPrecision(5))}
            series={series.map((s, index) => ({
                points: dates.map((d) => s.values.get(d) ?? null),
                colour: SERIES_COLOURS[index % SERIES_COLOURS.length] ?? 'var(--color-accent)',
                strong: series.length === 1,
                fill: series.length === 1,
            }))}
        />
    );
}

/** One publisher's series: pick one, see its fixings drawn and its latest listed. */
function ProviderDetail({ provider }: { readonly provider: PublicRatesProvider }): ReactNode {
    const { t } = useTranslation();
    const [chosen, setChosen] = useState<string | undefined>(provider.series[0]?.id);
    const series = provider.series.find((s) => s.id === chosen) ?? provider.series[0];
    const fixings = useQuery({
        queryKey: ['public-rates-fixings', series?.id],
        queryFn: () => api.publicRatesFixings(series === undefined ? [] : [series.id]),
        enabled: series !== undefined,
    });
    const rows = series === undefined ? [] : (fixings.data?.[series.id] ?? []);
    const values = new Map(rows.map((r) => [r.date, Number(r.value)]));
    return (
        <section className="card space-y-4 p-6" aria-label={provider.name}>
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">
                    {t('marketdata.publicRates.detail.title', { provider: provider.name })}
                </h2>
                <span className="text-xs text-ink-faint">{provider.terms_credit}</span>
            </header>
            {provider.series.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.publicRates.detail.empty')}</p>
            ) : (
                <>
                    <div
                        className="flex flex-wrap gap-1.5"
                        role="radiogroup"
                        aria-label={t('marketdata.publicRates.detail.series')}
                    >
                        {provider.series.map((s) => (
                            <button
                                key={s.id}
                                type="button"
                                role="radio"
                                aria-checked={s.id === series?.id}
                                onClick={() => setChosen(s.id)}
                                className={
                                    s.id === series?.id
                                        ? 'rounded-chip border border-accent bg-accent-dim px-2 py-0.5 font-mono text-xs text-accent'
                                        : 'rounded-chip border border-line px-2 py-0.5 font-mono text-xs text-ink-muted hover:text-ink'
                                }
                            >
                                {`${s.base}/${s.quote}`}
                            </button>
                        ))}
                    </div>
                    {series !== undefined && (
                        <p className="font-mono text-xs text-ink-faint">{series.oresmd_uri}</p>
                    )}
                    {fixings.isError ? (
                        <Notice tone="error">{fixings.error.message}</Notice>
                    ) : fixings.isPending ? (
                        <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                    ) : rows.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {t('marketdata.publicRates.detail.noFixings')}
                        </p>
                    ) : (
                        <div className="grid gap-6 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                            <SeriesChart
                                label={t('marketdata.publicRates.detail.chart', {
                                    pair: `${series?.base ?? ''}/${series?.quote ?? ''}`,
                                })}
                                series={[{ name: provider.code, values }]}
                            />
                            <table className="w-full text-sm">
                                <thead className="text-left text-xs text-ink-faint">
                                    <tr>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.date')}
                                        </th>
                                        <th className="py-1 text-right font-normal">
                                            {t('marketdata.publicRates.columns.value')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {rows
                                        .slice(-LATEST)
                                        .reverse()
                                        .map((r) => (
                                            <tr
                                                key={r.date}
                                                className="border-t border-line-subtle"
                                            >
                                                <td className="py-1 font-mono text-xs">{r.date}</td>
                                                <td className="py-1 text-right font-mono text-xs tabular-nums">
                                                    {r.value}
                                                </td>
                                            </tr>
                                        ))}
                                </tbody>
                            </table>
                        </div>
                    )}
                </>
            )}
        </section>
    );
}

/** One pair from every publisher that gives it, drawn together. */
function PairCompare({
    providers,
}: {
    readonly providers: readonly PublicRatesProvider[];
}): ReactNode {
    const { t } = useTranslation();
    const currencies = currenciesOf(providers);
    const [base, setBase] = useState('EUR');
    const [quote, setQuote] = useState('USD');
    const plans = useMemo(
        () =>
            providers.flatMap((p) => {
                const plan = planPair(p, base, quote);
                return plan === undefined ? [] : [plan];
            }),
        [providers, base, quote],
    );
    const ids = seriesOf(plans);
    const fixings = useQuery({
        queryKey: ['public-rates-fixings', ...ids],
        queryFn: () => api.publicRatesFixings(ids),
        enabled: ids.length > 0,
    });
    const lines = plans.map((plan) => ({
        plan,
        values:
            fixings.data === undefined ? new Map<string, number>() : pairValues(plan, fixings.data),
    }));
    return (
        <section
            className="card space-y-4 p-6"
            aria-label={t('marketdata.publicRates.compare.title')}
        >
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('marketdata.publicRates.compare.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('marketdata.publicRates.compare.lead')}
                </span>
            </header>
            <div className="flex flex-wrap items-center gap-2">
                <div className="w-28">
                    <Select
                        aria-label={t('marketdata.publicRates.compare.base')}
                        value={base}
                        onChange={(e) => setBase(e.target.value)}
                    >
                        {currencies.map((c) => (
                            <option key={c} value={c}>
                                {c}
                            </option>
                        ))}
                    </Select>
                </div>
                <span className="text-ink-faint">/</span>
                <div className="w-28">
                    <Select
                        aria-label={t('marketdata.publicRates.compare.quote')}
                        value={quote}
                        onChange={(e) => setQuote(e.target.value)}
                    >
                        {currencies.map((c) => (
                            <option key={c} value={c}>
                                {c}
                            </option>
                        ))}
                    </Select>
                </div>
            </div>
            {plans.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.publicRates.compare.none')}</p>
            ) : fixings.isError ? (
                <Notice tone="error">{fixings.error.message}</Notice>
            ) : fixings.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                    <SeriesChart
                        label={t('marketdata.publicRates.detail.chart', {
                            pair: `${base}/${quote}`,
                        })}
                        series={lines.map((l) => ({
                            name: l.plan.provider.code,
                            values: l.values,
                        }))}
                    />
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.publicRates.columns.provider')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.publicRates.columns.route')}
                                </th>
                                <th className="px-3 py-1 font-normal">
                                    {t('marketdata.publicRates.columns.date')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('marketdata.publicRates.columns.value')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {lines.map(({ plan, values }, index) => {
                                const last = [...values.keys()].sort().at(-1);
                                return (
                                    <tr
                                        key={plan.provider.code}
                                        className="border-t border-line-subtle"
                                    >
                                        <td className="py-1">
                                            <span
                                                className="mr-2 inline-block size-2 rounded-sm"
                                                style={{
                                                    background:
                                                        SERIES_COLOURS[
                                                            index % SERIES_COLOURS.length
                                                        ],
                                                }}
                                            />
                                            {plan.provider.name}
                                        </td>
                                        <td className="py-1">
                                            <Tag
                                                tone={plan.route === 'cross' ? 'muted' : 'neutral'}
                                                small
                                            >
                                                {t(`marketdata.publicRates.route.${plan.route}`)}
                                            </Tag>
                                        </td>
                                        <td className="whitespace-nowrap px-3 py-1 font-mono text-xs">
                                            {last ?? '—'}
                                        </td>
                                        <td className="py-1 text-right font-mono text-xs tabular-nums">
                                            {last === undefined
                                                ? '—'
                                                : (values.get(last) ?? 0).toPrecision(6)}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
            )}
        </section>
    );
}

/** The feed a publisher's readings come through. */
function routedFeed(provider: PublicRatesProvider): PublicRatesFeed | undefined {
    return provider.feeds.find((feed) => feed.is_default) ?? provider.feeds[0];
}

/** One item, every publisher that fixes it, and how far back each one goes. */
function Availability({
    providers,
    coverage,
}: {
    readonly providers: readonly PublicRatesProvider[];
    readonly coverage: Readonly<Record<string, SeriesCoverage>>;
}): ReactNode {
    const { t } = useTranslation();
    const pairs = useMemo(() => {
        const all = new Set<string>();
        for (const provider of providers) {
            for (const series of provider.series) {
                all.add(`${series.base}/${series.quote}`);
            }
        }
        return [...all].sort();
    }, [providers]);
    const [pair, setPair] = useState(pairs[0] ?? '');
    const [base, quote] = pair.split('/');
    const holders = providers.flatMap((provider) => {
        const series = provider.series.find((s) => s.base === base && s.quote === quote);
        if (series === undefined) {
            return [];
        }
        const covered = coverage[series.id];
        const feed = routedFeed(provider);
        return [
            {
                provider,
                series,
                feed,
                coverage: covered,
            },
        ];
    });
    return (
        <section
            className="card space-y-4 p-6"
            aria-label={t('marketdata.publicRates.availability.title')}
        >
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">
                    {t('marketdata.publicRates.availability.title')}
                </h2>
                <span className="text-xs text-ink-faint">
                    {t('marketdata.publicRates.availability.lead')}
                </span>
            </header>
            <div className="w-40">
                <Select
                    aria-label={t('marketdata.publicRates.availability.pair')}
                    value={pair}
                    onChange={(e) => setPair(e.target.value)}
                >
                    {pairs.map((option) => (
                        <option key={option} value={option}>
                            {option}
                        </option>
                    ))}
                </Select>
            </div>
            {holders.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {t('marketdata.publicRates.availability.empty')}
                </p>
            ) : (
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.provider')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.feed')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.supplier')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.first')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.last')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('marketdata.publicRates.columns.values')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('marketdata.publicRates.columns.freshness')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {holders.map(({ provider, feed, coverage: covered }) => (
                            <tr key={provider.code} className="border-t border-line-subtle">
                                <td className="py-1">{provider.name}</td>
                                <td className="py-1 font-mono text-xs">{feed?.name ?? '—'}</td>
                                <td className="py-1 text-xs">{feed?.supplier_name ?? '—'}</td>
                                <td className="py-1 font-mono text-xs">{covered?.first ?? '—'}</td>
                                <td className="py-1 font-mono text-xs">{covered?.last ?? '—'}</td>
                                <td className="py-1 text-right font-mono text-xs tabular-nums">
                                    {covered?.count ?? 0}
                                </td>
                                <td className="py-1">
                                    <FreshnessTag
                                        state={freshnessOf(covered?.last ?? '', today())}
                                    />
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

/** Asks for a publisher's history and saves it as a file. */
function Fetch({
    providers,
    onDone,
}: {
    readonly providers: readonly PublicRatesProvider[];
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const now = today();
    const [publisher, setPublisher] = useState(providers[0]?.code ?? '');
    const [from, setFrom] = useState(daysBefore(now, 30));
    const [to, setTo] = useState(now);
    const [pair, setPair] = useState('');
    const [saved, setSaved] = useState<{ name: string; rows: number } | undefined>(undefined);
    const provider = providers.find((p) => p.code === publisher) ?? providers[0];
    const pairs = provider?.series.map((s) => `${s.base}/${s.quote}`) ?? [];
    const item =
        pair === ''
            ? undefined
            : provider?.series.find((s) => `${s.base}/${s.quote}` === pair)?.oresmd_uri;
    const download = useMutation({
        mutationFn: () =>
            api.downloadPublicRatesFixings({
                publisher,
                from,
                to,
                ...(item === undefined ? {} : { item }),
            }),
        onSuccess: (file) => {
            setSaved({ name: file.name, rows: file.rows });
            const url = URL.createObjectURL(new Blob([file.content], { type: 'text/csv' }));
            const anchor = document.createElement('a');
            anchor.href = url;
            anchor.download = file.name === '' ? 'fixings.csv' : file.name;
            anchor.click();
            URL.revokeObjectURL(url);
            onDone();
        },
    });
    return (
        <section
            className="card space-y-4 p-6"
            aria-label={t('marketdata.publicRates.fetch.title')}
        >
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('marketdata.publicRates.fetch.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('marketdata.publicRates.fetch.lead')}
                </span>
            </header>
            <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                <label className="space-y-1 text-xs text-ink-muted">
                    <span>{t('marketdata.publicRates.fetch.publisher')}</span>
                    <Select
                        value={publisher}
                        onChange={(e) => {
                            setPublisher(e.target.value);
                            setPair('');
                            setSaved(undefined);
                        }}
                    >
                        {providers.map((p) => (
                            <option key={p.code} value={p.code}>
                                {p.name}
                            </option>
                        ))}
                    </Select>
                </label>
                <label className="space-y-1 text-xs text-ink-muted">
                    <span>{t('marketdata.publicRates.fetch.from')}</span>
                    <input
                        type="date"
                        className="w-full rounded-control border border-line bg-surface px-2 py-1 text-sm"
                        value={from}
                        onChange={(e) => setFrom(e.target.value)}
                    />
                </label>
                <label className="space-y-1 text-xs text-ink-muted">
                    <span>{t('marketdata.publicRates.fetch.to')}</span>
                    <input
                        type="date"
                        className="w-full rounded-control border border-line bg-surface px-2 py-1 text-sm"
                        value={to}
                        onChange={(e) => setTo(e.target.value)}
                    />
                </label>
                <label className="space-y-1 text-xs text-ink-muted">
                    <span>{t('marketdata.publicRates.fetch.item')}</span>
                    <Select value={pair} onChange={(e) => setPair(e.target.value)}>
                        <option value="">{t('marketdata.publicRates.fetch.allItems')}</option>
                        {pairs.map((option) => (
                            <option key={option} value={option}>
                                {option}
                            </option>
                        ))}
                    </Select>
                </label>
            </div>
            <p className="text-xs text-ink-faint">{t('marketdata.publicRates.fetch.note')}</p>
            {from > to && <Notice tone="error">{t('marketdata.publicRates.fetch.range')}</Notice>}
            {download.isError && <Notice tone="error">{download.error.message}</Notice>}
            {saved !== undefined && (
                <Notice tone="info">
                    {t('marketdata.publicRates.fetch.done', {
                        file: saved.name,
                        rows: String(saved.rows),
                    })}
                </Notice>
            )}
            <Button
                onClick={() => download.mutate()}
                disabled={download.isPending || from > to || publisher === ''}
            >
                {download.isPending
                    ? t('marketdata.publicRates.fetch.downloading')
                    : t('marketdata.publicRates.fetch.download')}
            </Button>
        </section>
    );
}

/**
 * The public rates: the public publishers, their terms, which a system
 * administrator accepts here, the feeds that carry their values, the fixings
 * each has stored, one pair from all of them side by side, and the history a
 * person asks for.
 */
export function PublicRatesPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHeld();
    const queries = useQueryClient();
    const providers = useQuery({ queryKey: ['public-rates'], queryFn: api.publicRates });
    const coverage = useQuery({
        queryKey: ['public-rates-coverage'],
        queryFn: api.publicRatesCoverage,
    });
    const [tab, setTab] = useState<RatesTab>('publishers');
    const [chosen, setChosen] = useState<string | undefined>(undefined);
    const accept = useMutation({
        mutationFn: (provider: PublicRatesProvider) =>
            api.acceptProviderTerms(provider.code, provider.version),
        onSettled: () => queries.invalidateQueries({ queryKey: ['public-rates'] }),
    });
    const load = { dataUpdatedAt: providers.dataUpdatedAt, isFetching: providers.isFetching };
    const news = useEntityChanges([{ component: 'marketdata', entity: 'providers' }], load);
    const trail = <AreaTrail area="marketdata" screen={t('marketdata.publicRates.title')} />;
    if (providers.data === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('marketdata.publicRates.title')}
                description={t('marketdata.publicRates.lead')}
            >
                {providers.isError ? (
                    <Notice tone="error">{providers.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }
    const list = providers.data;
    const seriesCoverage = coverage.data ?? {};
    const now = today();
    const mayAccept = holds('marketdata::providers:accept_terms');
    const selected = list.find((p) => p.code === chosen) ?? list[0];
    const tabs: readonly { readonly id: RatesTab; readonly label: string }[] = [
        { id: 'publishers', label: t('marketdata.publicRates.tabs.publishers') },
        { id: 'availability', label: t('marketdata.publicRates.tabs.availability') },
        { id: 'compare', label: t('marketdata.publicRates.tabs.compare') },
        { id: 'fetch', label: t('marketdata.publicRates.tabs.fetch') },
    ];
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.publicRates.title')}
                    description={t('marketdata.publicRates.lead')}
                    actions={
                        <RefreshButton
                            onClick={() => void providers.refetch()}
                            load={load}
                            news={news}
                        />
                    }
                />
            </div>
            {accept.isError && <Notice tone="error">{accept.error.message}</Notice>}
            <nav
                className="flex flex-wrap gap-1"
                aria-label={t('marketdata.publicRates.tabs.label')}
            >
                {tabs.map((entry) => (
                    <button
                        key={entry.id}
                        type="button"
                        onClick={() => setTab(entry.id)}
                        aria-current={tab === entry.id ? 'page' : undefined}
                        className={
                            tab === entry.id
                                ? 'rounded-control border border-accent bg-accent-dim px-3 py-1 text-sm text-accent'
                                : 'rounded-control border border-line px-3 py-1 text-sm text-ink-muted hover:text-ink'
                        }
                    >
                        {entry.label}
                    </button>
                ))}
            </nav>
            {tab === 'publishers' && (
                <>
                    <section
                        className="card space-y-4 p-6"
                        aria-label={t('marketdata.publicRates.providers.title')}
                    >
                        <header className="flex flex-wrap items-baseline justify-between gap-2">
                            <h2 className="text-lg font-medium">
                                {t('marketdata.publicRates.providers.title')}
                            </h2>
                            <span className="text-xs text-ink-faint">
                                {t('marketdata.publicRates.providers.lead')}
                            </span>
                        </header>
                        {list.length === 0 ? (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.publicRates.providers.empty')}
                            </p>
                        ) : (
                            <table className="w-full text-sm">
                                <thead className="text-left text-xs text-ink-faint">
                                    <tr>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.provider')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.pivot')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.terms')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.feed')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.last')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.publicRates.columns.freshness')}
                                        </th>
                                        <th className="py-1 text-right font-normal">
                                            {t('marketdata.publicRates.columns.series')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {list.map((p) => {
                                        const last = lastFixingOf(p, seriesCoverage);
                                        const feed = routedFeed(p);
                                        return (
                                            <tr
                                                key={p.code}
                                                className={
                                                    p.code === selected?.code
                                                        ? 'border-t border-line-subtle bg-accent-dim'
                                                        : 'border-t border-line-subtle'
                                                }
                                            >
                                                <td className="py-2 align-top">
                                                    <button
                                                        type="button"
                                                        onClick={() => setChosen(p.code)}
                                                        className="text-left font-medium hover:text-accent"
                                                    >
                                                        {p.name}
                                                    </button>
                                                    <p className="max-w-md text-xs text-ink-faint">
                                                        {p.description}
                                                    </p>
                                                    <a
                                                        href={p.terms_url}
                                                        target="_blank"
                                                        rel="noreferrer"
                                                        className="text-xs text-accent hover:underline"
                                                    >
                                                        {t('marketdata.publicRates.terms.read')}
                                                    </a>
                                                </td>
                                                <td className="py-2 align-top font-mono text-xs">
                                                    {pivotOf(p) ?? '—'}
                                                </td>
                                                <td className="py-2 align-top">
                                                    <TermsCell
                                                        provider={p}
                                                        mayAccept={mayAccept}
                                                        pending={accept.isPending}
                                                        onAccept={() => accept.mutate(p)}
                                                    />
                                                </td>
                                                <td className="py-2 align-top text-xs">
                                                    <p className="font-mono">{feed?.name ?? '—'}</p>
                                                    <p className="text-ink-faint">
                                                        {feed?.supplier_name ?? '—'} ·{' '}
                                                        {feed?.service ?? '—'}
                                                    </p>
                                                </td>
                                                <td className="py-2 align-top font-mono text-xs">
                                                    {last || '—'}
                                                </td>
                                                <td className="py-2 align-top">
                                                    <FreshnessTag state={freshnessOf(last, now)} />
                                                </td>
                                                <td className="py-2 text-right align-top tabular-nums">
                                                    {p.series.length}
                                                </td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        )}
                    </section>
                    {selected !== undefined && (
                        <ProviderDetail key={selected.code} provider={selected} />
                    )}
                </>
            )}
            {tab === 'availability' && <Availability providers={list} coverage={seriesCoverage} />}
            {tab === 'compare' && <PairCompare providers={list} />}
            {tab === 'fetch' && (
                <Fetch
                    providers={list}
                    onDone={() => {
                        void providers.refetch();
                    }}
                />
            )}
            {selected !== undefined && tab === 'publishers' && (
                <p className="text-xs text-ink-faint">
                    <a href={PATHS.connectivity} className="text-accent hover:underline">
                        {t('marketdata.publicRates.configure')}
                    </a>
                </p>
            )}
        </div>
    );
}
