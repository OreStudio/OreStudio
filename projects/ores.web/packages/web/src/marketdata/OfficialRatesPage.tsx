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
import { api, type OfficialProvider } from '../api/client.js';
import { useHeld } from '../access/holds.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { currenciesOf, pairValues, planPair, pivotOf, seriesOf } from './officialRates.js';

/** How many of a series' latest fixings the table lists. */
const LATEST = 15;

function dateLabel(epoch: number): string {
    return new Date(epoch).toISOString().slice(0, 10);
}

function TermsCell({
    provider,
    mayAccept,
    onAccept,
    pending,
}: {
    readonly provider: OfficialProvider;
    readonly mayAccept: boolean;
    readonly onAccept: () => void;
    readonly pending: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (provider.terms_accepted) {
        return (
            <div className="space-y-0.5">
                <Tag tone="up" small>
                    {t('marketdata.official.terms.accepted')}
                </Tag>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.official.terms.acceptedBy', {
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
                {t('marketdata.official.terms.notAccepted')}
            </Tag>
            {mayAccept && (
                <Button onClick={onAccept} disabled={pending}>
                    {t('marketdata.official.terms.accept')}
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

/** One provider's series: pick one, see its fixings drawn and its latest listed. */
function ProviderDetail({ provider }: { readonly provider: OfficialProvider }): ReactNode {
    const { t } = useTranslation();
    const [chosen, setChosen] = useState<string | undefined>(provider.series[0]?.id);
    const series = provider.series.find((s) => s.id === chosen) ?? provider.series[0];
    const fixings = useQuery({
        queryKey: ['official-fixings', series?.id],
        queryFn: () => api.officialFixings(series === undefined ? [] : [series.id]),
        enabled: series !== undefined,
    });
    const rows = series === undefined ? [] : (fixings.data?.[series.id] ?? []);
    const values = new Map(rows.map((r) => [r.date, Number(r.value)]));
    return (
        <section className="card space-y-4 p-6" aria-label={provider.name}>
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">
                    {t('marketdata.official.detail.title', { provider: provider.name })}
                </h2>
                <span className="text-xs text-ink-faint">{provider.terms_credit}</span>
            </header>
            {provider.series.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.official.detail.empty')}</p>
            ) : (
                <>
                    <div
                        className="flex flex-wrap gap-1.5"
                        role="radiogroup"
                        aria-label={t('marketdata.official.detail.series')}
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
                            {t('marketdata.official.detail.noFixings')}
                        </p>
                    ) : (
                        <div className="grid gap-6 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                            <SeriesChart
                                label={t('marketdata.official.detail.chart', {
                                    pair: `${series?.base ?? ''}/${series?.quote ?? ''}`,
                                })}
                                series={[{ name: provider.code, values }]}
                            />
                            <table className="w-full text-sm">
                                <thead className="text-left text-xs text-ink-faint">
                                    <tr>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.official.columns.date')}
                                        </th>
                                        <th className="py-1 text-right font-normal">
                                            {t('marketdata.official.columns.value')}
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

/** One pair from every provider that gives it, drawn together. */
function PairCompare({
    providers,
}: {
    readonly providers: readonly OfficialProvider[];
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
        queryKey: ['official-fixings', ...ids],
        queryFn: () => api.officialFixings(ids),
        enabled: ids.length > 0,
    });
    const lines = plans.map((plan) => ({
        plan,
        values:
            fixings.data === undefined ? new Map<string, number>() : pairValues(plan, fixings.data),
    }));
    return (
        <section className="card space-y-4 p-6" aria-label={t('marketdata.official.compare.title')}>
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('marketdata.official.compare.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('marketdata.official.compare.lead')}
                </span>
            </header>
            <div className="flex flex-wrap items-center gap-2">
                <div className="w-28">
                    <Select
                        aria-label={t('marketdata.official.compare.base')}
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
                        aria-label={t('marketdata.official.compare.quote')}
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
                <p className="text-sm text-ink-muted">{t('marketdata.official.compare.none')}</p>
            ) : fixings.isError ? (
                <Notice tone="error">{fixings.error.message}</Notice>
            ) : fixings.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,2fr)_minmax(0,1fr)]">
                    <SeriesChart
                        label={t('marketdata.official.detail.chart', { pair: `${base}/${quote}` })}
                        series={lines.map((l) => ({
                            name: l.plan.provider.code,
                            values: l.values,
                        }))}
                    />
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.provider')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.route')}
                                </th>
                                <th className="px-3 py-1 font-normal">
                                    {t('marketdata.official.columns.date')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('marketdata.official.columns.value')}
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
                                                {t(`marketdata.official.route.${plan.route}`)}
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

/**
 * The official rates: the public providers whose daily rates the official rates adapter
 * reads, their terms, which a system administrator accepts here, the fixings each has
 * stored, and one pair from all of them side by side.
 */
export function OfficialRatesPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHeld();
    const queries = useQueryClient();
    const providers = useQuery({ queryKey: ['official-rates'], queryFn: api.officialRates });
    const [chosen, setChosen] = useState<string | undefined>(undefined);
    const accept = useMutation({
        mutationFn: (code: string) => api.acceptProviderTerms(code),
        onSuccess: () => queries.invalidateQueries({ queryKey: ['official-rates'] }),
    });
    const load = { dataUpdatedAt: providers.dataUpdatedAt, isFetching: providers.isFetching };
    const news = useEntityChanges([{ component: 'marketdata', entity: 'providers' }], load);
    const trail = <AreaTrail area="marketdata" screen={t('marketdata.official.title')} />;
    if (providers.data === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('marketdata.official.title')}
                description={t('marketdata.official.lead')}
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
    const mayAccept = holds('marketdata::providers:accept_terms');
    const selected = list.find((p) => p.code === chosen) ?? list[0];
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.official.title')}
                    description={t('marketdata.official.lead')}
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
            <section
                className="card space-y-4 p-6"
                aria-label={t('marketdata.official.providers.title')}
            >
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">
                        {t('marketdata.official.providers.title')}
                    </h2>
                    <span className="text-xs text-ink-faint">
                        {t('marketdata.official.providers.lead')}
                    </span>
                </header>
                {list.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.official.providers.empty')}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.provider')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.pivot')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.terms')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('marketdata.official.columns.source')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('marketdata.official.columns.series')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {list.map((p) => (
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
                                            {t('marketdata.official.terms.read')}
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
                                            onAccept={() => accept.mutate(p.code)}
                                        />
                                    </td>
                                    <td className="py-2 align-top font-mono text-xs">
                                        {p.sources.map((s) => s.name).join(', ') || '—'}
                                    </td>
                                    <td className="py-2 text-right align-top tabular-nums">
                                        {p.series.length}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </section>
            {selected !== undefined && <ProviderDetail key={selected.code} provider={selected} />}
            {list.length > 0 && <PairCompare providers={list} />}
        </div>
    );
}
