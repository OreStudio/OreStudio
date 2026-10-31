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

import { useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { api, type PublicRatesProvider } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';

/** How many of a series' latest fixings the table lists. */
const LATEST = 15;

function dateLabel(epoch: number): string {
    return new Date(epoch).toISOString().slice(0, 10);
}

export function SeriesChart({
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
export function ProviderDetail({
    provider,
}: {
    readonly provider: PublicRatesProvider;
}): ReactNode {
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
