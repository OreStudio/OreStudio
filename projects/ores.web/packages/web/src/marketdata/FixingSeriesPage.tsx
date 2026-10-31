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

/**
 * One item in the market data store, named by its oresmd URI: the series, the
 * fixings written against it and the feeds that carried them. This is the page
 * an oresmd link opens; the publisher and the feeds are provenance, not the item.
 */

import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { marketDataSourcePath } from '../shell/paths.js';
import { Detail, Notice, PageHeader } from '../ui/Primitives.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';

/** The most values the table lists; the count states the rest. */
const LATEST = 100;

export function FixingSeriesPage(): ReactNode {
    const { t } = useTranslation();
    const [params] = useSearchParams();
    const uri = params.get('uri') ?? '';
    const query = useQuery({
        queryKey: ['fixing-series', uri],
        queryFn: () => api.fixingSeries(uri),
        enabled: uri !== '',
    });
    const trail = <AreaTrail area="marketdata" screen={t('marketdata.fixingSeries.title')} />;
    const data = query.data;
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.fixingSeries.title')}
                    description={t('marketdata.fixingSeries.lead')}
                />
            </div>
            {uri === '' ? (
                <Notice tone="warn">{t('marketdata.fixingSeries.noUri')}</Notice>
            ) : query.isError ? (
                <Notice tone="error">{query.error.message}</Notice>
            ) : query.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : data?.series == null ? (
                <Notice tone="warn">{t('marketdata.fixingSeries.notStored', { uri })}</Notice>
            ) : (
                <>
                    <section className="card space-y-4 p-6">
                        <p className="break-all font-mono text-xs text-ink-muted">{uri}</p>
                        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-4">
                            <Detail
                                label={t('marketdata.fixingSeries.columns.provider')}
                                value={data.publisher?.name ?? data.series.provider_code}
                            />
                            <Detail
                                label={t('marketdata.fixingSeries.columns.first')}
                                value={
                                    data.values?.first === '' ? '—' : (data.values?.first ?? '—')
                                }
                                mono
                            />
                            <Detail
                                label={t('marketdata.fixingSeries.columns.last')}
                                value={data.values?.last === '' ? '—' : (data.values?.last ?? '—')}
                                mono
                            />
                            <Detail
                                label={t('marketdata.fixingSeries.columns.count')}
                                value={(data.values?.count ?? 0).toLocaleString()}
                                mono
                            />
                        </dl>
                        {(data.values?.rows.length ?? 0) === 0 ? (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.fixingSeries.noValues')}
                            </p>
                        ) : (
                            <>
                                <LineChart
                                    label={t('marketdata.fixingSeries.chart', {
                                        item: data.publisher?.name ?? data.series.provider_code,
                                    })}
                                    xs={(data.values?.rows ?? []).map((row) =>
                                        Date.parse(`${row.date}T12:00:00Z`),
                                    )}
                                    xFormat={(epoch) => new Date(epoch).toISOString().slice(0, 10)}
                                    format={(value) =>
                                        Math.abs(value) >= 100
                                            ? value.toFixed(2)
                                            : value.toPrecision(5)
                                    }
                                    series={[
                                        {
                                            points: (data.values?.rows ?? []).map((row) =>
                                                Number(row.value),
                                            ),
                                            colour: SERIES_COLOURS[0] ?? 'var(--color-accent)',
                                            strong: true,
                                            fill: true,
                                        },
                                    ]}
                                />
                                <table className="w-full text-sm">
                                    <thead className="text-left text-xs text-ink-faint">
                                        <tr>
                                            <th className="py-1 font-normal">
                                                {t('marketdata.fixingSeries.columns.date')}
                                            </th>
                                            <th className="py-1 text-right font-normal">
                                                {t('marketdata.fixingSeries.columns.value')}
                                            </th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {[...(data.values?.rows ?? [])]
                                            .reverse()
                                            .slice(0, LATEST)
                                            .map((row) => (
                                                <tr
                                                    key={row.date}
                                                    className="border-t border-line-subtle"
                                                >
                                                    <td className="py-1 font-mono text-xs">
                                                        {row.date}
                                                    </td>
                                                    <td className="py-1 text-right font-mono text-xs tabular-nums">
                                                        {row.value}
                                                    </td>
                                                </tr>
                                            ))}
                                    </tbody>
                                </table>
                            </>
                        )}
                    </section>
                    <section className="card space-y-3 p-6">
                        <h2 className="text-sm font-medium">
                            {t('marketdata.fixingSeries.provenance')}
                        </h2>
                        {data.feeds.length === 0 ? (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.fixingSeries.noFeeds')}
                            </p>
                        ) : (
                            <table className="w-full text-sm">
                                <thead className="text-left text-xs text-ink-faint">
                                    <tr>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.fixingSeries.columns.feed')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.fixingSeries.columns.service')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.fixingSeries.columns.session')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('marketdata.fixingSeries.columns.supplier')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {data.feeds.map((feed) => (
                                        <tr key={feed.id} className="border-t border-line-subtle">
                                            <td className="py-1 font-mono text-xs">
                                                <Link
                                                    to={`/market-data/connectivity?feed=${encodeURIComponent(
                                                        feed.id,
                                                    )}`}
                                                    className="text-accent hover:underline"
                                                >
                                                    {feed.name}
                                                </Link>
                                            </td>
                                            <td className="py-1 text-xs">{feed.service}</td>
                                            <td className="py-1 font-mono text-xs">
                                                {feed.session_code}
                                            </td>
                                            <td className="py-1 text-xs">{feed.supplier_name}</td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        )}
                    </section>
                </>
            )}
            <Link to="/market-data" className="text-sm text-accent hover:underline">
                {t('marketdata.fixingSeries.back')}
            </Link>
        </div>
    );
}
