/**
 * The system view of the fixing observations: every FX and IR fixing series the
 * platform holds, one row each, with the source its values came from, the value
 * of its latest day and how many days it holds.
 *
 * It is not the party's fixings manager. The screen states what the sources
 * published, missing days included, and offers no authorisation: a day the
 * platform never stated is entered by a party, in its own fixings manager.
 */

import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import { api, type FixingObservation } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { fixingSeriesPath } from '../shell/paths.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';

/** The filters the address holds, so a view can be sent to someone else. */
type Filters = {
    readonly assetClass: string;
    readonly currency: string;
    readonly source: string;
};

/** A series that holds no day reads as such rather than disappearing. */
function DaysCell({ series }: { readonly series: FixingObservation }): ReactNode {
    const { t } = useTranslation();
    if (series.days === 0) {
        return (
            <Tag tone="warn" small>
                {t('marketdata.fixingObservations.noDay')}
            </Tag>
        );
    }
    return <span className="tabular-nums">{series.days}</span>;
}

function SeriesRow({ series }: { readonly series: FixingObservation }): ReactNode {
    const { t } = useTranslation();
    return (
        <tr className={series.days === 0 ? 'text-ink-muted' : undefined}>
            <td className="px-3 py-2">
                <Link
                    className="font-medium text-ink hover:underline"
                    to={fixingSeriesPath(series.oresmd_uri)}
                >
                    {series.index_label}
                </Link>
                <p className="break-all font-mono text-xs text-ink-faint">{series.oresmd_uri}</p>
            </td>
            <td className="px-3 py-2">
                <Tag tone={series.asset_class === 'ir' ? 'up' : 'muted'} small>
                    {series.asset_class.toUpperCase()}
                </Tag>
            </td>
            {/* The label states the currency first for an index, and the pair for FX. */}
            <td className="px-3 py-2">{series.index_label.split(' ')[0] ?? '—'}</td>
            <td className="px-3 py-2">{series.source === '' ? '—' : series.source}</td>
            <td className="px-3 py-2">{series.provider_code}</td>
            <td className="px-3 py-2 tabular-nums">
                {series.latest_value === '' ? '—' : series.latest_value}
            </td>
            <td className="px-3 py-2 tabular-nums">{series.latest_date ?? '—'}</td>
            <td className="px-3 py-2">
                <DaysCell series={series} />
            </td>
            <td className="px-3 py-2 text-right">
                <Link
                    className="text-sm text-ink-muted hover:underline"
                    to={fixingSeriesPath(series.oresmd_uri)}
                >
                    {t('marketdata.fixingObservations.columns.open')}
                </Link>
            </td>
        </tr>
    );
}

export function FixingObservationsPage(): ReactNode {
    const { t } = useTranslation();
    const [params, setParams] = useSearchParams();
    const filters: Filters = {
        assetClass: params.get('class') ?? '',
        currency: params.get('currency') ?? '',
        source: params.get('source') ?? '',
    };
    const query = useQuery({
        queryKey: ['fixing-observations', filters.assetClass, filters.currency, filters.source],
        queryFn: () => api.fixingObservations(filters),
    });
    const choose = (name: keyof Filters, value: string) => {
        const next = new URLSearchParams(params);
        if (value === '') {
            next.delete(name === 'assetClass' ? 'class' : name);
        } else {
            next.set(name === 'assetClass' ? 'class' : name, value);
        }
        setParams(next, { replace: true });
    };
    const series = query.data ?? [];
    const currencies = [
        ...new Set(series.flatMap((one) => (one.index_label.split(' ')[0] ?? '').split('/'))),
    ]
        .filter((code) => code !== '')
        .sort();
    const sources = [
        ...new Set(series.map((one) => one.source).filter((one) => one !== '')),
    ].sort();
    const empty = series.filter((one) => one.days === 0).length;
    return (
        <div className="space-y-6">
            <div className="flex flex-wrap items-start justify-between gap-3">
                <div>
                    <AreaTrail
                        area="marketdata"
                        screen={t('marketdata.fixingObservations.title')}
                    />
                    <PageHeader
                        title={t('marketdata.fixingObservations.title')}
                        description={t('marketdata.fixingObservations.lead')}
                    />
                </div>
                <Button
                    icon="refresh"
                    onClick={() => void query.refetch()}
                    pending={query.isFetching}
                >
                    {t('common.refresh')}
                </Button>
            </div>
            <Notice tone="info">{t('marketdata.fixingObservations.systemView')}</Notice>
            <section className="card space-y-4 p-6">
                <div className="flex flex-wrap items-end gap-3">
                    <div className="w-44">
                        <Select
                            aria-label={t('marketdata.fixingObservations.filters.assetClass')}
                            value={filters.assetClass}
                            onChange={(event) => choose('assetClass', event.target.value)}
                        >
                            <option value="">
                                {t('marketdata.fixingObservations.filters.everyClass')}
                            </option>
                            <option value="fx">FX</option>
                            <option value="ir">IR</option>
                        </Select>
                    </div>
                    <div className="w-44">
                        <Select
                            aria-label={t('marketdata.fixingObservations.filters.currency')}
                            value={filters.currency}
                            onChange={(event) => choose('currency', event.target.value)}
                        >
                            <option value="">
                                {t('marketdata.fixingObservations.filters.everyCurrency')}
                            </option>
                            {currencies.map((code) => (
                                <option key={code} value={code}>
                                    {code}
                                </option>
                            ))}
                        </Select>
                    </div>
                    <div className="w-56">
                        <Select
                            aria-label={t('marketdata.fixingObservations.filters.source')}
                            value={filters.source}
                            onChange={(event) => choose('source', event.target.value)}
                        >
                            <option value="">
                                {t('marketdata.fixingObservations.filters.everySource')}
                            </option>
                            {sources.map((code) => (
                                <option key={code} value={code}>
                                    {code}
                                </option>
                            ))}
                        </Select>
                    </div>
                    <p className="text-xs text-ink-faint">
                        {t('marketdata.fixingObservations.emptyCount', {
                            count: empty,
                            total: series.length,
                        })}
                    </p>
                </div>
                {query.isError ? (
                    <Notice tone="error">{query.error.message}</Notice>
                ) : query.isPending ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : series.length === 0 ? (
                    <Notice tone="warn">{t('marketdata.fixingObservations.none')}</Notice>
                ) : (
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-b border-line-subtle text-left text-xs uppercase text-ink-faint">
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.series')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.assetClass')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.underlying')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.source')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.provider')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.latestValue')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.latestDay')}
                                    </th>
                                    <th className="px-3 py-2">
                                        {t('marketdata.fixingObservations.columns.days')}
                                    </th>
                                    <th className="px-3 py-2" />
                                </tr>
                            </thead>
                            <tbody>
                                {series.map((one) => (
                                    <SeriesRow key={one.series_id} series={one} />
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}
                <div className="space-y-1 text-xs text-ink-faint">
                    <p>{t('marketdata.fixingObservations.legendNoDay')}</p>
                    <p>{t('marketdata.fixingObservations.legendEnded')}</p>
                </div>
            </section>
        </div>
    );
}
