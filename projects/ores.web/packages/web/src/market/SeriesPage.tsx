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
 *
 */

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { DEFAULT_OBSERVATIONS, type ListedSeries } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { LineChart, SERIES_COLOURS } from '../marketdata/LineChart.js';
import { evolutionRoute, seriesOfUrl, seriesVerdict } from '../oresmd/identity.js';
import { oresmdQueries } from '../oresmd/queries.js';
import { useSeriesTitle } from '../oresmd/useSeriesTitle.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Notice, PageHeader } from '../ui/Primitives.js';
import { instantLabel, latestFirst, pointsOf } from './seriesView.js';

/** How many observations the page reads, and how many rows of them it lists. */
const READ = DEFAULT_OBSERVATIONS;
const LISTED = 20;

/**
 * The page of a series that has one value at each instant, such as an FX spot
 * rate: the values over time as a line, and the latest of them in a table.
 *
 * An oresmd URL in the address, as `uri`, names the series. The page reads it
 * through the server's parse and finds the series among the ones the party can
 * see. A series the party does not have, a fixing and a URL the codec refuses are
 * said in words. A series with an axis has the Evolution page, and the page says so
 * and links to it.
 */
export function SeriesPage({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    const titled = useSeriesTitle();
    const [search] = useSearchParams();
    const uri = search.get('uri') ?? '';
    const parsed = useQuery({ ...oresmdQueries.parse(uri), enabled: uri !== '' });
    const schema = useQuery({ ...oresmdQueries.schema(), enabled: uri !== '' });
    const list = useQuery({ ...oresmdQueries.seriesList(partyId), enabled: uri !== '' });

    const named =
        parsed.data === undefined || schema.data === undefined
            ? undefined
            : seriesOfUrl(schema.data, parsed.data);
    const verdict =
        named?.ok === true && list.data !== undefined
            ? seriesVerdict(list.data, named.form)
            : undefined;
    const listed = verdict?.kind === 'no-shape' ? verdict.listed : undefined;

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="marketdata" screen={t('market.series.title')} />
                <PageHeader
                    title={t('market.series.title')}
                    description={t('market.series.description')}
                />
            </div>
            {uri === '' ? (
                <Notice>{t('market.series.noUri')}</Notice>
            ) : parsed.isError || schema.isError || list.isError ? (
                <Notice tone="error">
                    {(parsed.error ?? schema.error ?? list.error)?.message ?? ''}
                </Notice>
            ) : named === undefined || list.data === undefined ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : !named.ok ? (
                <Notice tone="warn">
                    {named.reason === 'fixing'
                        ? t('market.evolution.errors.fixing')
                        : named.reason === 'unknown'
                          ? t('market.evolution.errors.unknown', { name: named.message })
                          : named.message}
                </Notice>
            ) : verdict?.kind === 'not-listed' ? (
                <Notice tone="warn">{t('market.evolution.errors.notListed')}</Notice>
            ) : verdict?.kind === 'shown' ? (
                <Notice>
                    {t('market.series.hasShape', { series: titled(verdict.listed.identity) })}{' '}
                    <Link className="text-accent-bright hover:underline" to={evolutionRoute(uri)}>
                        {t('market.series.openEvolution')}
                    </Link>
                </Notice>
            ) : listed !== undefined ? (
                <Observations series={listed} uri={uri} />
            ) : null}
        </div>
    );
}

function Observations({
    series,
    uri,
}: {
    readonly series: ListedSeries;
    readonly uri: string;
}): ReactNode {
    const { t } = useTranslation();
    const titled = useSeriesTitle();
    const read = useQuery(oresmdQueries.observations(series.series_id, READ));
    if (read.isError) return <Notice tone="error">{read.error.message}</Notice>;
    if (read.isPending) return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    const { observations, total } = read.data;
    const points = pointsOf(observations);
    return (
        <section className="card space-y-4 p-6">
            <div>
                <h2 className="text-lg font-medium">{titled(series.identity)}</h2>
                <p className="break-all font-mono text-xs text-ink-faint">{uri}</p>
                <p className="text-sm text-ink-muted">
                    {observations.length === 0
                        ? t('market.series.empty')
                        : t('market.series.showing', { shown: observations.length, total })}
                </p>
            </div>
            {points.length >= 2 && (
                <LineChart
                    price
                    label={t('market.series.chart')}
                    xs={points.map((point) => point.at)}
                    xFormat={instantLabel}
                    format={(value) => String(Number(value.toPrecision(7)))}
                    series={[
                        {
                            points: points.map((point) => point.value),
                            colour: SERIES_COLOURS[0],
                            strong: true,
                            fill: true,
                        },
                    ]}
                />
            )}
            {observations.length > 0 && (
                <table className="w-full text-sm">
                    <thead>
                        <tr className="border-b border-line text-left text-xs text-ink-faint">
                            <th className="py-1.5 font-normal">{t('market.series.at')}</th>
                            <th className="py-1.5 text-right font-normal">
                                {t('market.series.value')}
                            </th>
                            <th className="py-1.5 pl-6 font-normal">{t('market.series.source')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {latestFirst(observations, LISTED).map((row) => (
                            <tr
                                key={`${row.at}|${row.source}`}
                                className="border-b border-line-subtle"
                            >
                                <td className="py-1.5 font-mono text-xs">{row.at}</td>
                                <td className="py-1.5 text-right font-mono text-xs">{row.value}</td>
                                <td className="py-1.5 pl-6 text-ink-muted">{row.source}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}
