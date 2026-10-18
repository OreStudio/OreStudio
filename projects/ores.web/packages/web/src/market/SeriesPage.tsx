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

import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { DEFAULT_OBSERVATIONS, type ListedSeries } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { LineChart, SERIES_COLOURS } from '../marketdata/LineChart.js';
import { evolutionIdRoute, evolutionRoute } from '../oresmd/identity.js';
import { oresmdQueries } from '../oresmd/queries.js';
import { useSeriesTitle } from '../oresmd/useSeriesTitle.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Notice, PageHeader } from '../ui/Primitives.js';
import { seriesPageState, type SeriesPageState, type SeriesRead } from './seriesState.js';
import {
    formatText,
    formatValue,
    instantLabel,
    isSpotRate,
    latestFirst,
    placesOf,
    pointsOf,
} from './seriesView.js';

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
 *
 * A series id in the address, as `series`, names a series the party already holds, so
 * a screen that knows the id links to it without composing a URL.
 */
export function SeriesPage({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    const [search] = useSearchParams();
    const uri = search.get('uri') ?? '';
    const seriesId = search.get('series') ?? '';
    const parsed = useQuery({ ...oresmdQueries.parse(uri), enabled: uri !== '' });
    const schema = useQuery({ ...oresmdQueries.schema(), enabled: uri !== '' });
    const list = useQuery({
        ...oresmdQueries.seriesList(partyId),
        enabled: uri !== '' || seriesId !== '',
    });
    const state = seriesPageState({
        uri,
        seriesId,
        parsed: answerOf(parsed),
        schema: answerOf(schema),
        list: answerOf(list),
    });

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="marketdata" screen={t('market.series.title')} />
                <PageHeader
                    title={t('market.series.title')}
                    description={t('market.series.description')}
                />
            </div>
            <Situation state={state} uri={uri} seriesId={seriesId} />
        </div>
    );
}

/** One read of the page as the state function takes it. A failed read is read first. */
function answerOf<T>(query: UseQueryResult<T, Error>): SeriesRead<T> {
    if (query.isError) {
        return { kind: 'error', message: query.error.message };
    }
    return query.data === undefined ? { kind: 'waiting' } : { kind: 'answer', data: query.data };
}

/** The words and the drawing of the one situation the page is in. */
function Situation({
    state,
    uri,
    seriesId,
}: {
    readonly state: SeriesPageState;
    readonly uri: string;
    readonly seriesId: string;
}): ReactNode {
    const { t } = useTranslation();
    const titled = useSeriesTitle();
    switch (state.kind) {
        case 'no-uri':
            return <Notice>{t('market.series.noUri')}</Notice>;
        case 'error':
            return <Notice tone="error">{state.message}</Notice>;
        case 'loading':
            return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
        case 'refused':
            return <Notice tone="warn">{state.message}</Notice>;
        case 'fixing':
            return <Notice tone="warn">{t('market.evolution.errors.fixing')}</Notice>;
        case 'unknown':
            return (
                <Notice tone="warn">
                    {t('market.evolution.errors.unknown', { name: state.name })}
                </Notice>
            );
        case 'not-listed':
            return <Notice tone="warn">{t('market.evolution.errors.notListed')}</Notice>;
        case 'has-shape':
            return (
                <Notice>
                    {t('market.series.hasShape', { series: titled(state.listed.identity) })}{' '}
                    <Link
                        className="text-accent-bright hover:underline"
                        to={seriesId === '' ? evolutionRoute(uri) : evolutionIdRoute(seriesId)}
                    >
                        {t('market.series.openEvolution')}
                    </Link>
                </Notice>
            );
        case 'values':
            return <Observations series={state.listed} uri={uri} />;
    }
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
    const conventions = useQuery(oresmdQueries.pairConventions());
    const places = placesOf(series.identity, conventions.data ?? []);
    if (read.isError) return <Notice tone="error">{read.error.message}</Notice>;
    // A spot rate waits for its convention, so the values do not change format as it loads.
    if (read.isPending || (isSpotRate(series.identity) && conventions.isPending))
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    const { observations, total } = read.data;
    const points = pointsOf(observations);
    return (
        <section className="card space-y-4 p-6">
            <div>
                <h2 className="text-lg font-medium">{titled(series.identity)}</h2>
                {uri !== '' && <p className="break-all font-mono text-xs text-ink-faint">{uri}</p>}
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
                    format={(value) => formatValue(value, places)}
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
                                <td className="py-1.5 text-right font-mono text-xs">
                                    {formatText(row.value, places)}
                                </td>
                                <td className="py-1.5 pl-6 text-ink-muted">{row.source}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}
