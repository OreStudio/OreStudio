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
import { Link, useParams, useSearchParams } from 'react-router';
import {
    orderTimeline,
    recordEvents,
    type TimelineEvent,
} from '@ores/wire-protocol/browser';
import { api, type PublishedCurveSet } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';

import { Timeline } from '../timeline/Timeline.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { formatDateTime } from '../ui/Time.js';
import { PublishedCompareTab } from './PublishedCompareTab.js';
import { PublishedPointsTab } from './PublishedPointsTab.js';
import {
    DEFAULT_OVERLAY_SETS,
    hasEnded,
    lastChangedAt,
    newestPublished,
    publishedSetsOf,
    stateOf,
    type SetState,
} from './publishedCurves.js';

/**
 * One published curve's record, from
 * doc/prototypes/market_data/published-curves.org.
 *
 * The record is a curve and a party, and the set it draws is the newest one the
 * curve holds: the set in use where there is one, and the newest ended set where
 * the set in use has ended. The list's ended-set filter is how a person reaches a
 * record whose newest set has ended.
 *
 * The four tabs are the standard's Details and History plus the kind's own two
 * views: the last published sets overlaid, so a move between publishes is a
 * shape, and the comparison against the candidate, so a decider reads what the
 * candidate changes point by point. The tabs are the only way between them, and
 * the address carries the tab and the as-of, so a comparison reopens where it
 * was left.
 *
 * The record is read only. Nobody may write a publish record: the gate that
 * applies an approved request writes it, so the page offers no Revert on any
 * version and says why once, under the history.
 */

/** The entity type the publish record's history is read by. */
export const PUBLISH_ENTITY = 'ores.marketdata.curve_publish';

const TABS = ['details', 'points', 'compare', 'history'] as const;

function toneOf(state: SetState): 'up' | 'accent' | 'muted' | 'neutral' {
    if (state === 'published') return 'up';
    if (state === 'candidate') return 'accent';
    if (state === 'withdrawn') return 'muted';
    return 'neutral';
}

export function PublishedCurvePage(): ReactNode {
    const { t, language } = useTranslation();
    const { curve = '' } = useParams();
    const [search] = useSearchParams();
    const party = search.get('party') ?? '';
    const { tab, bar } = useTabs({
        label: t('marketdata.published.tabs'),
        tabs: [...TABS],
        titleOf: (candidate) => t(`marketdata.published.tab.${candidate}`),
    });

    // The list is the index of the curves: the record is resolved by the name it
    // is opened with, and every state is read so an ended set resolves too.
    const matches = useQuery({
        queryKey: ['published-curves', 'record', curve, party],
        queryFn: () =>
            api.publishedCurves({
                search: curve,
                includeCandidates: true,
                includeEnded: true,
                limit: 200,
            }),
    });

    const rows = (matches.data?.sets ?? []).filter(
        (row) => row.curve === curve && (party === '' || row.party === party),
    );
    const seriesId = rows[0]?.series_id ?? '';
    const count = Number(search.get('count') ?? DEFAULT_OVERLAY_SETS);
    const sets = useQuery({
        queryKey: ['published-curve-sets', seriesId, count],
        queryFn: () =>
            api.publishedCurveSets({
                seriesId,
                limit: Number.isFinite(count) && count > 0 ? count : DEFAULT_OVERLAY_SETS,
            }),
        enabled: seriesId !== '',
    });

    if (matches.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (matches.isError) {
        return <Notice tone="error">{matches.error.message}</Notice>;
    }
    if (rows.length === 0) {
        return (
            <div className="space-y-6">
                <AreaTrail area="marketdata" />
                <PageHeader title={curve} description={party} />
                <Notice tone="warn">
                    {t('marketdata.published.noSuchCurve', { curve })}
                </Notice>
            </div>
        );
    }

    const loaded = sets.data?.sets ?? [];
    const record = newestPublished(loaded);
    if (record === undefined) {
        return (
            <div className="space-y-6">
                <AreaTrail area="marketdata" />
                <PageHeader title={curve} description={party} />
                <Notice tone="info">{t('marketdata.published.noSet', { curve })}</Notice>
            </div>
        );
    }

    const state = stateOf(record);
    const ended = hasEnded(record);
    const lead = [
        record.party,
        t('marketdata.published.asOfLead', { at: formatDateTime(record.as_of, language) }),
        ended
            ? t('marketdata.published.endedLead', { at: relativeOrExact(record, language) })
            : record.published_at === null
              ? t('marketdata.published.waitingLead')
              : t('marketdata.published.publishedLead', {
                    at: relativeOrExact(record, language),
                }),
    ].join(' · ');

    const heading = (
        <div>
            <AreaTrail area="marketdata" />
            <PageHeader
                title={curve}
                description={lead}
                actions={
                    <RefreshQueries
                        keys={[
                            ['published-curves', 'record', curve, party],
                            ['published-curve-sets', seriesId, count],
                            ['history', PUBLISH_ENTITY, record.publish_id],
                            ['request', record.request_id],
                        ]}
                    />
                }
            />
            <div className="mb-4 flex flex-wrap items-center gap-2">
                <Tag tone={toneOf(state)}>{t(`marketdata.published.state.${state}`)}</Tag>
            </div>
            {bar}
        </div>
    );

    return (
        <div className="space-y-4">
            {heading}
            {tab === 'details' && <DetailsTab record={record} sets={loaded} />}
            {tab === 'points' && (
                <PublishedPointsTab
                    sets={loaded}
                    publishedCount={sets.data?.publishedCount ?? publishedSetsOf(loaded).length}
                />
            )}
            {tab === 'compare' && <PublishedCompareTab sets={loaded} />}
            {tab === 'history' && <HistoryTab record={record} />}
            <p className="text-xs text-ink-faint">{t('marketdata.published.readOnly')}</p>
        </div>
    );
}

/** The instant a set last moved, in the reader's language. */
function relativeOrExact(set: PublishedCurveSet, language: string): string {
    return formatDateTime(lastChangedAt(set) ?? set.as_of, language);
}

/** The subject the request authorised, the act that published it, and its state. */
function DetailsTab({
    record,
    sets,
}: {
    readonly record: PublishedCurveSet;
    readonly sets: readonly PublishedCurveSet[];
}): ReactNode {
    const { t, language } = useTranslation();
    const state = stateOf(record);
    return (
        <div className="space-y-4">
            <section className="rounded-md border border-line p-4">
                <h2 className="mb-3 text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.published.details.subject')}
                </h2>
                <dl className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                    <Fact label={t('marketdata.published.columns.curve')}>{record.curve}</Fact>
                    <Fact label={t('marketdata.published.columns.party')}>{record.party}</Fact>
                    <Fact label={t('marketdata.published.details.asOf')}>
                        {formatDateTime(record.as_of, language)}
                    </Fact>
                    <Fact label={t('marketdata.published.details.series')}>
                        <Link
                            className="break-all text-accent hover:underline"
                            to={`/market-data/evolution?uri=${encodeURIComponent(record.series_uri)}`}
                        >
                            {record.series_uri}
                        </Link>
                    </Fact>
                    <Fact label={t('marketdata.published.details.config')}>
                        <span className="break-all">{record.bootstrap_config_id}</span>
                    </Fact>
                    <Fact label={t('marketdata.published.columns.state')}>
                        {t(`marketdata.published.state.${state}`)}
                    </Fact>
                </dl>
            </section>

            <section className="rounded-md border border-line p-4">
                <h2 className="mb-3 text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.published.details.act')}
                </h2>
                <dl className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                    <Fact label={t('marketdata.published.details.publisher')}>
                        {record.published_by === '' ? '—' : record.published_by}
                    </Fact>
                    <Fact label={t('marketdata.published.details.publishedAt')}>
                        {record.published_at === null
                            ? '—'
                            : formatDateTime(record.published_at, language)}
                    </Fact>
                    <Fact label={t('marketdata.published.details.request')}>
                        <Link
                            className="text-accent hover:underline"
                            to={`/requests/${encodeURIComponent(record.request_id)}`}
                        >
                            {record.request_id}
                        </Link>
                    </Fact>
                    <Fact label={t('marketdata.published.columns.points')}>
                        {String(record.point_count)}
                    </Fact>
                </dl>
            </section>

            {hasEnded(record) && (
                <section className="rounded-md border border-warn/40 bg-warn/5 p-4">
                    <h2 className="mb-3 text-xs font-semibold tracking-wide uppercase">
                        {t('marketdata.published.details.end')}
                    </h2>
                    <dl className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                        {record.withdrawn_at !== null ? (
                            <>
                                <Fact label={t('marketdata.published.details.endedAt')}>
                                    {formatDateTime(record.withdrawn_at, language)}
                                </Fact>
                                <Fact label={t('marketdata.published.details.endedBy')}>
                                    {record.withdrawn_by === '' ? '—' : record.withdrawn_by}
                                </Fact>
                                <Fact label={t('marketdata.published.details.reason')}>
                                    {record.withdrawn_reason_code === ''
                                        ? '—'
                                        : record.withdrawn_reason_code}
                                </Fact>
                                <Fact label={t('marketdata.published.details.endRequest')}>
                                    {record.withdrawal_request_id === '' ? (
                                        t('marketdata.published.details.noRequest')
                                    ) : (
                                        <Link
                                            className="text-accent hover:underline"
                                            to={`/requests/${encodeURIComponent(record.withdrawal_request_id)}`}
                                        >
                                            {record.withdrawal_request_id}
                                        </Link>
                                    )}
                                </Fact>
                            </>
                        ) : (
                            <>
                                <Fact label={t('marketdata.published.details.supersededAt')}>
                                    {record.superseded_at === null
                                        ? '—'
                                        : formatDateTime(record.superseded_at, language)}
                                </Fact>
                                <Fact label={t('marketdata.published.details.successorRequest')}>
                                    {record.superseded_by_request_id === '' ? (
                                        '—'
                                    ) : (
                                        <Link
                                            className="text-accent hover:underline"
                                            to={`/requests/${encodeURIComponent(record.superseded_by_request_id)}`}
                                        >
                                            {record.superseded_by_request_id}
                                        </Link>
                                    )}
                                </Fact>
                                <Fact label={t('marketdata.published.details.successorAsOf')}>
                                    {record.superseded_as_of === null
                                        ? '—'
                                        : formatDateTime(record.superseded_as_of, language)}
                                </Fact>
                            </>
                        )}
                    </dl>
                </section>
            )}

            <p className="text-sm text-ink-muted">
                {t('marketdata.published.details.lastChanged', {
                    at: lastChangedAt(record) === null ? '—' : lastChangedAt(record) ?? '',
                    count: String(publishedSetsOf(sets).length),
                })}
            </p>
            <p className="text-sm text-ink-muted">{t('marketdata.published.details.readOnly')}</p>
        </div>
    );
}

/**
 * The record's story: its own versions, and the decision that published it.
 *
 * The versions are the publish record's, read by its own history provider. The
 * decision belongs to the request and not to this record, so it is read by the
 * request's id and named as the request's entry rather than as a version of this
 * one. There is no Revert: nobody may write a publish record, so a correction is
 * a new publish or a withdrawal, and each is an act of its own with a request
 * behind it.
 */
function HistoryTab({ record }: { readonly record: PublishedCurveSet }): ReactNode {
    const { t } = useTranslation();
    const versions = useQuery({
        queryKey: ['history', PUBLISH_ENTITY, record.publish_id],
        queryFn: () => api.history(PUBLISH_ENTITY, record.publish_id),
    });
    const request = useQuery({
        queryKey: ['request', record.request_id],
        queryFn: () => api.request(record.request_id),
        enabled: record.request_id !== '',
    });

    if (versions.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (versions.isError) {
        return <Notice tone="error">{versions.error.message}</Notice>;
    }

    const events: TimelineEvent[] = [
        ...recordEvents(PUBLISH_ENTITY, record.publish_id, versions.data ?? []),
    ];
    const decided = request.data?.decision ?? null;
    // The stream states its gaps: a request that nobody answers can have no
    // decision, and an answered one whose decision this reader may not read is a
    // gap and not a silence.
    const decisionHidden =
        request.data !== null &&
        request.data !== undefined &&
        decided === null &&
        request.data.stateCode !== 'waiting' &&
        request.data.stateCode !== 'held';
    if (decided !== null && decided.decidedAt !== '') {
        events.push({
            entityType: 'ores.inbox.approval_request',
            entityId: record.request_id,
            kind: 'decided',
            at: decided.decidedAt,
            actor: decided.decidedBy,
            version: 0,
            reasonCode: decided.decisionCode,
            commentary: decided.comment,
            fields: [],
        });
    }

    return (
        <div className="space-y-3">
            <Timeline
                timeline={{
                    subject: 'record',
                    id: record.publish_id,
                    events: orderTimeline(events),
                    gaps: [
                        ...(request.isError && record.request_id !== ''
                            ? [
                                  {
                                      entity: 'ores.inbox.approval_request',
                                      reason: request.error.message,
                                  },
                              ]
                            : []),
                        ...(decisionHidden
                            ? [
                                  {
                                      entity: t('marketdata.published.history.decision'),
                                      reason: t('marketdata.published.history.decisionHidden'),
                                  },
                              ]
                            : []),
                    ],
                }}
                recordName={(event) =>
                    event.entityType === 'ores.inbox.approval_request'
                        ? t('marketdata.published.history.decision')
                        : undefined
                }
            />
            <p className="text-sm text-ink-muted">{t('marketdata.published.history.noRevert')}</p>
        </div>
    );
}

function Fact({ label, children }: { readonly label: string; readonly children: ReactNode }): ReactNode {
    return (
        <div>
            <dt className="text-[10px] tracking-wide text-ink-faint uppercase">{label}</dt>
            <dd className="font-mono text-sm text-ink">{children}</dd>
        </div>
    );
}
