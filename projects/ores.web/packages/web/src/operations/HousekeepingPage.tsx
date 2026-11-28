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

/**
 * Housekeeping, the list: every store the installation keeps, in one table, with
 * the bound that governs it and the size it holds.
 *
 * It is variant A of the signed-off prototype
 * (doc/prototypes/operations/housekeeping/index.html?variant=A): the only shape
 * that holds every store and every number at once, and the shape the logs screen
 * already uses. The verdict of boundedness comes first because it is the only
 * column with a wrong answer, and the stores nothing bounds sort to the top,
 * because they are the work.
 */

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { PATHS, housekeepingStorePath } from '../shell/paths.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Metric } from '../ui/Metric.js';
import { Button, LinkButton, Notice, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import {
    arrange,
    formatSize,
    measureUnit,
    summarise,
    verdictOf,
    type StoreFilter,
    type StoreOrder,
} from './housekeeping.js';

/** The list's read: every store, once, as the bounds view states them. */
export const HOUSEKEEPING_STORES_QUERY_KEY = ['operations', 'housekeeping', 'stores'] as const;

export function useHousekeepingStores() {
    return useQuery({
        queryKey: HOUSEKEEPING_STORES_QUERY_KEY,
        queryFn: api.housekeepingStores,
    });
}

/** The name of a target shape, in the reader's language. */
export function targetKindLabel(kind: string): string {
    switch (kind) {
        case 'chunked_table':
            return 'chunkedTable';
        case 'table':
            return 'table';
        case 'object_prefix':
            return 'objectPrefix';
        case 'directory':
            return 'directory';
        default:
            return kind;
    }
}

/** A target shape in the reader's language, or its own code when nobody has named it. */
export function targetKindName(t: (key: string) => string, kind: string): string {
    const key = `operations.housekeeping.targetKinds.${targetKindLabel(kind)}`;
    const name = t(key);
    return name === key ? kind : name;
}

/** A bound's action in the reader's language, or its own code when nobody has named it. */
export function actionLabel(t: (key: string) => string, action: string): string {
    const key = `operations.housekeeping.actions.${action}`;
    const name = t(key);
    return name === key ? action : name;
}

/**
 * What the verdicts and the mechanisms mean, said in the screen's own words.
 *
 * The list states "Bounded by keying" and "the sweep cannot perform its action"
 * because those are the read's answers, and a person who has not read this week's
 * design does not know what either means. The explanation sits once, folded away,
 * rather than beside every row.
 */
function BoundLegend(): ReactNode {
    const { t } = useTranslation();
    const verdicts = ['bounded', 'keyed', 'unbounded'] as const;
    const mechanisms = ['sweep', 'node_release', 'content_keying', 'policy_off', 'none'] as const;
    return (
        <details className="card p-4 text-sm">
            <summary className="cursor-pointer text-ink-muted">
                {t('operations.housekeeping.legend.summary')}
            </summary>
            <div className="mt-3 grid gap-6 sm:grid-cols-2">
                <div className="space-y-2">
                    <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                        {t('operations.housekeeping.legend.verdicts')}
                    </h3>
                    <dl className="space-y-2">
                        {verdicts.map((verdict) => (
                            <div key={verdict}>
                                <dt className="font-medium">
                                    {t(`operations.housekeeping.verdict.${verdict}`)}
                                </dt>
                                <dd className="text-ink-muted">
                                    {t(`operations.housekeeping.legend.${verdict}`)}
                                </dd>
                            </div>
                        ))}
                    </dl>
                </div>
                <div className="space-y-2">
                    <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                        {t('operations.housekeeping.legend.mechanisms')}
                    </h3>
                    <dl className="space-y-2">
                        {mechanisms.map((mechanism) => (
                            <div key={mechanism}>
                                <dt className="font-medium">
                                    {t(
                                        `operations.housekeeping.legend.mechanismNames.${mechanism}`,
                                    )}
                                </dt>
                                <dd className="text-ink-muted">
                                    {t(`operations.housekeeping.mechanisms.${mechanism}`)}
                                </dd>
                            </div>
                        ))}
                    </dl>
                </div>
            </div>
        </details>
    );
}

/** The tag a verdict wears: green where something bounds the store, red where nothing does. */
function VerdictTag({
    verdict,
}: {
    readonly verdict: 'bounded' | 'keyed' | 'unbounded';
}): ReactNode {
    const { t } = useTranslation();
    const tone = verdict === 'unbounded' ? 'down' : 'up';
    return <Tag tone={tone}>{t(`operations.housekeeping.verdict.${verdict}`)}</Tag>;
}

export function HousekeepingPage(): ReactNode {
    const { t } = useTranslation();
    const [filter, setFilter] = useState<StoreFilter>('all');
    const [order, setOrder] = useState<StoreOrder>('attention');
    const stores = useHousekeepingStores();
    /*
     * The database's own size, so the summary can state what the catalogued
     * stores are a part of. It is a second read, and its failure leaves the figure
     * unstated rather than the page unread.
     */
    const capacity = useQuery({
        queryKey: ['operations', 'housekeeping', 'capacity', '24h'],
        queryFn: () => api.housekeepingCapacity('24h'),
    });
    const databaseBytes =
        capacity.data === undefined || capacity.data.samples.length === 0
            ? null
            : (capacity.data.samples[capacity.data.samples.length - 1]?.database_bytes ?? null);
    const trail = <AreaTrail area="operations" screen={t('operations.screens.housekeeping')} />;

    if (stores.isPending || stores.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.housekeeping.title')}
                description={t('operations.housekeeping.description')}
            >
                {stores.isError ? (
                    <Notice tone="error">{stores.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const rows = stores.data.rows;
    const figures = summarise(rows);
    const shown = arrange(rows, filter, order);

    return (
        <ScreenFrame
            trail={trail}
            title={t('operations.housekeeping.title')}
            description={t('operations.housekeeping.description')}
        >
            <section className="card grid gap-4 p-4 sm:grid-cols-3 lg:grid-cols-6">
                <Metric
                    label={t('operations.housekeeping.summary.stores')}
                    value={String(figures.stores)}
                />
                <Metric
                    label={t('operations.housekeeping.summary.bounded')}
                    value={String(figures.bounded)}
                    tone={figures.bounded > 0 ? 'good' : 'neutral'}
                />
                <Metric
                    label={t('operations.housekeeping.summary.unbounded')}
                    value={String(figures.unbounded)}
                    tone={figures.unbounded > 0 ? 'bad' : 'good'}
                    {...(figures.unbounded > 0
                        ? { note: t('operations.housekeeping.summary.unboundedNote') }
                        : {})}
                />
                <Metric
                    label={t('operations.housekeeping.summary.boundedSize')}
                    value={formatSize(figures.bytes)}
                />
                <Metric
                    label={t('operations.housekeeping.summary.database')}
                    value={databaseBytes === null ? '—' : formatSize(databaseBytes)}
                    {...(databaseBytes === null
                        ? {}
                        : {
                              note: t('operations.housekeeping.summary.databaseShare', {
                                  share: ((figures.bytes / databaseBytes) * 100).toFixed(0),
                              }),
                          })}
                />
                <Metric
                    label={t('operations.housekeeping.summary.unmeasured')}
                    value={String(figures.unmeasured)}
                    tone={figures.unmeasured > 0 ? 'warn' : 'neutral'}
                />
            </section>

            <div className="flex flex-wrap items-end justify-between gap-3">
                <div className="flex flex-wrap gap-2">
                    {(['all', 'unbounded'] as const).map((candidate) => (
                        <Button
                            key={candidate}
                            size="sm"
                            variant={filter === candidate ? 'primary' : 'secondary'}
                            onClick={() => setFilter(candidate)}
                        >
                            {candidate === 'all'
                                ? t('operations.housekeeping.filter.all')
                                : t('operations.housekeeping.filter.unbounded', {
                                      count: String(figures.unbounded),
                                  })}
                        </Button>
                    ))}
                </div>
                <div className="flex items-center gap-3">
                    <LinkButton to={PATHS.housekeepingTables} size="sm">
                        {t('operations.housekeeping.seeTables')}
                    </LinkButton>
                    <label className="flex items-center gap-2 text-xs text-ink-muted">
                        {t('operations.housekeeping.order.label')}
                        <select
                            className="cursor-pointer rounded-md border border-line bg-surface-base px-2 py-1 text-sm"
                            value={order}
                            onChange={(event) => setOrder(event.target.value as StoreOrder)}
                        >
                            {(['attention', 'size', 'name'] as const).map((candidate) => (
                                <option key={candidate} value={candidate}>
                                    {t(`operations.housekeeping.order.${candidate}`)}
                                </option>
                            ))}
                        </select>
                    </label>
                </div>
            </div>

            {shown.length === 0 ? (
                <Notice tone={filter === 'unbounded' ? 'info' : 'warn'}>
                    {filter === 'unbounded'
                        ? t('operations.housekeeping.emptyUnbounded')
                        : t('operations.housekeeping.emptyAll')}
                </Notice>
            ) : (
                <section className="card p-4">
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('operations.housekeeping.columns.store')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('operations.housekeeping.columns.bound')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('operations.housekeeping.columns.boundBy')}
                                </th>
                                <th className="py-1 text-right font-normal">
                                    {t('operations.housekeeping.columns.size')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('operations.housekeeping.columns.lastRun')}
                                </th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {shown.map((store) => {
                                const verdict = verdictOf(store);
                                const unit = measureUnit(store.measure_kind);
                                return (
                                    <tr
                                        key={store.code}
                                        className="align-top hover:bg-surface-hover"
                                    >
                                        <td className="py-2 pr-3">
                                            <Link
                                                to={housekeepingStorePath(store.code)}
                                                className="text-accent hover:text-accent-bright"
                                            >
                                                {store.display_name}
                                            </Link>
                                            <div className="font-mono text-xs text-ink-faint">
                                                {store.target}
                                            </div>
                                            <div className="text-xs text-ink-faint">
                                                {targetKindName(t, store.target_kind)}
                                            </div>
                                        </td>
                                        <td className="py-2 pr-3">
                                            <VerdictTag verdict={verdict.verdict} />
                                            {verdict.reason !== null && (
                                                <div className="text-xs text-ink-muted">
                                                    {t(
                                                        `operations.housekeeping.reason.${verdict.reason}`,
                                                    )}
                                                </div>
                                            )}
                                        </td>
                                        <td className="py-2 pr-3 text-ink-muted">
                                            {store.bound.action === null ? (
                                                <span className="text-ink-faint">
                                                    {t('operations.housekeeping.row.noAction')}
                                                </span>
                                            ) : (
                                                <>
                                                    <div>{actionLabel(t, store.bound.action)}</div>
                                                    {store.bound.retention_days !== null && (
                                                        <div className="text-xs text-ink-faint">
                                                            {t('operations.housekeeping.period', {
                                                                days: String(
                                                                    store.bound.retention_days,
                                                                ),
                                                            })}
                                                        </div>
                                                    )}
                                                    <div className="font-mono text-xs text-ink-faint">
                                                        {store.bound.policy_code ??
                                                            t(
                                                                'operations.housekeeping.row.noPolicy',
                                                            )}
                                                    </div>
                                                </>
                                            )}
                                        </td>
                                        <td className="py-2 pr-3 text-right tabular-nums">
                                            {store.size.sampled_at === null ? (
                                                <span className="text-ink-faint">
                                                    {t('operations.housekeeping.row.notMeasured')}
                                                </span>
                                            ) : (
                                                <>
                                                    <div>{formatSize(store.size.size_bytes)}</div>
                                                    <div className="text-xs text-ink-faint">
                                                        {t('operations.housekeeping.measure', {
                                                            count: store.size.measure_count.toLocaleString(),
                                                            unit: t(
                                                                `operations.housekeeping.units.${unit}`,
                                                            ),
                                                        })}
                                                    </div>
                                                </>
                                            )}
                                        </td>
                                        <td className="py-2 text-ink-muted">
                                            {store.last_run.outcome === null ? (
                                                <span className="text-ink-faint">
                                                    {t('operations.housekeeping.row.neverSwept')}
                                                </span>
                                            ) : (
                                                <>
                                                    <div className="flex items-center gap-2">
                                                        <Tag
                                                            tone={
                                                                store.last_run.outcome === 'failed'
                                                                    ? 'down'
                                                                    : store.last_run.outcome ===
                                                                        'skipped'
                                                                      ? 'warn'
                                                                      : 'neutral'
                                                            }
                                                        >
                                                            {t(
                                                                `operations.housekeeping.outcomes.${store.last_run.outcome}`,
                                                            )}
                                                        </Tag>
                                                        {store.last_run.ran_at !== null && (
                                                            <RelativeTime
                                                                at={store.last_run.ran_at}
                                                            />
                                                        )}
                                                    </div>
                                                    <div className="text-xs text-ink-faint">
                                                        {t('operations.housekeeping.row.removed', {
                                                            count: store.last_run.removed.toLocaleString(),
                                                        })}
                                                    </div>
                                                </>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </section>
            )}

            <BoundLegend />

            <p className="text-xs text-ink-faint">{t('operations.housekeeping.footnote')}</p>
        </ScreenFrame>
    );
}
