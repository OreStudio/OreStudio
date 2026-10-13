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
 * Who uses the grid.
 *
 * One table with a row for each group of work. The server decides the group
 * from the session: an operator in the system tenant reads every tenant and
 * sees one row for each, and a tenant's administrator reads the tenant and sees
 * one row for each party. The screen draws what it is given and states no scope
 * of its own.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import type { GridUsageGroup, GridUsageView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { SharePie } from '../ui/SharePie.js';
import { QUEUE_CAUTION_MINUTES, QUEUE_WAIT_LIMIT_MINUTES, formatWait } from './GridSeries.js';

/** Where the usage answer is cached, so a test can seed it. */
export const GRID_USAGE_QUERY_KEY = ['operations-grid-usage'] as const;

/** A fraction as a whole percentage, or a dash where the group ran nothing. */
export function formatShare(share: number): string {
    if (share <= 0) {
        return '—';
    }
    const percent = share * 100;
    return percent < 1 ? '<1%' : `${Math.round(percent)}%`;
}

/** What a person reads for a group: its name, else a no-party label, else its id. */
export function groupLabel(
    group: GridUsageGroup,
    unassigned: string,
): { readonly text: string; readonly isId: boolean } {
    if (group.name !== null && group.name !== '') {
        return { text: group.name, isId: false };
    }
    if (group.key === '') {
        return { text: unassigned, isId: false };
    }
    return { text: group.key, isId: true };
}

/** How long the oldest waiting job has waited, or a dash when none waits. */
export function formatOldest(seconds: number): string {
    return seconds <= 0 ? '—' : formatWait(seconds / 60);
}

/**
 * The warning that rests on the age of the oldest waiting job: a job that waits
 * past the limit loses its place, so the age, and not an estimate, says it.
 */
export function QueueAgeNotice({ usage }: { readonly usage: GridUsageView }): ReactNode {
    const { t } = useTranslation();
    const oldest = Math.max(
        0,
        ...usage.groups
            .filter((group) => !group.unavailable)
            .map((group) => group.oldest_unsent_seconds),
    );
    const minutes = oldest / 60;
    if (minutes < QUEUE_CAUTION_MINUTES) {
        return null;
    }
    const over = minutes >= QUEUE_WAIT_LIMIT_MINUTES;
    return (
        <Notice tone={over ? 'error' : 'warn'}>
            {t(`operations.grid.usage.age.${over ? 'over' : 'caution'}`, {
                time: formatWait(minutes),
                limit: formatWait(QUEUE_WAIT_LIMIT_MINUTES),
            })}
        </Notice>
    );
}

/**
 * How the results that finished in the last 24 hours ended, over every group of the view.
 * The colours carry the meaning, green for a success and red for a failure, so a pie of
 * outcomes reads at a glance. Nothing is drawn when no result finished in the window.
 */
export function OutcomePie({ usage }: { readonly usage: GridUsageView }): ReactNode {
    const { t } = useTranslation();
    const readable = usage.groups.filter((group) => !group.unavailable);
    const succeeded = readable.reduce((sum, group) => sum + group.outcomes_success, 0);
    const failed = readable.reduce((sum, group) => sum + group.outcomes_failed, 0);
    return (
        <SharePie
            caption={t('operations.grid.usage.outcomes.title')}
            label={t('operations.grid.usage.outcomes.title')}
            otherLabel={t('operations.grid.usage.pie.other')}
            items={[
                {
                    key: 'succeeded',
                    label: t('operations.grid.usage.columns.succeeded'),
                    value: succeeded,
                    swatch: 'text-up',
                },
                {
                    key: 'failed',
                    label: t('operations.grid.usage.columns.failed'),
                    value: failed,
                    swatch: 'text-down',
                },
            ]}
        />
    );
}

function UsageTable({ usage }: { readonly usage: GridUsageView }): ReactNode {
    const { t } = useTranslation();
    const unassigned = t('operations.grid.usage.unassigned');
    const columns = [
        'batches',
        'workunits',
        'results',
        'inProgress',
        'unsent',
        'oldest',
        'done',
        'succeeded',
        'failed',
        'share',
    ] as const;
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-1 font-normal">
                        {t(`operations.grid.usage.columns.${usage.group_kind}`)}
                    </th>
                    {columns.map((column) => (
                        <th key={column} className="py-1 text-right font-normal">
                            {t(`operations.grid.usage.columns.${column}`)}
                        </th>
                    ))}
                </tr>
            </thead>
            <tbody className="divide-y divide-line-subtle">
                {usage.groups.map((group) => {
                    const label = groupLabel(group, unassigned);
                    if (group.unavailable) {
                        return (
                            <tr key={group.key === '' ? 'none' : group.key}>
                                <td className="py-2">{label.text}</td>
                                <td colSpan={columns.length} className="py-2 text-warn">
                                    {t('operations.grid.usage.unavailable')}
                                </td>
                            </tr>
                        );
                    }
                    return (
                        <tr key={group.key === '' ? 'none' : group.key}>
                            <td className={label.isId ? 'py-2 font-mono text-xs' : 'py-2'}>
                                {label.text}
                            </td>
                            <td className="py-2 text-right font-mono">{group.batches}</td>
                            <td className="py-2 text-right font-mono">{group.workunits}</td>
                            <td className="py-2 text-right font-mono">{group.results}</td>
                            <td className="py-2 text-right font-mono">
                                {group.results_in_progress}
                            </td>
                            <td className="py-2 text-right font-mono">{group.results_unsent}</td>
                            <td className="py-2 text-right font-mono">
                                {formatOldest(group.oldest_unsent_seconds)}
                            </td>
                            <td className="py-2 text-right font-mono">{group.results_done}</td>
                            <td className="py-2 text-right font-mono">{group.outcomes_success}</td>
                            <td className="py-2 text-right font-mono">{group.outcomes_failed}</td>
                            <td className="py-2 text-right font-mono">
                                {formatShare(group.share)}
                            </td>
                        </tr>
                    );
                })}
            </tbody>
        </table>
    );
}

/** The usage table on its own, for a test that seeds the view. */
export function UsagePanel({ usage }: { readonly usage: GridUsageView }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">
                    {t(`operations.grid.usage.title.${usage.group_kind}`)}
                </h2>
                <p className="text-xs text-ink-faint">
                    {t(`operations.grid.usage.note.${usage.group_kind}`)}
                </p>
            </header>
            <QueueAgeNotice usage={usage} />
            <div className="flex flex-wrap gap-10">
                <OutcomePie usage={usage} />
                <SharePie
                    caption={t(`operations.grid.usage.pie.${usage.group_kind}`)}
                    label={t(`operations.grid.usage.pie.${usage.group_kind}`)}
                    otherLabel={t('operations.grid.usage.pie.other')}
                    items={usage.groups
                        .filter((group) => !group.unavailable)
                        .map((group) => ({
                            key: group.key === '' ? 'none' : group.key,
                            label: groupLabel(group, t('operations.grid.usage.unassigned')).text,
                            value: group.results,
                        }))}
                />
            </div>
            {usage.groups.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('operations.grid.usage.empty')}</p>
            ) : (
                <UsageTable usage={usage} />
            )}
        </section>
    );
}

/** The Usage tab: who uses the grid, read for the session's scope. */
export function GridUsageTab(): ReactNode {
    const { t } = useTranslation();
    // The usage is read when the tab opens and when the person asks. An operator's
    // read enters every tenant and each entry is recorded, so it is not polled.
    const usage = useQuery({
        queryKey: GRID_USAGE_QUERY_KEY,
        queryFn: api.gridUsage,
    });

    if (usage.isError) {
        return <Notice tone="error">{usage.error.message}</Notice>;
    }
    if (usage.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    return <UsagePanel usage={usage.data} />;
}
