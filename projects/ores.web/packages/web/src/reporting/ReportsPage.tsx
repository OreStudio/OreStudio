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
 * The reports a party runs, from
 * doc/knowledge/journeys/reporting/journey_see_the_reports_a_party_runs.org.
 *
 * One row per definition the session's party owns. The row is an arc: the
 * definition exists, its latest run started and took a length of time, that run
 * is healthy or it is not, and the definition runs again. Health is derived from
 * the latest run and never from the definition, and a definition that never ran
 * says so in words rather than reading as clean.
 *
 * The owner cell the prototype draws has no source and is left out (G10), and
 * the on-demand and paused words wait on G14, so the schedule cell states the
 * expression and nothing about whether a job will fire it.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link } from 'react-router';
import type { ReportRowView, ReportTallyView, ReportUnread } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { reportPath } from '../shell/paths.js';
import { Metric } from '../ui/Metric.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';
import { formatDateTime } from '../ui/Time.js';
import {
    Absent,
    HealthTag,
    ProblemCounts,
    formatCount,
    formatDuration,
    runDuration,
} from './shared.js';

/** The address the list's rows are read from, and the key they are cached under. */
export const REPORTS_QUERY_KEY = ['reporting', 'reports'] as const;

/** The column of the row each unread read would have filled. */
const CELL_COLUMNS: Readonly<Record<ReportUnread['cell'], string>> = {
    parties: 'reporting.columns.party',
    latestRun: 'reporting.columns.latestRun',
    health: 'reporting.columns.health',
    errors: 'reporting.columns.problems',
    nextFire: 'reporting.columns.schedule',
};

/** The reads that failed, so the cells they would have filled say why they are empty. */
function UnreadNote({ unread }: { readonly unread: readonly ReportUnread[] }): ReactNode {
    const { t } = useTranslation();
    if (unread.length === 0) {
        return null;
    }
    return (
        <Notice tone="warn">
            <ul className="space-y-1">
                {unread.map((entry) => (
                    <li key={entry.cell}>
                        {t('reporting.unread', {
                            cell: t(CELL_COLUMNS[entry.cell]),
                            message: entry.message,
                        })}
                    </li>
                ))}
            </ul>
        </Notice>
    );
}

/** The party's short code, or a word saying it is not stated rather than a guess. */
function PartyCell({ row }: { readonly row: ReportRowView }): ReactNode {
    const { t } = useTranslation();
    if (row.partyCode === null) {
        return <span className="text-ink-faint">{t('reporting.cell.partyNotStated')}</span>;
    }
    return <span className="font-mono">{row.partyCode}</span>;
}

/** When the latest run started, or an absence when the definition never ran. */
function StartedCell({ row }: { readonly row: ReportRowView }): ReactNode {
    const { language } = useTranslation();
    if (row.latestRun === null || row.latestRun.startedAt === null) {
        return <Absent />;
    }
    return (
        <time dateTime={row.latestRun.startedAt} className="font-mono">
            {formatDateTime(row.latestRun.startedAt, language)}
        </time>
    );
}

/** How long the latest run took, or an absence when it has not finished. */
function DurationCell({ row }: { readonly row: ReportRowView }): ReactNode {
    const { t } = useTranslation();
    if (row.latestRun === null) {
        return <Absent />;
    }
    const ms = runDuration(row.latestRun.startedAt, row.latestRun.completedAt);
    if (ms === null) {
        return <Absent />;
    }
    return <span className="font-mono tabular-nums">{formatDuration(ms, t)}</span>;
}

/** The expression the definition runs on, with the next fire when the scheduler states one. */
function ScheduleCell({ row }: { readonly row: ReportRowView }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <span className="flex flex-col gap-1">
            <span className="font-mono text-xs">{row.scheduleExpression}</span>
            {row.nextFireAt !== null && (
                <span className="text-xs text-ink-faint">
                    {t('reporting.cell.nextFire', {
                        at: formatDateTime(row.nextFireAt, language),
                    })}
                </span>
            )}
        </span>
    );
}

/** The five words, counted, so the shape of the party reads before any row does. */
function Tally({ tally }: { readonly tally: ReportTallyView }): ReactNode {
    const { t, language } = useTranslation();
    const cards: (readonly [string, number])[] = [
        [t('reporting.health.failed'), tally.failed],
        [t('reporting.health.completedWithErrors'), tally.completedWithErrors],
        [t('reporting.health.running'), tally.running],
        [t('reporting.health.neverRun'), tally.neverRun],
        [t('reporting.health.completedClean'), tally.completedClean],
    ];
    // Unknown rows are counted beside the five words rather than inside them: a
    // run that could not be read is not a run that never happened.
    if (tally.unknown > 0) {
        cards.push([t('reporting.health.unknown'), tally.unknown]);
    }
    return (
        <section className="card space-y-3 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-sm font-medium">{t('reporting.tally.title')}</h2>
                <span className="text-xs text-ink-faint">{t('reporting.tally.lead')}</span>
            </header>
            <div className="grid gap-2 sm:grid-cols-3 lg:grid-cols-5">
                {cards.map(([label, count]) => (
                    <Metric key={label} value={formatCount(count, language)} label={label} />
                ))}
            </div>
            {tally.unstated > 0 && (
                <p className="text-xs text-ink-faint">
                    {t('reporting.tally.unstated', {
                        count: formatCount(tally.unstated, language),
                    })}
                </p>
            )}
        </section>
    );
}

/** The rows, one per definition, in the order the server sorted them. */
function Rows({
    rows,
    healthChecked,
}: {
    readonly rows: readonly ReportRowView[];
    /** False when the read that derives the health word failed for every row. */
    readonly healthChecked: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (rows.length === 0) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('reporting.empty')}
            </p>
        );
    }
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">{t('reporting.columns.report')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.party')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.schedule')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.latestRun')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.duration')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.health')}</th>
                        <th className="py-1 font-normal">{t('reporting.columns.problems')}</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {rows.map((row) => (
                        <tr key={row.name}>
                            <td className="py-2">
                                <Link to={reportPath(row.name)} className="text-accent">
                                    {row.name}
                                </Link>
                                <span className="ml-2 text-xs text-ink-faint">
                                    {row.reportType}
                                </span>
                            </td>
                            <td className="py-2">
                                <PartyCell row={row} />
                            </td>
                            <td className="py-2">
                                <ScheduleCell row={row} />
                            </td>
                            <td className="py-2 whitespace-nowrap">
                                <StartedCell row={row} />
                            </td>
                            <td className="py-2 whitespace-nowrap">
                                <DurationCell row={row} />
                            </td>
                            <td className="py-2">
                                <HealthTag
                                    health={row.health}
                                    stateName={row.stateName}
                                    checked={healthChecked}
                                />
                            </td>
                            <td className="py-2">
                                {row.latestRun === null ? (
                                    <Absent />
                                ) : (
                                    <ProblemCounts
                                        errors={row.latestRun.errors}
                                        warnings={row.latestRun.warnings}
                                    />
                                )}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

export function ReportsPage(): ReactNode {
    const { t } = useTranslation();
    const reports = useQuery({ queryKey: REPORTS_QUERY_KEY, queryFn: api.reports });
    const trail = (
        <Crumbs
            parts={[{ label: t('shell.menu.home'), to: '/' }, { label: t('reporting.title') }]}
        />
    );

    if (reports.isPending) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('reporting.title')}
                description={t('reporting.description')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('reporting.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (reports.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('reporting.title')}
                description={t('reporting.description')}
            >
                <Notice tone="error">{reports.error.message}</Notice>
            </ScreenFrame>
        );
    }

    const { rows, tally, unread } = reports.data;
    const healthChecked = !unread.some(
        (entry) => entry.cell === 'latestRun' || entry.cell === 'health',
    );
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('reporting.title')}
                    description={t('reporting.description')}
                    actions={<RefreshQueries keys={[REPORTS_QUERY_KEY]} />}
                />
            </div>
            <UnreadNote unread={unread} />
            <Tally tally={tally} />
            <section className="card space-y-4 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-sm font-medium">{t('reporting.list.title')}</h2>
                    <span className="text-xs text-ink-faint">{t('reporting.list.lead')}</span>
                </header>
                <Rows rows={rows} healthChecked={healthChecked} />
                <p className="text-xs text-ink-faint">{t('reporting.list.note')}</p>
            </section>
        </div>
    );
}
