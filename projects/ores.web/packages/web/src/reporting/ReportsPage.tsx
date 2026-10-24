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
 * One row per definition the session's parties own. The row is an arc: the
 * definition exists, its latest run started and took a length of time, that run
 * is healthy or it is not, and the definition runs again. Health is derived from
 * the latest run and never from the definition, and a definition that never ran
 * says so in words rather than reading as clean.
 *
 * The list spans the parties the session can see, which for an administrator is
 * the tenant's parties under the login party, so every row states its party and
 * the party filter narrows the rows to one. That is the list the entity screen
 * specification draws: it names what it holds in its lead sentence, tags each
 * party by name, and filters on the server.
 *
 * The server answers all three things the standard asks of a list. It searches
 * the name and the description, it orders by the columns the model declares
 * sortable, and it counts the definitions by health for the party and the search
 * the list is narrowed to, so no row is read in the browser to work one out.
 * The columns of a run have no order of their own: the order is the definition's.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link } from 'react-router';
import type { ReportRowView, ReportUnread } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Crumbs } from '../refdata/shared.js';
import { MY_PARTIES_QUERY_KEY, PARTY_COLUMN_ID, usePartyScope } from '../refdata/partyScope.js';
import { reportPath } from '../shell/paths.js';
import {
    RecordList,
    type ListColumn,
    type ListFilter,
    type ListSource,
} from '../refdata/RecordList.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { CronValue } from '../ui/CronField.js';
import { REPORTS_EVENTS } from './store.js';
import {
    Absent,
    HealthSummary,
    HealthTag,
    formatCount,
    formatDuration,
    runDuration,
} from './shared.js';

/** The key the list's pages are cached under, and the prefix its load state reads. */
export const REPORTS_QUERY_KEY = ['records', 'reports'] as const;

export { MY_PARTIES_QUERY_KEY };

/** The column of the row each unread read would have filled. */
const CELL_COLUMNS: Readonly<Record<ReportUnread['cell'], string>> = {
    parties: 'reporting.columns.party',
    latestRun: 'reporting.columns.latestRun',
    health: 'reporting.columns.health',
    errors: 'reporting.columns.errors',
    nextFire: 'reporting.columns.schedule',
};

/**
 * The reads that failed, so the cells they would have filled say why they are
 * empty. It sits above the table, where the list states what it knows about the
 * page before the rows.
 */
export function unreadNotes(
    unread: readonly ReportUnread[],
    t: (key: string, values?: Record<string, string>) => string,
): readonly { readonly tone: 'warn'; readonly text: string }[] {
    return unread.map((entry) => ({
        tone: 'warn' as const,
        text: t('reporting.unread', {
            cell: t(CELL_COLUMNS[entry.cell]),
            message: entry.message,
        }),
    }));
}

/** The fields of a definition the server orders by, which are the columns that can be clicked. */
const SORTABLE_FIELDS = ['name', 'report_type', 'schedule_expression'] as const;

/** The parties the rows come from, read one page at a time. */
function useReportSource(party: ListFilter | undefined): ListSource<ReportRowView> {
    const { t } = useTranslation();
    return {
        key: 'reports',
        read: async (page) => {
            // A party in the address is dropped when the list has no party filter to clear it.
            const partyId = party === undefined ? null : (page.filters['party'] ?? null);
            // The tally is of every report the party and the search match, so it
            // is read with the page and not added up from it. It is read apart
            // from the rows: a tally that cannot be read costs the strip and the
            // list still draws.
            const [answer, tally] = await Promise.all([
                api.reports({
                    offset: page.offset,
                    limit: page.limit,
                    sort: page.sort,
                    descending: page.descending,
                    partyId,
                    search: page.search,
                }),
                api
                    .reportHealth({ partyId, search: page.search })
                    .then((counted) => ({ counted }))
                    .catch((error: unknown) => ({
                        failure: error instanceof Error ? error.message : String(error),
                    })),
            ]);
            // A read that failed costs its own cell, and the list says which one
            // above the table rather than losing the rows it could compose.
            const notes = unreadNotes(answer.unread, t);
            if ('failure' in tally) {
                return {
                    rows: answer.rows,
                    total: answer.total,
                    notes: [
                        ...notes,
                        {
                            tone: 'warn' as const,
                            text: t('reporting.tally.unread', { message: tally.failure }),
                        },
                    ],
                };
            }
            return {
                rows: answer.rows,
                total: answer.total,
                notes,
                summary: <HealthSummary tally={tally.counted} />,
            };
        },
        search: true,
        sortable: SORTABLE_FIELDS,
        ...(party === undefined ? {} : { filters: [party] }),
        mayAdd: false,
        watches: [REPORTS_EVENTS],
        // The change event names a definition by its name, which is the key the
        // model declares for the row.
        recordKey: (row) => ({ name: row.name }),
    };
}

/** The party a row belongs to, as the tag the entity screen specification draws. */
function PartyTag({ row }: { readonly row: ReportRowView }): ReactNode {
    const { t } = useTranslation();
    if (row.partyName === '') {
        return <span className="text-ink-faint">{t('reporting.cell.partyNotStated')}</span>;
    }
    return <Tag>{row.partyName}</Tag>;
}

/** When the latest run started, or an absence when the definition never ran. */
function StartedCell({ row }: { readonly row: ReportRowView }): ReactNode {
    if (row.latestRun?.startedAt == null) {
        return <Absent />;
    }
    return <RelativeTime at={row.latestRun.startedAt} />;
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
    return <>{formatDuration(ms, t)}</>;
}

/** A count of one severity the run recorded, or an absence when nothing was read. */
function ProblemCount({ value }: { readonly value: number | null }): ReactNode {
    const { language } = useTranslation();
    if (value === null) {
        return <Absent />;
    }
    return <>{formatCount(value, language)}</>;
}

/** The schedule the definition runs on, in words, with the next fire when the scheduler states one. */
function ScheduleCell({ row }: { readonly row: ReportRowView }): ReactNode {
    const { t } = useTranslation();
    return (
        <span className="flex flex-col gap-1">
            <span className="text-xs">
                <CronValue expression={row.scheduleExpression} />
            </span>
            {row.nextFireAt !== null && (
                <span className="text-xs text-ink-faint">
                    <RelativeTime at={row.nextFireAt} />
                </span>
            )}
        </span>
    );
}

/**
 * The column list of the reports, in the standard's order: the display field
 * first, then the party the row belongs to, then what the row says about its
 * latest run. A count is a number, so it is stated in its own column, right
 * aligned and tabular, and never as a word a screen invented.
 */
function useColumns(showParty: boolean): readonly ListColumn<ReportRowView>[] {
    const { t } = useTranslation();
    const columns: readonly ListColumn<ReportRowView>[] = [
        {
            id: 'name',
            sort: 'name',
            header: t('reporting.columns.report'),
            cell: (row) => (
                <Link to={reportPath(row.id)} className="text-accent">
                    {row.name}
                </Link>
            ),
        },
        {
            id: 'party',
            header: t('reporting.columns.party'),
            cell: (row) => <PartyTag row={row} />,
        },
        {
            id: 'reportType',
            sort: 'report_type',
            header: t('reporting.columns.reportType'),
            cell: (row) => row.reportType,
            mono: true,
        },
        {
            id: 'schedule',
            sort: 'schedule_expression',
            header: t('reporting.columns.schedule'),
            cell: (row) => <ScheduleCell row={row} />,
        },
        {
            id: 'latestRun',
            header: t('reporting.columns.latestRun'),
            cell: (row) => <StartedCell row={row} />,
        },
        {
            id: 'duration',
            header: t('reporting.columns.duration'),
            cell: (row) => <DurationCell row={row} />,
            mono: true,
            numeric: true,
        },
        {
            id: 'health',
            header: t('reporting.columns.health'),
            cell: (row) => (
                <HealthTag
                    health={row.health}
                    stateName={row.stateName}
                    hasRun={row.latestRun !== null}
                />
            ),
        },
        {
            id: 'errors',
            header: t('reporting.columns.errors'),
            cell: (row) => <ProblemCount value={row.latestRun?.errors ?? null} />,
            mono: true,
            numeric: true,
        },
        {
            id: 'warnings',
            header: t('reporting.columns.warnings'),
            cell: (row) => <ProblemCount value={row.latestRun?.warnings ?? null} />,
            mono: true,
            numeric: true,
        },
    ];
    return showParty ? columns : columns.filter((column) => column.id !== PARTY_COLUMN_ID);
}

export function ReportsPage(): ReactNode {
    const { t } = useTranslation();
    const { several, filter, pending, error } = usePartyScope({
        label: t('reporting.columns.party'),
        all: t('reporting.filter.allParties'),
    });
    const source = useReportSource(filter);
    const trail = [...useAreaRoot('reporting'), { label: t('reporting.title') }];
    const lead = t('reporting.lead');

    if (pending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('reporting.title')}
                description={lead}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (error !== null) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('reporting.title')}
                description={lead}
            >
                <Notice tone="error">{error}</Notice>
            </ScreenFrame>
        );
    }

    return <ReportsList source={source} trail={trail} showParty={several} />;
}

/**
 * The list itself, drawn once the parties it filters by are known.
 *
 * The notes above the table say what a read that refused could not fill.
 */
function ReportsList({
    source,
    trail,
    showParty,
}: {
    readonly source: ListSource<ReportRowView>;
    readonly showParty: boolean;
    readonly trail: readonly { readonly label: string; readonly to?: string }[];
}): ReactNode {
    const { t } = useTranslation();
    const columns = useColumns(showParty);
    return (
        <RecordList
            source={source}
            title={t('reporting.title')}
            lead={t('reporting.lead')}
            crumbs={trail}
            columns={columns}
            pathOf={(row) => reportPath(row.id)}
        />
    );
}
