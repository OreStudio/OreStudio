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
 * One report definition, as the record screen standard draws a record page: the
 * trail, the display field as the title, the key and version line, then the
 * actions, the tabs in the address, the details with the last-changed line, and
 * the history of the record.
 *
 * A change is made in the record dialog the standard names, opened by Edit on
 * the card it changes, and never in a form on the page. The BFF rebuilds the
 * write from the read it makes at write time, so a field the dialog does not
 * offer keeps its stored value, and the version the dialog was filled from
 * travels with the write so a record somebody else changed is a conflict rather
 * than a silent overwrite.
 *
 * The page's own gaps are stated where they matter and not faked: the server
 * offers no delete for a definition, no create, and no server search of the
 * runs it lists, so the page offers none of those actions.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import type { ReportDetail, ReportNameChoice } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordDetails, RecordDialog, RecordHeader, type FieldSpec } from '../refdata/records.js';
import { RecordStoreProvider } from '../refdata/store.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, reportPath } from '../shell/paths.js';
import { AccessMark } from '../ui/AccessMark.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { CronValue } from '../ui/CronField.js';
import { REPORTS_EVENTS, REPORTS_RESOURCE, reportingStore } from './store.js';
import { Absent, HealthTag, formatDuration, runDuration } from './shared.js';

/** Where one definition's record is cached. */
export function reportKey(id: string): readonly unknown[] {
    return ['records', REPORTS_RESOURCE, 'key', id];
}

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

/** Whether an address segment names a record by id rather than by name. */
function isId(segment: string): boolean {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(segment);
}

/**
 * The definition's own fields, in the order the record states them.
 *
 * The field table drives the details and the dialog both, so the two cannot
 * disagree about what a definition holds. A field that is fixed once the record
 * exists is marked, so the dialog refuses to edit it.
 */
const DEFINITION_FIELDS: readonly FieldSpec[] = [
    {
        field: 'name',
        history: 'Name',
        labelKey: 'reporting.fields.name',
        kind: { kind: 'text', max: 200 },
        fixed: true,
        means: { key: 'reporting.means.name' },
    },
    {
        field: 'description',
        history: 'Description',
        labelKey: 'reporting.fields.description',
        kind: { kind: 'text', max: 2000 },
        blank: true,
    },
    {
        field: 'report_type',
        history: 'Report Type',
        labelKey: 'reporting.fields.report_type',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        means: { key: 'reporting.means.reportType', values: ['risk'] },
    },
    {
        field: 'schedule_expression',
        history: 'Schedule Expression',
        labelKey: 'reporting.fields.schedule_expression',
        kind: { kind: 'cron' },
        help: 'reporting.detail.scheduleExpressionHint',
    },
    {
        field: 'concurrency_policy',
        history: 'Concurrency Policy',
        labelKey: 'reporting.fields.concurrency_policy',
        kind: { kind: 'choice', options: ['skip', 'queue', 'fail'] },
        fixed: true,
        means: { key: 'reporting.means.concurrencyPolicy', values: ['skip', 'queue', 'fail'] },
    },
    {
        field: 'prepared_input_key',
        history: 'Prepared Input Key',
        labelKey: 'reporting.fields.prepared_input_key',
        kind: { kind: 'text', max: 500 },
        blank: true,
        means: { key: 'reporting.means.preparedInputKey' },
    },
    {
        field: 'pre_processing',
        history: 'Pre-Processing',
        labelKey: 'reporting.fields.pre_processing',
        kind: { kind: 'choice', options: ['execute', 'substitute'] },
        means: { key: 'reporting.means.preProcessing', values: ['execute', 'substitute'] },
    },
    {
        field: 'post_processing',
        history: 'Post-Processing',
        labelKey: 'reporting.fields.post_processing',
        kind: { kind: 'choice', options: ['execute', 'ignore'] },
        means: { key: 'reporting.means.postProcessing', values: ['execute', 'ignore'] },
    },
    {
        field: 'is_official',
        history: 'Is Official',
        labelKey: 'reporting.fields.is_official',
        kind: { kind: 'bool' },
        help: 'reporting.detail.isOfficialHint',
    },
];

/**
 * The record page, inside the store the shared record parts read.
 *
 * The store is the reporting one, so the dialog, the revert dialog and the
 * details read and write the reporting routes and not reference data's.
 */
export function ReportPage(): ReactNode {
    return (
        <RecordStoreProvider store={reportingStore}>
            <ReportBody />
        </RecordStoreProvider>
    );
}

function ReportBody(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const holds = useHolds();
    const mayEdit = holds(PERMISSION.reportDefinitionsWrite);
    const queries = useQueryClient();
    const navigate = useNavigate();
    const [editing, setEditing] = useState(false);
    const area = useAreaRoot('reporting');
    /*
     * The address is whatever a link or a bookmark carried: an id, or the name
     * an older address saved. The server resolves both, so this page never
     * answers a name with a refusal. A name that one definition carries comes
     * back as that record, and its id is seeded too, so the address can settle
     * on the canonical one without reading the record a second time.
     *
     * The page states its own failures, so the reads here are quiet and one
     * failure is one message rather than a notice and a banner both.
     */
    const open = useQuery({
        queryKey: reportKey(id),
        queryFn: async () => {
            const opened = await api.report(id);
            if (opened.kind === 'record' && opened.detail.definition.id !== id) {
                queries.setQueryData(reportKey(opened.detail.definition.id), opened);
            }
            return opened;
        },
        meta: { quiet: true },
    });
    const load = useLoadState([['records', REPORTS_RESOURCE]]);
    const news = useEntityChanges([REPORTS_EVENTS], load);
    const opened = open.data;
    const record = opened?.kind === 'record' ? opened.detail : undefined;
    const canonical = record?.definition.id;

    /*
     * A name that one definition carries replaces the address with that
     * definition's own, so a person who followed a stale link ends on the
     * address the list links to. The record is already drawn while this runs.
     */
    useEffect(() => {
        if (canonical !== undefined && canonical !== id) {
            void navigate(reportPath(canonical), { replace: true });
        }
    }, [canonical, id, navigate]);

    const trail = [...area, { label: t('reporting.title'), to: PATHS.reports }];
    const title = isId(id) ? shortId(id) : id;

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={id}
                description={t('reporting.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        const missing = open.error instanceof ApiFailure && open.error.status === 404;
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: title }]} />}
                title={title}
                description={t('reporting.lead')}
            >
                {missing ? (
                    <div className="space-y-3">
                        <Notice tone="info">{t('refdata.records.notFound')}</Notice>
                        <Link
                            to={PATHS.reports}
                            className="text-sm text-accent hover:text-accent-bright"
                        >
                            {t('refdata.records.backToList')}
                        </Link>
                    </div>
                ) : (
                    <Notice tone="error">{open.error.message}</Notice>
                )}
            </ScreenFrame>
        );
    }

    if (open.data.kind === 'choice') {
        return <NameChooser name={id} choices={open.data.choices} />;
    }

    const { definition, report, unread } = open.data.detail;
    /*
     * Refresh reads again what the page shows and nothing else. It does not
     * reload the page, because a save's own change event turns this button gold
     * like any other change.
     */
    const reload = (): void => {
        void queries.invalidateQueries({ queryKey: reportKey(definition.id) });
    };
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...trail, { label: definition.name }]}
                title={definition.name}
                recordKey={shortId(definition.id)}
                version={definition.version}
                subdued
                access={null}
                own={<RefreshButton onClick={reload} load={load} news={news} />}
            />
            <div className="space-y-4">
                <RecordDetails
                    specs={DEFINITION_FIELDS}
                    row={definition}
                    heading={{
                        title: t('reporting.detail.record'),
                        onEdit: mayEdit ? () => setEditing(true) : undefined,
                    }}
                    extra={[
                        [
                            t('reporting.detail.party'),
                            <PartyDetail detail={open.data.detail} />,
                            t('reporting.detail.partyHint'),
                        ],
                        [
                            t('reporting.detail.health'),
                            <HealthTag
                                health={report.health}
                                stateName={report.stateName}
                                hasRun={report.latestRun !== null}
                            />,
                            t('reporting.detail.healthHint'),
                        ],
                    ]}
                />
                <section className="card space-y-4 p-4">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-sm font-medium">{t('reporting.detail.latestRun')}</h2>
                        <AccessMark canWrite={false} />
                    </header>
                    <LatestRun detail={open.data.detail} unread={unread} />
                </section>
                <Notice tone="info">{t('reporting.detail.gaps')}</Notice>
            </div>
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: definition.name })}
                    resource={REPORTS_RESOURCE}
                    specs={DEFINITION_FIELDS}
                    row={definition}
                    keep={['id']}
                    onClose={() => setEditing(false)}
                />
            )}
        </div>
    );
}

/** The party the definition belongs to, by name, and the page of the party it names. */
function PartyDetail({ detail }: { readonly detail: ReportDetail }): ReactNode {
    const { t } = useTranslation();
    if (detail.definition.party_name === '') {
        return <span className="text-ink-faint">{t('reporting.cell.partyNotStated')}</span>;
    }
    return (
        <Link
            to={`/parties/${encodeURIComponent(detail.definition.party_id)}`}
            className="text-accent hover:text-accent-bright"
        >
            {detail.definition.party_name}
        </Link>
    );
}

/**
 * The latest run, as the list states it, so the detail and the list agree.
 *
 * A page that saw no run says which it is: a definition that never ran, or a
 * latest-run read that failed, in which case nothing about the run was checked.
 */
function LatestRun({
    detail,
    unread,
}: {
    readonly detail: ReportDetail;
    readonly unread: ReportDetail['unread'];
}): ReactNode {
    const { t } = useTranslation();
    const { report } = detail;
    if (report.latestRun === null) {
        const refusal = unread.find((entry) => entry.cell === 'latestRun');
        if (refusal !== undefined) {
            return (
                <p className="text-sm text-ink-muted">
                    {t('reporting.detail.couldNotCheck', { message: refusal.message })}
                </p>
            );
        }
        return <p className="text-sm text-ink-muted">{t('reporting.detail.neverRun')}</p>;
    }
    const ms = runDuration(report.latestRun.startedAt, report.latestRun.completedAt);
    const countsUnread = unread.some((entry) => entry.cell === 'errors');
    return (
        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
            <Detail
                label={t('reporting.detail.started')}
                value={
                    report.latestRun.startedAt === null ? (
                        <Absent />
                    ) : (
                        <RelativeTime at={report.latestRun.startedAt} />
                    )
                }
            />
            <Detail
                label={t('reporting.detail.completed')}
                value={
                    report.latestRun.completedAt === null ? (
                        <Absent />
                    ) : (
                        <RelativeTime at={report.latestRun.completedAt} />
                    )
                }
            />
            <Detail
                label={t('reporting.columns.duration')}
                value={ms === null ? <Absent /> : formatDuration(ms, t)}
            />
            <Detail
                label={t('reporting.columns.errors')}
                value={
                    countsUnread
                        ? t('reporting.cell.unread')
                        : String(report.latestRun.errors ?? '')
                }
            />
            <Detail
                label={t('reporting.columns.warnings')}
                value={
                    countsUnread
                        ? t('reporting.cell.unread')
                        : String(report.latestRun.warnings ?? '')
                }
            />
            <Detail
                label={t('reporting.columns.schedule')}
                value={
                    report.nextFireAt === null ? (
                        t('reporting.detail.noNextFire')
                    ) : (
                        <RelativeTime at={report.nextFireAt} />
                    )
                }
            />
        </dl>
    );
}

/**
 * The definitions a shared name offers, as a page a person chooses from.
 *
 * A name is unique only within a party, so an address that names one can stand
 * for several records. The page states the name it was asked for, then one row
 * per definition: the party that owns it, its schedule and what its latest run
 * did. Each row links to its own id, which is what settles the address. It owns
 * the trail because it is a page in its own right and not a record page.
 */
function NameChooser({
    name,
    choices,
}: {
    readonly name: string;
    readonly choices: readonly ReportNameChoice[];
}): ReactNode {
    const { t } = useTranslation();
    const area = useAreaRoot('reporting');
    const trail = [...area, { label: t('reporting.title'), to: PATHS.reports }, { label: name }];
    return (
        <ScreenFrame
            trail={<Crumbs parts={trail} />}
            title={name}
            description={t('reporting.choose.lead', { count: String(choices.length) })}
        >
            <div className="space-y-4">
                <section className="overflow-hidden rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.report')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.party')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.schedule')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.health')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {choices.map((choice) => (
                                <tr
                                    key={choice.id}
                                    className="border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                >
                                    <td className="px-4 py-2">
                                        <Link
                                            to={reportPath(choice.id)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {choice.name}
                                        </Link>
                                    </td>
                                    <td className="px-4 py-2">
                                        {choice.partyName === '' ? (
                                            <span className="text-ink-faint">
                                                {t('reporting.cell.partyNotStated')}
                                            </span>
                                        ) : (
                                            <Tag>{choice.partyName}</Tag>
                                        )}
                                    </td>
                                    <td className="px-4 py-2 text-xs">
                                        <CronValue expression={choice.scheduleExpression} />
                                    </td>
                                    <td className="px-4 py-2">
                                        <HealthTag
                                            health={choice.health}
                                            stateName={choice.stateName}
                                            hasRun={choice.hasRun}
                                        />
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </section>
                <Link to={PATHS.reports} className="text-sm text-accent hover:text-accent-bright">
                    {t('refdata.records.backToList')}
                </Link>
            </div>
        </ScreenFrame>
    );
}
