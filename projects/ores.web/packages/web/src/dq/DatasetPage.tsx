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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useParams, useSearchParams } from 'react-router';
import type {
    BundleRosterBundle,
    BundleRosterMember,
} from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import {
    readBundleRoster,
    readDatasetPublications,
    readDatasetRows,
    readDatasetTables,
    reasonOf,
    type DatasetRowsPage,
} from '../api/dq.js';
import type { DatasetTable } from '@ores/wire-protocol/generated/dq/protocol/dataset_contents_protocol';
import { OWNER_KEY, REASON_KEY } from './readiness.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import type { DatasetPublication } from '@ores/wire-protocol/generated/dq/protocol/dataset_publications_protocol';
import { DEFAULT_PAGE_SIZE, LOAD_ALL_LIMIT, Pager, pageBounds } from '../ui/Pager.js';
import { useTabs } from '../ui/Tabs.js';
import { RelativeTime } from '../ui/Time.js';
import { RecordHeader } from '../refdata/records.js';

/**
 * One dataset, on the page the Record screen standard shapes.
 *
 * A dataset is a record, so its page has the breadcrumb, the display field as
 * the title and the key as the lead. The details state where its rows are staged,
 * what a publish dispatches to, both counts and the reason it cannot publish;
 * the publications state what each run wrote and what it said.
 *
 * The page names the dataset and the server resolves the bundle that carries it,
 * so a saved address works without the list having been read first.
 */
export function DatasetPage(): ReactNode {
    const { t } = useTranslation();
    const { code = '' } = useParams<{ code: string }>();
    const roster = useQuery({
        queryKey: ['dq-roster', 'dataset', code],
        queryFn: () => readBundleRoster('', code),
        enabled: code !== '',
    });
    const runs = useQuery({
        queryKey: ['dq-publications', code],
        queryFn: () => readDatasetPublications(code),
        enabled: code !== '',
    });
    const { tab, bar } = useTabs({
        label: code,
        tabs: ['details', 'publications', 'contents'],
        titleOf: (one) => t(`dq.dataset.tabs.${one}`),
    });

    const found = memberOf(roster.data, code);
    const crumbs = [
        ...useAreaRoot('dq'),
        { label: t('dq.coverage.title'), to: PATHS.dataQualityCoverage },
        { label: code },
    ];

    const bare = <RecordHeader crumbs={crumbs} title={code} recordKey={code} version={null} />;
    if (roster.isError) {
        return (
            <div className="space-y-4">
                {bare}
                <Notice tone="error">{roster.error.message}</Notice>
            </div>
        );
    }
    if (roster.data === undefined) {
        return (
            <div className="space-y-4">
                {bare}
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </div>
        );
    }
    if (found === undefined) {
        return (
            <div className="space-y-4">
                {bare}
                <Notice tone="info">{t('dq.dataset.notCarried', { code })}</Notice>
            </div>
        );
    }

    const { bundle, member } = found;
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={crumbs}
                title={member.dataset_name === '' ? code : member.dataset_name}
                recordKey={code}
                version={null}
                detail={t('dq.dataset.level', { level: bundle.level })}
            />
            {bar}
            {tab === 'details' ? (
                <Details bundle={bundle} member={member} runs={runs.data ?? []} />
            ) : tab === 'publications' ? (
                <Publications
                    runs={runs.data}
                    failed={runs.isError}
                    message={runs.error?.message}
                />
            ) : (
                <Contents code={code} artefactType={member.artefact_type} />
            )}
        </div>
    );
}

/** The bundle that carries a dataset, and the dataset's row within it. */
function memberOf(
    bundles: readonly BundleRosterBundle[] | undefined,
    code: string,
): { readonly bundle: BundleRosterBundle; readonly member: BundleRosterMember } | undefined {
    for (const bundle of bundles ?? []) {
        const member = bundle.members.find((one) => one.dataset_code === code);
        if (member !== undefined) {
            return { bundle, member };
        }
    }
    return undefined;
}

/** The fields of the dataset, in the grid the standard lays a record out in. */
function Details({
    bundle,
    member,
    runs,
}: {
    readonly bundle: BundleRosterBundle;
    readonly member: BundleRosterMember;
    readonly runs: readonly DatasetPublication[];
}): ReactNode {
    const { t } = useTranslation();
    const reason = reasonOf(member);
    const last = runs[0];
    const fields: readonly (readonly [string, ReactNode, string])[] = [
        [t('dq.dataset.name'), member.dataset_name, ''],
        [
            t('dq.dataset.code'),
            <span className="font-mono text-xs">{member.dataset_code}</span>,
            '',
        ],
        [
            t('dq.dataset.bundle'),
            `${bundle.name} · ${bundle.code}`,
            t('dq.dataset.level', { level: bundle.level }),
        ],
        [
            t('dq.dataset.artefactType'),
            <span className="font-mono text-xs">{member.artefact_type || '—'}</span>,
            '',
        ],
        [
            t('dq.dataset.artefactTable'),
            <span className="font-mono text-xs">{member.artefact_table || '—'}</span>,
            t('dq.dataset.artefactTableHint'),
        ],
        [
            t('dq.dataset.targetSubject'),
            <span className="font-mono text-xs">{member.target_subject || '—'}</span>,
            t('dq.dataset.targetSubjectHint'),
        ],
        [t('dq.dataset.staged'), member.staged_count.toLocaleString(), t('dq.dataset.stagedHint')],
        [t('dq.dataset.published'), '—', t('dq.dataset.notServed')],
        [
            t('dq.dataset.optional'),
            member.optional ? t('common.yes') : t('common.no'),
            t('dq.dataset.optionalHint'),
        ],
        [
            t('dq.dataset.lastPublished'),
            last === undefined ? (
                '—'
            ) : (
                <span>
                    <RelativeTime at={last.published_at} /> · {last.published_by}
                </span>
            ),
            last === undefined ? t('dq.dataset.lastPublishedGap') : outcomeLabel(t, last.outcome),
        ],
        [
            t('dq.dataset.readiness'),
            reason === null ? (
                <Tag>{t('dq.dataset.ready')}</Tag>
            ) : (
                <Tag tone="down">{t(REASON_KEY[reason])}</Tag>
            ),
            reason === null ? t('dq.dataset.readyHint') : t(OWNER_KEY[reason]),
        ],
    ];
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <dl className="grid gap-4 p-4 sm:grid-cols-2 lg:grid-cols-4">
                {fields.map(([label, value, hint]) => (
                    <div key={label}>
                        <dt className="text-xs text-ink-faint">{label}</dt>
                        <dd className="mt-0.5 text-sm">
                            {value}
                            {hint === '' ? null : (
                                <span className="mt-1 block text-xs text-ink-faint">{hint}</span>
                            )}
                        </dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

/**
 * The publishes of one dataset, newest first.
 *
 * A run that failed is drawn beside the runs that worked, because a person
 * asking when a dataset was last published is asking whether the last attempt
 * worked, and a table of the successes answers a different question.
 */
function Publications({
    runs,
    failed,
    message,
}: {
    readonly runs: readonly DatasetPublication[] | undefined;
    readonly failed: boolean;
    readonly message: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    if (failed) {
        return (
            <section className="rounded-md border border-line bg-surface-raised">
                <div className="p-4">
                    <h2 className="text-sm font-medium">{t('dq.dataset.publicationsTitle')}</h2>
                </div>
                <div className="px-4 pb-4">
                    <Notice tone="error">{message ?? ''}</Notice>
                </div>
            </section>
        );
    }
    if (runs === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (runs.length === 0) {
        return (
            <section className="rounded-md border border-line bg-surface-raised">
                <div className="p-4">
                    <h2 className="text-sm font-medium">{t('dq.dataset.publicationsTitle')}</h2>
                    <p className="mt-1 text-xs text-ink-muted">
                        {t('dq.dataset.publicationsNone')}
                    </p>
                </div>
            </section>
        );
    }
    return (
        <section className="overflow-x-auto rounded-md border border-line bg-surface-raised">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.runWhen')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.runBy')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.runMode')}</th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.dataset.runInserted')}
                        </th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.dataset.runUpdated')}
                        </th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.dataset.runSkipped')}
                        </th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.dataset.runDeleted')}
                        </th>
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.runOutcome')}</th>
                    </tr>
                </thead>
                <tbody>
                    {runs.map((run, i) => (
                        <tr
                            key={`${run.published_at}-${i}`}
                            className="border-b border-line-subtle last:border-b-0"
                        >
                            <td className="px-4 py-2">
                                <RelativeTime at={run.published_at} />
                            </td>
                            <td className="px-4 py-2 font-mono text-xs">{run.published_by}</td>
                            <td className="px-4 py-2 font-mono text-xs">{run.mode}</td>
                            <td className="px-4 py-2 text-right font-mono text-xs">
                                {run.records_inserted.toLocaleString()}
                            </td>
                            <td className="px-4 py-2 text-right font-mono text-xs">
                                {run.records_updated.toLocaleString()}
                            </td>
                            <td className="px-4 py-2 text-right font-mono text-xs">
                                {run.records_skipped.toLocaleString()}
                            </td>
                            <td className="px-4 py-2 text-right font-mono text-xs">
                                {run.records_deleted.toLocaleString()}
                            </td>
                            <td className="px-4 py-2">
                                <Tag
                                    {...(run.outcome === 'failed' ? { tone: 'down' as const } : {})}
                                >
                                    {outcomeLabel(t, run.outcome)}
                                </Tag>
                                {messagesOf(run).map((one) => (
                                    <span key={one} className="mt-1 block text-xs text-ink-faint">
                                        {one}
                                    </span>
                                ))}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/** The words a run's outcome is drawn in. */
function outcomeLabel(t: (key: string) => string, outcome: string): string {
    if (outcome === 'failed') {
        return t('dq.dataset.outcome.failed');
    }
    if (outcome === 'published_with_warnings') {
        return t('dq.dataset.outcome.warnings');
    }
    return t('dq.dataset.outcome.published');
}

/** What a run said, one sentence per message. */
function messagesOf(run: DatasetPublication): readonly string[] {
    try {
        const parsed = JSON.parse(run.messages) as { severity?: string; text?: string }[];
        return parsed
            .filter((one) => typeof one.text === 'string')
            .map((one) => `${one.severity ?? ''}: ${one.text}`.trim());
    } catch {
        return [];
    }
}

/**
 * What a dataset has staged: the tables its artefact type names, and the rows of
 * one of them.
 *
 * A dataset does not sit in one table. Its artefact type names a table or a
 * family of them, and every family carries its own columns, so the tab lists the
 * tables first — each with its own columns and its own staged count — and a
 * table is opened to read its rows. The open table is held in the address, so a
 * link reopens the same rows and Back returns to the list.
 */
function Contents({
    code,
    artefactType,
}: {
    readonly code: string;
    readonly artefactType: string;
}): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const table = search.get('table') ?? '';
    // The page is held in the address the way every list holds it: `from` is the
    // first row, omitted on the first page, and `size` is omitted at the default.
    const offset = pageNumber(search.get('from'), 0);
    const pageSize = Math.min(
        Math.max(pageNumber(search.get('size'), DEFAULT_PAGE_SIZE), 1),
        LOAD_ALL_LIMIT,
    );
    const tables = useQuery({
        queryKey: ['dq-tables', code],
        queryFn: () => readDatasetTables(code),
    });
    const rows = useQuery({
        queryKey: ['dq-rows', code, table, offset, pageSize],
        queryFn: () => readDatasetRows(code, table, pageSize, offset),
        enabled: table !== '',
    });
    const move = (next: number): void => {
        const params = new URLSearchParams(search);
        if (next === 0) {
            params.delete('from');
        } else {
            params.set('from', String(next));
        }
        setSearch(params);
    };
    const resize = (size: number): void => {
        const params = new URLSearchParams(search);
        params.delete('from');
        if (size === DEFAULT_PAGE_SIZE) {
            params.delete('size');
        } else {
            params.set('size', String(size));
        }
        setSearch(params);
    };

    if (tables.isError) {
        return <Notice tone="error">{tables.error.message}</Notice>;
    }
    if (tables.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (table !== '') {
        return (
            <TableRows
                code={code}
                table={table}
                rows={rows.data}
                offset={offset}
                pageSize={pageSize}
                onMove={move}
                onPageSize={resize}
                failed={rows.isError}
                message={rows.error?.message}
                onBack={() => setSearch({ tab: 'contents' })}
            />
        );
    }
    return (
        <TableList
            code={code}
            artefactType={artefactType}
            tables={tables.data}
            onOpen={(name) => setSearch({ tab: 'contents', table: name })}
        />
    );
}

/** The tables a dataset stages into, as the one list a person picks from. */
function TableList({
    code,
    artefactType,
    tables,
    onOpen,
}: {
    readonly code: string;
    readonly artefactType: string;
    readonly tables: readonly DatasetTable[];
    readonly onOpen: (table: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="overflow-x-auto rounded-md border border-line bg-surface-raised">
            <div className="p-4">
                <h2 className="text-sm font-medium">
                    {t('dq.dataset.contentsTitle')}
                    {artefactType === '' ? null : (
                        <span className="ml-2 font-mono text-xs text-ink-muted">
                            {artefactType}
                        </span>
                    )}
                </h2>
                <p className="mt-1 text-xs text-ink-muted">
                    {t('dq.dataset.contentsLead', { count: tables.length, code })}
                </p>
            </div>
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.table')}</th>
                        <th className="px-4 py-2 font-medium">{t('dq.dataset.columns')}</th>
                        <th className="px-4 py-2 text-right font-medium">
                            {t('dq.dataset.staged')}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {tables.map((one) => (
                        <tr
                            key={one.table_name}
                            className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                            tabIndex={0}
                            role="button"
                            onClick={() => onOpen(one.table_name)}
                            onKeyDown={(event) => {
                                if (event.key === 'Enter' || event.key === ' ') {
                                    event.preventDefault();
                                    onOpen(one.table_name);
                                }
                            }}
                        >
                            <td className="px-4 py-2 font-mono text-xs">{one.table_name}</td>
                            <td className="px-4 py-2 text-xs text-ink-muted">
                                {one.columns === '' ? '—' : one.columns}
                            </td>
                            <td className="px-4 py-2 text-right font-mono text-xs">
                                {one.staged_count.toLocaleString()}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/** One table's rows, drawn as the columns the rows carry state them. */
function TableRows({
    code,
    table,
    rows,
    offset,
    pageSize,
    onMove,
    onPageSize,
    failed,
    message,
    onBack,
}: {
    readonly code: string;
    readonly table: string;
    readonly rows: DatasetRowsPage | undefined;
    readonly offset: number;
    readonly pageSize: number;
    readonly onMove: (offset: number) => void;
    readonly onPageSize: (pageSize: number) => void;
    readonly failed: boolean;
    readonly message: string | undefined;
    readonly onBack: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const back = (
        <button type="button" className="text-xs text-accent underline" onClick={onBack}>
            {t('dq.dataset.allTables')}
        </button>
    );
    if (failed) {
        return (
            <section className="rounded-md border border-line bg-surface-raised">
                <div className="space-y-3 p-4">
                    {back}
                    <Notice tone="error">{message ?? ''}</Notice>
                </div>
            </section>
        );
    }
    if (rows === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    return (
        <section className="overflow-x-auto rounded-md border border-line bg-surface-raised">
            <div className="p-4">
                {back}
                <h2 className="mt-2 font-mono text-sm font-medium">{table}</h2>
                <p className="mt-1 text-xs text-ink-muted">
                    {t('dq.dataset.rowsLead', { code, count: rows.total.toLocaleString() })}
                </p>
            </div>
            {rows.rows.length === 0 ? (
                <p className="px-4 pb-4 text-sm text-ink-muted">
                    {t('dq.dataset.rowsNone', { code })}
                </p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            {rows.columns.map((column) => (
                                <th
                                    key={column.name}
                                    className="px-4 py-2 font-medium"
                                    title={column.type}
                                >
                                    {column.name}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows.rows.map((row, i) => (
                            <tr key={i} className="border-b border-line-subtle last:border-b-0">
                                {rows.columns.map((column) => (
                                    <td
                                        key={column.name}
                                        className="px-4 py-2 font-mono text-xs"
                                        title={cellText(row[column.name])}
                                    >
                                        {cellText(row[column.name])}
                                    </td>
                                ))}
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
            <div className="border-t border-line px-4 pb-3">
                <Pager
                    offset={offset}
                    shown={rows.rows.length}
                    total={rows.total}
                    pageSize={pageSize}
                    showing={t('dq.dataset.rowsShowing', {
                        first: String(pageBounds(offset, rows.rows.length).first),
                        last: String(pageBounds(offset, rows.rows.length).last),
                        total: rows.total.toLocaleString(),
                    })}
                    onMove={onMove}
                    onPageSize={onPageSize}
                />
            </div>
        </section>
    );
}

/** A page parameter, or its default when the address states none or nonsense. */
function pageNumber(raw: string | null, fallback: number): number {
    const value = Number.parseInt(raw ?? '', 10);
    return Number.isNaN(value) || value < 0 ? fallback : value;
}

/** One cell of a row, as the store's JSON holds it. */
function cellText(value: unknown): string {
    if (value === null || value === undefined) {
        return '—';
    }
    if (typeof value === 'string') {
        return value;
    }
    if (typeof value === 'number' || typeof value === 'boolean') {
        return String(value);
    }
    return JSON.stringify(value);
}
