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
 * The job page, from doc/knowledge/journeys/compute/journey_follow_a_job.org.
 *
 * One page answers where a job is, what ran it and what it produced. A job is a
 * workunit: it has no status of its own, so the page reads the state from the
 * canonical result, which the glossary defines, and puts the outcome of the run
 * beside it. A closed batch is not a successful batch, so a run that failed is
 * said to have failed.
 *
 * The page draws what the models hold. A run in flight has no node and no output
 * yet, and the page says so instead of leaving the field empty. The service stores
 * when a run was written and when its output arrived and nothing between, so the
 * bar for a run has two ends and is not split into phases. The run statistics are
 * not stored yet, so no panel stands in for them.
 */

import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { BatchView, ResultView, WorkunitView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import {
    gridAppVersionPath,
    gridBatchPath,
    gridJobPath,
    gridNodePath,
    gridReportRunPath,
    reportPath,
} from '../shell/paths.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { APP_VERSION_QUERY_KEY } from './GridAppVersionPage.js';
import { REPORT_RUN_QUERY_KEY } from './GridReportRunPage.js';
import { Button, Detail, Input, LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { formatDateTime, parseTimestamp } from '../ui/Time.js';
import {
    DownloadLink,
    GRID_JOBS_QUERY_KEY,
    OUTCOME,
    QUIET,
    ReadFailure,
    SERVER_STATE,
    StatusTag,
    numberParam,
} from './GridJobs.js';

/** The most runs of one job the page reads: a job is run as often as its target redundancy. */
export const RUNS_READ_LIMIT = 50;

const FIELD = 'operations.grid.jobs.fields';

/** A reference that opens another page without leaving the application. */
function PageLink({ to, children }: { readonly to: string; readonly children: ReactNode }) {
    return (
        <Link
            to={to}
            className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
        >
            {children}
        </Link>
    );
}

function Panel({
    title,
    note,
    children,
}: {
    readonly title: string;
    readonly note?: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <h2 className="text-lg font-medium">{title}</h2>
                {note !== undefined && <span className="text-xs text-ink-faint">{note}</span>}
            </header>
            {children}
        </section>
    );
}

/** The canonical result of a job: the run the grid accepted as its answer. */
export function canonicalRun(
    workunit: WorkunitView,
    runs: readonly ResultView[],
): ResultView | undefined {
    return runs.find((run) => run.id === workunit.canonical_result_id);
}

/** The run the page speaks for: the accepted one, else the first. */
export function focusRun(
    workunit: WorkunitView,
    runs: readonly ResultView[],
): ResultView | undefined {
    return canonicalRun(workunit, runs) ?? runs[0];
}

/** A run that ended in a client error. */
function failed(run: ResultView | undefined): boolean {
    return run !== undefined && OUTCOME[run.outcome]?.name === 'clientError';
}

function OutcomeTag({ run }: { readonly run: ResultView }): ReactNode {
    const { t } = useTranslation();
    const outcome = OUTCOME[run.outcome];
    return outcome === undefined ? (
        <span className="text-ink-faint">{t('operations.grid.jobs.outcomeNotSet')}</span>
    ) : (
        <Tag tone={outcome.tone}>{t(`operations.grid.jobs.outcome.${outcome.name}`)}</Tag>
    );
}

/** Done or not done, with the outcome of the accepted run beside it. */
function JobState({
    workunit,
    runs,
}: {
    readonly workunit: WorkunitView;
    readonly runs: readonly ResultView[];
}): ReactNode {
    const { t } = useTranslation();
    const canonical = canonicalRun(workunit, runs);
    if (workunit.canonical_result_id === null) {
        return <Tag tone="accent">{t('operations.grid.job.notDone')}</Tag>;
    }
    return (
        <span className="flex gap-1">
            <Tag tone="up">{t('operations.grid.job.done')}</Tag>
            {canonical !== undefined && <OutcomeTag run={canonical} />}
        </span>
    );
}

/** The node of a run: a link for a role that may read it, and the reason for one that may not. */
function NodeCell({ run }: { readonly run: ResultView | undefined }): ReactNode {
    const { t } = useTranslation();
    if (run === undefined || (run.host_id === null && !run.host_hidden)) {
        return <span className="text-ink-faint">{t('operations.grid.job.notYetKnown')}</span>;
    }
    if (run.host_hidden || run.host_id === null) {
        return <span>{t('operations.grid.jobs.hostHidden')}</span>;
    }
    return <PageLink to={gridNodePath(run.host_id)}>{run.host_id}</PageLink>;
}

function ReceivedAt({ run }: { readonly run: ResultView | undefined }): ReactNode {
    const { t, language } = useTranslation();
    return run === undefined || run.received_at === null ? (
        <span className="text-ink-faint">{t('operations.grid.job.notYetReceived')}</span>
    ) : (
        <span>{formatDateTime(run.received_at, language)}</span>
    );
}

function Header({
    workunit,
    batch,
    runs,
}: {
    readonly workunit: WorkunitView;
    readonly batch: BatchView | undefined;
    readonly runs: readonly ResultView[];
}): ReactNode {
    const { t, language } = useTranslation();
    const first = focusRun(workunit, runs);
    const none = t('operations.grid.jobs.none');
    return (
        <Panel
            title={t('operations.grid.job.title')}
            note={t('operations.grid.jobs.details.readOnly')}
        >
            <div>
                <p className="text-xs text-ink-faint">{t(`${FIELD}.workunitId`)}</p>
                <p className="break-all font-mono text-sm">{workunit.id}</p>
                <div className="mt-2">
                    <JobState workunit={workunit} runs={runs} />
                </div>
            </div>
            <dl className="grid gap-3 sm:grid-cols-2">
                <Detail
                    label={t('operations.grid.job.batchName')}
                    value={
                        batch === undefined ? (
                            none
                        ) : (
                            <PageLink to={gridBatchPath(batch.id)}>
                                {batch.report_run_name ?? batch.external_ref}
                            </PageLink>
                        )
                    }
                />
                <Detail
                    label={t('operations.grid.job.batchStatus')}
                    value={batch === undefined ? none : <StatusTag status={batch.status} />}
                />
                <Detail
                    label={
                        batch?.party_name == null
                            ? t(`${FIELD}.partyId`)
                            : t('operations.grid.jobs.fields.party')
                    }
                    value={
                        batch?.party_name == null ? (
                            (batch?.party_id ?? none)
                        ) : (
                            <span title={batch.party_id ?? undefined}>{batch.party_name}</span>
                        )
                    }
                    mono={batch?.party_name == null}
                />
                <Detail label={t(`${FIELD}.hostId`)} value={<NodeCell run={first} />} mono />
                <Detail
                    label={t('operations.grid.job.batchRecordedAt')}
                    value={batch === undefined ? none : formatDateTime(batch.recorded_at, language)}
                />
                <Detail label={t(`${FIELD}.receivedAt`)} value={<ReceivedAt run={first} />} />
                <Detail label={t(`${FIELD}.priority`)} value={workunit.priority} />
                <Detail label={t(`${FIELD}.targetRedundancy`)} value={workunit.target_redundancy} />
                <Detail
                    label={t(`${FIELD}.canonicalResultId`)}
                    value={workunit.canonical_result_id ?? none}
                    mono
                />
            </dl>
            {failed(canonicalRun(workunit, runs)) && (
                <Notice tone="warn">{t('operations.grid.job.failedNote')}</Notice>
            )}
        </Panel>
    );
}

function RunsTable({
    workunit,
    runs,
}: {
    readonly workunit: WorkunitView;
    readonly runs: readonly ResultView[];
}): ReactNode {
    const { t, language, plural } = useTranslation();
    return (
        <Panel
            title={t('operations.grid.job.runs.title')}
            note={plural('operations.grid.job.runs.count', runs.length)}
        >
            <div className="overflow-x-auto">
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            {[
                                t('operations.grid.job.runs.run'),
                                t(`${FIELD}.serverState`),
                                t(`${FIELD}.outcome`),
                                t(`${FIELD}.hostId`),
                                t(`${FIELD}.recordedAt`),
                                t(`${FIELD}.receivedAt`),
                            ].map((heading) => (
                                <th key={heading} className="py-1 pr-3 font-normal">
                                    {heading}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {runs.map((run, index) => {
                            const state = SERVER_STATE[run.server_state];
                            return (
                                <tr key={run.id} className="border-t border-line-subtle align-top">
                                    <td className="py-1.5 pr-3 font-mono text-xs" title={run.id}>
                                        {index + 1}
                                        {run.id === workunit.canonical_result_id &&
                                            ` ${t('operations.grid.job.runs.canonical')}`}
                                    </td>
                                    <td className="py-1.5 pr-3">
                                        {state === undefined ? (
                                            run.server_state
                                        ) : (
                                            <Tag tone={state.tone}>
                                                {t(
                                                    `operations.grid.jobs.serverState.${state.name}`,
                                                )}
                                            </Tag>
                                        )}
                                    </td>
                                    <td className="py-1.5 pr-3">
                                        <OutcomeTag run={run} />
                                    </td>
                                    <td className="py-1.5 pr-3 font-mono text-xs">
                                        <NodeCell run={run} />
                                    </td>
                                    <td className="py-1.5 pr-3">
                                        {formatDateTime(run.recorded_at, language)}
                                    </td>
                                    <td className="py-1.5 pr-3">
                                        <ReceivedAt run={run} />
                                    </td>
                                </tr>
                            );
                        })}
                        {runs
                            .filter((run) => run.error_message !== null)
                            .map((run) => (
                                <tr key={`${run.id}-error`}>
                                    <td colSpan={6} className="pb-2 text-xs text-down">
                                        {t(`${FIELD}.errorMessage`)}: {run.error_message}
                                    </td>
                                </tr>
                            ))}
                    </tbody>
                </table>
            </div>
        </Panel>
    );
}

/** The seconds between two instants, or null when the run has not arrived. */
export function secondsBetween(recordedAt: string, receivedAt: string | null): number | null {
    if (receivedAt === null) {
        return null;
    }
    const milliseconds =
        parseTimestamp(receivedAt).getTime() - parseTimestamp(recordedAt).getTime();
    return Math.max(0, Math.round(milliseconds / 1000));
}

/** The split of a run's time: claim to arrival, and how much of it the engine ran. */
export interface Phases {
    readonly totalMs: number;
    /** The engine's own time, or null when the node did not report it. */
    readonly ranMs: number | null;
    /** The rest: the download, the upload and the hand back, or null with no engine time. */
    readonly otherMs: number | null;
}

/**
 * The phases of a run, or nothing when the run has no recorded claim or has not ended.
 *
 * The server's clock stamps the claim and the arrival, so their difference is the whole time
 * of the run on the grid. The node's clock times the engine alone. The engine's time cannot
 * exceed the whole, so a longer one, which only a rounding or a fault could make, is cut to it.
 */
export function phasesOf(run: ResultView): Phases | null {
    if (run.started_at === null || run.received_at === null) {
        return null;
    }
    const totalMs = Math.max(
        0,
        parseTimestamp(run.received_at).getTime() - parseTimestamp(run.started_at).getTime(),
    );
    if (run.duration_ms === null) {
        return { totalMs, ranMs: null, otherMs: null };
    }
    const ranMs = Math.min(run.duration_ms, totalMs);
    return { totalMs, ranMs, otherMs: totalMs - ranMs };
}

/** A time span the way a person reads it: whole seconds, then minutes. */
export function formatDuration(milliseconds: number): string {
    if (milliseconds < 500) {
        return '< 1 s';
    }
    const seconds = Math.round(milliseconds / 1000);
    return seconds < 60 ? `${seconds} s` : `${Math.floor(seconds / 60)} min ${seconds % 60} s`;
}

/** The bar of a run that has a recorded claim, split by what the engine ran. */
function PhaseBar({
    run,
    phases,
}: {
    readonly run: ResultView;
    readonly phases: Phases;
}): ReactNode {
    const { t, language } = useTranslation();
    const key = 'operations.grid.job.time';
    const ranShare =
        phases.ranMs === null
            ? null
            : phases.totalMs === 0
              ? 100
              : (phases.ranMs / phases.totalMs) * 100;
    return (
        <Panel title={t(`${key}.title`)} note={t(`${key}.phasesNote`)}>
            <div className="flex h-3 w-full overflow-hidden rounded bg-bg-hover">
                {ranShare === null ? (
                    <div className="h-3 w-full bg-accent/60" />
                ) : (
                    <>
                        <div
                            className={`h-3 ${failed(run) ? 'bg-down' : 'bg-up'}`}
                            style={{ width: `${ranShare}%` }}
                        />
                        <div className="h-3 bg-accent/40" style={{ width: `${100 - ranShare}%` }} />
                    </>
                )}
            </div>
            <div className="flex justify-between text-xs text-ink-muted">
                <span>
                    {t(`${key}.claimed`, {
                        time: formatDateTime(run.started_at as string, language),
                    })}
                </span>
                <span>
                    {t(`${key}.receivedAfter`, {
                        time: formatDateTime(run.received_at as string, language),
                        duration: formatDuration(phases.totalMs),
                    })}
                </span>
            </div>
            {phases.ranMs === null || phases.otherMs === null ? (
                <p className="text-xs text-ink-faint">{t(`${key}.unreported`)}</p>
            ) : (
                <ul className="flex flex-wrap gap-x-6 text-xs text-ink-muted">
                    <li>{t(`${key}.ran`, { duration: formatDuration(phases.ranMs) })}</li>
                    <li>{t(`${key}.other`, { duration: formatDuration(phases.otherMs) })}</li>
                </ul>
            )}
            <p className="text-xs text-ink-faint">{t(`${key}.phasesHint`)}</p>
        </Panel>
    );
}

/**
 * One bar for the first run. A run with a recorded claim is split by what the engine ran. An
 * older run has no claim, and its bar runs from its row being written to its output arriving.
 */
function TimeBar({ run }: { readonly run: ResultView }): ReactNode {
    const { t, language } = useTranslation();
    const phases = phasesOf(run);
    if (phases !== null) {
        return <PhaseBar run={run} phases={phases} />;
    }
    const seconds = secondsBetween(run.recorded_at, run.received_at);
    const tone = seconds === null ? 'bg-accent/40' : failed(run) ? 'bg-down' : 'bg-up';
    return (
        <Panel
            title={t('operations.grid.job.time.title')}
            note={t('operations.grid.job.time.note')}
        >
            <div className="h-3 w-full rounded bg-bg-hover">
                <div
                    className={`h-3 rounded ${tone}`}
                    style={{ width: seconds === null ? '55%' : '100%' }}
                />
            </div>
            <div className="flex justify-between text-xs text-ink-muted">
                <span>
                    {t('operations.grid.job.time.recorded', {
                        time: formatDateTime(run.recorded_at, language),
                    })}
                </span>
                <span>
                    {seconds === null || run.received_at === null
                        ? t('operations.grid.job.time.running')
                        : t('operations.grid.job.time.received', {
                              time: formatDateTime(run.received_at, language),
                              seconds,
                          })}
                </span>
            </div>
            <p className="text-xs text-ink-faint">{t('operations.grid.job.time.hint')}</p>
        </Panel>
    );
}

function SiblingJobs({ workunit }: { readonly workunit: WorkunitView }): ReactNode {
    const { t, plural } = useTranslation();
    const [params, setParams] = useSearchParams();
    const size = numberParam(params.get('size'), DEFAULT_PAGE_SIZE);
    const page = numberParam(params.get('page'), 1);
    const offset = (page - 1) * size;
    const siblings = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'workunits', workunit.batch_id, offset, size],
        queryFn: () => api.gridJobWorkunits(workunit.batch_id, offset, size),
        meta: QUIET,
    });
    const move = (patch: Readonly<Record<string, string | null>>): void => {
        const next = new URLSearchParams(params);
        for (const [key, value] of Object.entries(patch)) {
            if (value === null) {
                next.delete(key);
            } else {
                next.set(key, value);
            }
        }
        setParams(next, { replace: true });
    };
    if (siblings.isError) {
        return <ReadFailure error={siblings.error} onRetry={() => void siblings.refetch()} />;
    }
    if (siblings.isPending) {
        return (
            <p className="text-sm text-ink-muted">{t('operations.grid.jobs.loadingWorkunits')}</p>
        );
    }
    const { rows, total } = siblings.data;
    const { first, last } = pageBounds(offset, rows.length);
    return (
        <Panel
            title={t('operations.grid.job.siblings.title')}
            note={plural('operations.grid.job.siblings.count', total, { total })}
        >
            <p className="text-xs text-ink-faint">{t('operations.grid.job.siblings.note')}</p>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 pr-3 font-normal">{t(`${FIELD}.workunitId`)}</th>
                        <th className="py-1 pr-3 font-normal">
                            {t('operations.grid.job.siblings.state')}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {rows.map((row) => (
                        <tr
                            key={row.id}
                            className={`border-t border-line-subtle ${row.id === workunit.id ? 'bg-bg-hover' : ''}`}
                        >
                            <td className="py-1.5 pr-3 font-mono text-xs" title={row.id}>
                                {row.id === workunit.id ? (
                                    `${row.id.slice(0, 8)} ${t('operations.grid.job.siblings.here')}`
                                ) : (
                                    <PageLink to={gridJobPath(row.id)}>
                                        {row.id.slice(0, 8)}
                                    </PageLink>
                                )}
                            </td>
                            <td className="py-1.5 pr-3">
                                <Tag tone={row.canonical_result_id === null ? 'accent' : 'up'}>
                                    {t(
                                        row.canonical_result_id === null
                                            ? 'operations.grid.job.notDone'
                                            : 'operations.grid.job.done',
                                    )}
                                </Tag>
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <Pager
                offset={offset}
                shown={rows.length}
                total={total}
                pageSize={size}
                showing={t('operations.grid.job.siblings.showing', { first, last, total })}
                onMove={(next) => move({ page: String(Math.floor(next / size) + 1) })}
                onPageSize={(next) => move({ size: String(next), page: null })}
            />
        </Panel>
    );
}

function Files({
    workunit,
    runs,
}: {
    readonly workunit: WorkunitView;
    readonly runs: readonly ResultView[];
}): ReactNode {
    const { t } = useTranslation();
    const none = t('operations.grid.jobs.none');
    return (
        <Panel title={t('operations.grid.job.files.title')}>
            <dl className="space-y-3">
                <Detail
                    label={t(`${FIELD}.inputUri`)}
                    value={<DownloadLink kind="input" id={workunit.id} uri={workunit.input_uri} />}
                    mono
                />
                <Detail label={t(`${FIELD}.configUri`)} value={workunit.config_uri ?? none} mono />
                {runs.map((run, index) => (
                    <Detail
                        key={run.id}
                        label={t('operations.grid.job.files.output', { run: index + 1 })}
                        value={
                            run.output_uri === null ? (
                                none
                            ) : (
                                <DownloadLink kind="output" id={run.id} uri={run.output_uri} />
                            )
                        }
                        mono
                    />
                ))}
            </dl>
        </Panel>
    );
}

function Release({ workunit }: { readonly workunit: WorkunitView }): ReactNode {
    const { t } = useTranslation();
    const version = useQuery({
        queryKey: [APP_VERSION_QUERY_KEY, workunit.app_version_id],
        queryFn: () => api.gridAppVersion(workunit.app_version_id),
        meta: QUIET,
    });
    return (
        <Panel title={t('operations.grid.job.release.title')}>
            <dl className="grid gap-3">
                <Detail
                    label={t(`${FIELD}.appVersionId`)}
                    value={
                        <PageLink to={gridAppVersionPath(workunit.app_version_id)}>
                            {workunit.app_version_id}
                        </PageLink>
                    }
                    mono
                />
                {version.isSuccess && (
                    <>
                        <Detail
                            label={t('operations.grid.appVersion.build.engine')}
                            value={version.data.version.engine_version}
                        />
                        <Detail
                            label={t('operations.grid.appVersion.build.wrapper')}
                            value={version.data.version.wrapper_version}
                        />
                    </>
                )}
            </dl>
        </Panel>
    );
}

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/**
 * How a node of the chain stands: stated by the model, named by text only, or the job
 * the page is about.
 */
type NodeKind = 'stated' | 'unchecked' | 'here';

const DOT: Readonly<Record<NodeKind, string>> = {
    stated: 'bg-ink-faint',
    unchecked: 'border border-dashed border-warn bg-surface-base',
    here: 'bg-accent',
};

function ChainNode({
    kind = 'stated',
    title,
    note,
    children,
}: {
    readonly kind?: NodeKind;
    readonly title: ReactNode;
    readonly note?: ReactNode;
    readonly children?: ReactNode;
}): ReactNode {
    return (
        <li className="relative">
            <span
                aria-hidden="true"
                className={`absolute -left-[1.81rem] top-1.5 h-2.5 w-2.5 rounded-full ${DOT[kind]}`}
            />
            <div className={kind === 'here' ? 'font-medium' : undefined}>{title}</div>
            {note !== undefined && <p className="text-xs text-ink-faint">{note}</p>}
            {children}
        </li>
    );
}

/**
 * The chain that put this job on the grid, one column of nodes from the top: the scheduled
 * job, the report definition, the report run, the batch, and this job with its results.
 *
 * The batch names its run in the External Ref, which the model does not state as a
 * relationship, so a batch a report made is drawn dashed and says so. The scheduled job has no
 * read by id yet, so its node says that. A session that may not read the run sees no report
 * nodes, and the batch says why.
 */
function Lineage({
    workunit,
    batch,
    runs,
}: {
    readonly workunit: WorkunitView;
    readonly batch: BatchView | undefined;
    readonly runs: readonly ResultView[];
}): ReactNode {
    const { t, plural } = useTranslation();
    const reportRef =
        batch !== undefined && UUID.test(batch.external_ref) ? batch.external_ref : null;
    const run = useQuery({
        queryKey: [REPORT_RUN_QUERY_KEY, reportRef],
        queryFn: () => api.gridReportRun(reportRef ?? ''),
        enabled: reportRef !== null,
        meta: QUIET,
    });
    const batchId = batch?.id;
    const jobs = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'lineage-jobs', batchId],
        queryFn: () => api.gridJobWorkunits(batchId ?? '', 0, 1),
        enabled: batchId !== undefined,
        meta: QUIET,
    });
    const siblings = jobs.data === undefined ? 0 : Math.max(0, jobs.data.total - 1);
    // The service names the run on the batch exactly when this session may read it, so the
    // chain above the batch is drawn then. An id that names no run it can read is text.
    const reported = reportRef !== null && batch?.report_run_name != null;
    const key = 'operations.grid.job.lineage';
    return (
        <Panel title={t(`${key}.title`)}>
            <ol className="relative ml-2 space-y-4 pl-6 text-sm before:absolute before:bottom-2 before:left-0 before:top-2 before:w-px before:bg-ink-faint">
                {reported && (
                    <ChainNode
                        kind="unchecked"
                        title={t(`${key}.scheduled`)}
                        note={t(`${key}.scheduledNote`)}
                    />
                )}
                {run.data !== undefined && (
                    <ChainNode
                        title={
                            <PageLink to={reportPath(run.data.definition_id)}>
                                {run.data.definition_name ?? t(`${key}.definition`)}
                            </PageLink>
                        }
                        note={t(`${key}.definitionNote`)}
                    />
                )}
                {reported && reportRef !== null && (
                    <ChainNode
                        title={
                            <PageLink to={gridReportRunPath(reportRef)}>
                                {run.data?.name ?? batch?.report_run_name ?? t(`${key}.reportRun`)}
                            </PageLink>
                        }
                        note={t(`${key}.runNote`)}
                    />
                )}
                {batch !== undefined && (
                    <ChainNode
                        kind={reported ? 'unchecked' : 'stated'}
                        title={
                            <PageLink to={gridBatchPath(batch.id)}>
                                {batch.report_run_name ?? batch.external_ref}
                            </PageLink>
                        }
                        note={
                            reported
                                ? t(`${key}.batchUnchecked`)
                                : reportRef === null
                                  ? t(`${key}.noReport`)
                                  : t(`${key}.unreadable`)
                        }
                    />
                )}
                <ChainNode
                    kind="here"
                    title={t(`${key}.workunit`, { id: workunit.id.slice(0, 8) })}
                    note={t(`${key}.viaBatch`)}
                >
                    <ul className="ml-4 mt-1 list-disc text-xs text-ink-muted">
                        {runs.map((result, index) => (
                            <li key={result.id}>
                                {t(`${key}.result`, {
                                    id: result.id.slice(0, 8),
                                    run: index + 1,
                                })}{' '}
                                <OutcomeTag run={result} />
                            </li>
                        ))}
                    </ul>
                </ChainNode>
                {siblings > 0 && <ChainNode title={plural(`${key}.siblings`, siblings)} />}
            </ol>
        </Panel>
    );
}

const UUID_TEXT = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

/** The reads the finder tries, each answering with the id it opens or refusing. */
export interface FinderReads {
    readonly workunit: (id: string) => Promise<{ readonly id: string }>;
    readonly result: (id: string) => Promise<{ readonly workunit_id: string }>;
    readonly batch: (id: string) => Promise<{ readonly id: string }>;
}

/**
 * Where a search opens, or nothing when it matches nothing.
 *
 * An id is tried as a job, then as a result, then as a batch, and the first the session
 * may read opens. Text that is not an id is a part of a batch reference, and the Jobs tab
 * lists the batches it matches.
 */
export async function findTarget(wanted: string, reads: FinderReads): Promise<string | null> {
    if (!UUID_TEXT.test(wanted)) {
        return `/operations/grid?${new URLSearchParams({ tab: 'jobs', find: wanted })}`;
    }
    const attempts: readonly (() => Promise<string>)[] = [
        async () => gridJobPath((await reads.workunit(wanted)).id),
        async () => gridJobPath((await reads.result(wanted)).workunit_id),
        async () => gridBatchPath((await reads.batch(wanted)).id),
    ];
    for (const attempt of attempts) {
        try {
            return await attempt();
        } catch (error) {
            // Not this kind of id, or not one the session may read: try the next. Any other
            // failure is the search failing, and it is not read as an id that matches nothing.
            if (!(error instanceof ApiFailure && (error.status === 404 || error.status === 403))) {
                throw error;
            }
        }
    }
    return null;
}

/** The finder bar of the page: a batch reference, a job id or a result id. */
function JobFinder(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [text, setText] = useState('');
    const [outcome, setOutcome] = useState<'idle' | 'searching' | 'missing' | 'failed'>('idle');
    const find = async (): Promise<void> => {
        const wanted = text.trim();
        if (wanted === '' || outcome === 'searching') {
            return;
        }
        setOutcome('searching');
        try {
            const target = await findTarget(wanted, {
                workunit: (id) => api.gridJobWorkunit(id),
                result: (id) => api.gridJobResult(id),
                batch: (id) => api.gridJobBatch(id),
            });
            if (target === null) {
                setOutcome('missing');
            } else {
                setOutcome('idle');
                void navigate(target);
            }
        } catch {
            setOutcome('failed');
        }
    };
    return (
        <form
            className="flex flex-wrap items-end gap-3"
            onSubmit={(event) => {
                event.preventDefault();
                void find();
            }}
        >
            <label className="flex flex-col gap-1 text-xs text-ink-faint sm:w-96">
                {t('operations.grid.job.find.label')}
                <Input
                    type="search"
                    value={text}
                    placeholder={t('operations.grid.job.find.placeholder')}
                    onChange={(event) => setText(event.target.value)}
                />
            </label>
            <Button type="submit" variant="secondary" disabled={outcome === 'searching'}>
                {t('operations.grid.job.find.button')}
            </Button>
            <span role="status" className="text-sm text-warn">
                {outcome === 'missing' && t('operations.grid.job.find.none')}
                {outcome === 'failed' && t('operations.grid.job.find.failed')}
            </span>
        </form>
    );
}

/** The job page: one workunit, the runs of it and the batch it belongs to. */
export function GridJobPage(): ReactNode {
    const { t } = useTranslation();
    const { workunitId = '' } = useParams();
    const workunit = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'job', workunitId],
        queryFn: () => api.gridJobWorkunit(workunitId),
        meta: QUIET,
    });
    const batchId = workunit.data?.batch_id;
    const batch = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'job-batch', batchId],
        queryFn: () => api.gridJobBatch(batchId ?? ''),
        enabled: batchId !== undefined,
        meta: QUIET,
    });
    const runs = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'job-runs', workunitId],
        queryFn: () => api.gridJobResults(workunitId, 0, RUNS_READ_LIMIT),
        enabled: workunit.isSuccess,
        meta: QUIET,
    });
    const trail = [{ label: t('operations.screens.grid'), to: '/operations/grid?tab=jobs' }];
    const shown =
        workunit.data === undefined || runs.data === undefined
            ? undefined
            : focusRun(workunit.data, runs.data.rows);

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" steps={trail} screen={workunitId.slice(0, 8)} />
                <PageHeader
                    title={t('operations.grid.job.title')}
                    description={t('operations.grid.job.description')}
                    actions={
                        batch.data === undefined ? (
                            <LinkButton to="/operations/grid?tab=jobs" variant="secondary">
                                {t('operations.grid.job.back')}
                            </LinkButton>
                        ) : (
                            <LinkButton to={gridBatchPath(batch.data.id)} variant="secondary">
                                {t('operations.grid.job.backToBatch', {
                                    batch: batch.data.report_run_name ?? batch.data.external_ref,
                                })}
                            </LinkButton>
                        )
                    }
                />
            </div>
            <JobFinder />
            {workunit.isPending && (
                <p className="text-sm text-ink-muted">{t('operations.grid.job.loading')}</p>
            )}
            {workunit.isError && (
                <ReadFailure error={workunit.error} onRetry={() => void workunit.refetch()} />
            )}
            {batch.isError && (
                <ReadFailure error={batch.error} onRetry={() => void batch.refetch()} />
            )}
            {workunit.isSuccess && runs.isError && (
                <ReadFailure error={runs.error} onRetry={() => void runs.refetch()} />
            )}
            {workunit.isSuccess && runs.isSuccess && (
                <>
                    {runs.data.total > RUNS_READ_LIMIT && (
                        <Notice tone="warn">
                            {t('operations.grid.job.runs.truncated', {
                                limit: RUNS_READ_LIMIT,
                                total: runs.data.total,
                            })}
                        </Notice>
                    )}
                    <div className="grid gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(280px,2fr)]">
                        <div className="min-w-0 space-y-6">
                            <Header
                                workunit={workunit.data}
                                batch={batch.data}
                                runs={runs.data.rows}
                            />
                            <RunsTable workunit={workunit.data} runs={runs.data.rows} />
                            {shown !== undefined && <TimeBar run={shown} />}
                            <SiblingJobs workunit={workunit.data} />
                        </div>
                        <div className="min-w-0 space-y-6">
                            <Files workunit={workunit.data} runs={runs.data.rows} />
                            <Release workunit={workunit.data} />
                            <Lineage
                                workunit={workunit.data}
                                batch={batch.data}
                                runs={runs.data.rows}
                            />
                        </div>
                    </div>
                </>
            )}
        </div>
    );
}
