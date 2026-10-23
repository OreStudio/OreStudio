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

import type { ReactNode } from 'react';
import { Link, useParams, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { BatchView, ResultView, WorkunitView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { gridAppVersionPath, gridBatchPath, gridJobPath, gridNodePath } from '../shell/paths.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { Detail, LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { formatDateTime, parseTimestamp } from '../ui/Time.js';
import {
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
    const first = runs[0];
    const none = t('operations.grid.jobs.none');
    const failed = runs.some((run) => run.outcome === 3);
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
            <dl className="grid gap-3 sm:grid-cols-3">
                <Detail
                    label={t(`${FIELD}.externalRef`)}
                    value={
                        batch === undefined ? (
                            none
                        ) : (
                            <PageLink to={gridBatchPath(batch.id)}>{batch.external_ref}</PageLink>
                        )
                    }
                />
                <Detail
                    label={t('operations.grid.job.batchStatus')}
                    value={batch === undefined ? none : <StatusTag status={batch.status} />}
                />
                <Detail label={t(`${FIELD}.partyId`)} value={batch?.party_id ?? none} mono />
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
            {failed && <Notice tone="warn">{t('operations.grid.job.failedNote')}</Notice>}
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
    return receivedAt === null
        ? null
        : Math.round(
              (parseTimestamp(receivedAt).getTime() - parseTimestamp(recordedAt).getTime()) / 1000,
          );
}

/** One bar for the first run, from its row being written to its output arriving. */
function TimeBar({ run }: { readonly run: ResultView }): ReactNode {
    const { t, language } = useTranslation();
    const seconds = secondsBetween(run.recorded_at, run.received_at);
    const tone = seconds === null ? 'bg-accent/40' : run.outcome === 3 ? 'bg-down' : 'bg-up';
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
                <Detail label={t(`${FIELD}.inputUri`)} value={workunit.input_uri} mono />
                <Detail label={t(`${FIELD}.configUri`)} value={workunit.config_uri ?? none} mono />
                {runs.map((run, index) => (
                    <Detail
                        key={run.id}
                        label={t('operations.grid.job.files.output', { run: index + 1 })}
                        value={run.output_uri ?? none}
                        mono
                    />
                ))}
            </dl>
        </Panel>
    );
}

function Release({ workunit }: { readonly workunit: WorkunitView }): ReactNode {
    const { t } = useTranslation();
    return (
        <Panel title={t('operations.grid.job.release.title')}>
            <dl>
                <Detail
                    label={t(`${FIELD}.appVersionId`)}
                    value={
                        <PageLink to={gridAppVersionPath(workunit.app_version_id)}>
                            {workunit.app_version_id}
                        </PageLink>
                    }
                    mono
                />
            </dl>
        </Panel>
    );
}

/**
 * What the model states and what it does not.
 *
 * The hop from a batch to the report run behind it is the batch's External Ref,
 * which holds the run's id when a report dispatched it. The model does not state
 * that as a relationship, so the page says it is not a checked one. A batch the
 * shell dispatched holds an operator string there, and has no report behind it.
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
    const { t } = useTranslation();
    return (
        <Panel title={t('operations.grid.job.lineage.title')}>
            <ol className="space-y-2 text-sm">
                {batch !== undefined && (
                    <li>
                        <PageLink to={gridBatchPath(batch.id)}>{batch.external_ref}</PageLink>
                        <p className="text-xs text-ink-faint">
                            {t('operations.grid.job.lineage.batchVia', {
                                ref: batch.external_ref,
                            })}
                        </p>
                    </li>
                )}
                <li>
                    {t('operations.grid.job.lineage.workunit', { id: workunit.id.slice(0, 8) })}
                    <p className="text-xs text-ink-faint">
                        {t('operations.grid.job.lineage.viaBatch')}
                    </p>
                    <ul className="ml-4 list-disc text-xs text-ink-muted">
                        {runs.map((run, index) => (
                            <li key={run.id}>
                                {t('operations.grid.job.lineage.result', {
                                    id: run.id.slice(0, 8),
                                    run: index + 1,
                                })}{' '}
                                <OutcomeTag run={run} />
                            </li>
                        ))}
                    </ul>
                </li>
            </ol>
        </Panel>
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

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" steps={trail} screen={workunitId.slice(0, 8)} />
                <PageHeader
                    title={t('operations.grid.job.title')}
                    description={t('operations.grid.job.description')}
                    actions={
                        <LinkButton to="/operations/grid?tab=jobs" variant="secondary">
                            {t('operations.grid.job.back')}
                        </LinkButton>
                    }
                />
            </div>
            {workunit.isPending && (
                <p className="text-sm text-ink-muted">{t('operations.grid.job.loading')}</p>
            )}
            {workunit.isError && (
                <ReadFailure error={workunit.error} onRetry={() => void workunit.refetch()} />
            )}
            {workunit.isSuccess && runs.isError && (
                <ReadFailure error={runs.error} onRetry={() => void runs.refetch()} />
            )}
            {workunit.isSuccess && runs.isSuccess && (
                <>
                    <Header workunit={workunit.data} batch={batch.data} runs={runs.data.rows} />
                    <RunsTable workunit={workunit.data} runs={runs.data.rows} />
                    {runs.data.rows[0] !== undefined && <TimeBar run={runs.data.rows[0]} />}
                    <SiblingJobs workunit={workunit.data} />
                    <Files workunit={workunit.data} runs={runs.data.rows} />
                    <Release workunit={workunit.data} />
                    <Lineage workunit={workunit.data} batch={batch.data} runs={runs.data.rows} />
                </>
            )}
        </div>
    );
}
