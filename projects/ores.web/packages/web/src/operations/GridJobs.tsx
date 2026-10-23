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
 * The Jobs tab of the grid screen, from
 * doc/knowledge/journeys/compute/journey_browse_the_grids_jobs.org.
 *
 * A paged tree of batches, workunits and results beside a details panel. Every
 * level pages on the server: opening a batch reads one page of its workunits,
 * never all of them. The screen draws the fields the entity models hold under
 * the models' own headers and computes nothing: no count, no total and no state
 * of its own. The paging total is the server's figure.
 *
 * The open rows, the pages and the selection live in the address, so a reload or
 * a shared link shows the same screen.
 */

import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { BatchView, ResultView, WorkunitView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { gridAppVersionPath, gridJobPath, gridNodePath } from '../shell/paths.js';
import { formatDateTime } from '../ui/Time.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { Button, Detail, LinkButton, Notice, Tag } from '../ui/Primitives.js';

/** Where the jobs answers are cached, so a test can seed them and Refresh can read them again. */
export const GRID_JOBS_QUERY_KEY = 'operations-grid-jobs';

/** How many workunits or results a row shows before it pages. */
export const CHILD_PAGE_SIZE = 5;

export type Tone = 'neutral' | 'accent' | 'warn' | 'muted' | 'up' | 'down';

/** The record kinds the tree holds, as the selection names them in the address. */
type Kind = 'batch' | 'workunit' | 'result';

interface Selection {
    readonly kind: Kind;
    readonly id: string;
}

const KINDS: readonly Kind[] = ['batch', 'workunit', 'result'];

/** The selection an address carries, or nothing when it names none. */
export function parseSelection(value: string | null): Selection | null {
    if (value === null) {
        return null;
    }
    const at = value.indexOf(':');
    if (at < 0) {
        return null;
    }
    const kind = KINDS.find((candidate) => candidate === value.slice(0, at));
    const id = value.slice(at + 1);
    return kind === undefined || id === '' ? null : { kind, id };
}

/**
 * batch.status: open, processing, assimilating and closed in the model, and
 * dispatched, which the dispatch command writes. A status the screen does not know is
 * drawn as the server stated it.
 */
const BATCH_STATUS_TONE: Readonly<Record<string, Tone>> = {
    open: 'neutral',
    dispatched: 'accent',
    processing: 'accent',
    assimilating: 'accent',
    closed: 'up',
};

/** result.server_state: 1=Inactive, 2=Unsent, 4=InProgress, 5=Done. */
export const SERVER_STATE: Readonly<
    Record<number, { readonly name: string; readonly tone: Tone }>
> = {
    1: { name: 'inactive', tone: 'muted' },
    2: { name: 'unsent', tone: 'neutral' },
    4: { name: 'inProgress', tone: 'accent' },
    5: { name: 'done', tone: 'up' },
};

/** result.outcome: 1=Success, 3=ClientError, 4=NoReply. Any other value is not set. */
export const OUTCOME: Readonly<Record<number, { readonly name: string; readonly tone: Tone }>> = {
    1: { name: 'success', tone: 'up' },
    3: { name: 'clientError', tone: 'down' },
    4: { name: 'noReply', tone: 'warn' },
};

/** The address of the tab: the parameters it holds and the way to change them. */
function useJobsAddress(): {
    readonly params: URLSearchParams;
    readonly hrefWith: (patch: Readonly<Record<string, string | null>>) => string;
    readonly set: (patch: Readonly<Record<string, string | null>>) => void;
} {
    const [params, setParams] = useSearchParams();
    const merged = (patch: Readonly<Record<string, string | null>>): URLSearchParams => {
        const next = new URLSearchParams(params);
        for (const [key, value] of Object.entries(patch)) {
            if (value === null || value === '') {
                next.delete(key);
            } else {
                next.set(key, value);
            }
        }
        return next;
    };
    return {
        params,
        hrefWith: (patch) => `?${merged(patch).toString()}`,
        set: (patch) => setParams(merged(patch), { replace: true }),
    };
}

/** A whole number of at least one from the address, or the fallback. */
export function numberParam(value: string | null, fallback: number): number {
    const parsed = Number(value);
    return Number.isInteger(parsed) && parsed >= 1 ? parsed : fallback;
}

function openIds(params: URLSearchParams): ReadonlySet<string> {
    return new Set((params.get('open') ?? '').split(',').filter((id) => id !== ''));
}

function isRefused(error: unknown): boolean {
    return error instanceof ApiFailure && error.status === 403;
}

/** A read the tab states itself, so the shell raises no banner for it. */
export const QUIET = { quiet: true } as const;

export function ReadFailure({
    error,
    onRetry,
}: {
    readonly error: unknown;
    readonly onRetry: () => void;
}): ReactNode {
    const { t } = useTranslation();
    if (isRefused(error)) {
        return <Notice tone="warn">{t('operations.grid.jobs.refused')}</Notice>;
    }
    const message = error instanceof Error ? error.message : '';
    if (error instanceof ApiFailure && error.status === 404) {
        return <Notice tone="warn">{message}</Notice>;
    }
    return (
        <Notice tone="error">
            <span>{t('operations.grid.jobs.error', { message })}</span>{' '}
            <Button size="sm" onClick={onRetry}>
                {t('operations.grid.jobs.retry')}
            </Button>
        </Notice>
    );
}

export function StatusTag({ status }: { readonly status: string }): ReactNode {
    const { t } = useTranslation();
    const known = status in BATCH_STATUS_TONE;
    const label = known ? t(`operations.grid.jobs.status.${status}`) : status;
    return <Tag tone={BATCH_STATUS_TONE[status] ?? 'neutral'}>{label}</Tag>;
}

export function ResultTags({ result }: { readonly result: ResultView }): ReactNode {
    const { t } = useTranslation();
    const state = SERVER_STATE[result.server_state];
    const outcome = OUTCOME[result.outcome];
    return (
        <span className="flex gap-1">
            {state !== undefined && (
                <Tag tone={state.tone}>{t(`operations.grid.jobs.serverState.${state.name}`)}</Tag>
            )}
            {outcome !== undefined && (
                <Tag tone={outcome.tone}>{t(`operations.grid.jobs.outcome.${outcome.name}`)}</Tag>
            )}
        </span>
    );
}

/**
 * One row of the tree: a button that selects the record and, for a parent, opens it.
 *
 * The children of an open row sit inside its list item, so the list nests as the
 * tree does. The full id is the row's title, because the label is only its start.
 */
function TreeRow({
    label,
    title,
    note,
    tags,
    twist,
    selected,
    onClick,
    children,
}: {
    readonly label: string;
    readonly title?: string | undefined;
    readonly note?: string | undefined;
    readonly tags?: ReactNode;
    /** Open or closed for a parent, and nothing for a leaf. */
    readonly twist?: 'open' | 'closed';
    readonly selected: boolean;
    readonly onClick: () => void;
    readonly children?: ReactNode;
}): ReactNode {
    return (
        <li>
            <button
                type="button"
                title={title}
                aria-expanded={twist === undefined ? undefined : twist === 'open'}
                aria-current={selected ? 'true' : undefined}
                className={`flex w-full items-center justify-between gap-3 rounded px-2 py-1.5 text-left text-sm hover:bg-bg-hover ${
                    selected ? 'bg-bg-hover' : ''
                }`}
                onClick={onClick}
            >
                <span className="flex min-w-0 items-center gap-2">
                    <span aria-hidden="true" className="w-3 text-ink-faint">
                        {twist === undefined ? '' : twist === 'open' ? '▾' : '▸'}
                    </span>
                    <span className="truncate font-mono text-xs">{label}</span>
                </span>
                <span className="flex shrink-0 items-center gap-3 text-xs text-ink-muted">
                    {note !== undefined && <span>{note}</span>}
                    {tags}
                </span>
            </button>
            {children}
        </li>
    );
}

function showingText(
    t: Translator['t'],
    key: string,
    offset: number,
    shown: number,
    total: number,
): string {
    const { first, last } = pageBounds(offset, shown);
    return t(key, { first, last, total });
}

/** The way back when an address names a page past the last one. */
function PastTheLastPage({
    param,
    address,
}: {
    readonly param: string;
    readonly address: ReturnType<typeof useJobsAddress>;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <p className="px-2 text-xs text-ink-muted">
            {t('operations.grid.jobs.pastTheLastPage')}{' '}
            <Button size="sm" onClick={() => address.set({ [param]: null })}>
                {t('operations.grid.jobs.firstPage')}
            </Button>
        </p>
    );
}

/** Opens a parent that is closed and closes one that is open, naming the one opened. */
function toggled(
    open: ReadonlySet<string>,
    id: string,
    select: string,
): Readonly<Record<string, string | null>> {
    const next = new Set(open);
    if (next.has(id)) {
        next.delete(id);
        return { open: [...next].join(',') };
    }
    next.add(id);
    return { open: [...next].join(','), sel: select };
}

function ResultRows({ workunit }: { readonly workunit: WorkunitView }): ReactNode {
    const { t } = useTranslation();
    const address = useJobsAddress();
    const param = `rp.${workunit.id}`;
    const page = numberParam(address.params.get(param), 1);
    const offset = (page - 1) * CHILD_PAGE_SIZE;
    const results = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'results', workunit.id, offset],
        queryFn: () => api.gridJobResults(workunit.id, offset, CHILD_PAGE_SIZE),
        meta: QUIET,
    });
    const selection = parseSelection(address.params.get('sel'));

    if (results.isError) {
        return <ReadFailure error={results.error} onRetry={() => void results.refetch()} />;
    }
    if (results.isPending) {
        return (
            <p className="px-2 text-xs text-ink-muted">
                {t('operations.grid.jobs.loadingResults')}
            </p>
        );
    }
    const rows = results.data.rows;
    if (rows.length === 0) {
        return results.data.total > 0 ? (
            <PastTheLastPage param={param} address={address} />
        ) : (
            <p className="px-2 text-xs text-ink-muted">{t('operations.grid.jobs.noResults')}</p>
        );
    }
    return (
        <>
            <ul className="ml-6 border-l border-line-subtle pl-2">
                {rows.map((result) => (
                    <TreeRow
                        key={result.id}
                        label={result.id.slice(0, 8)}
                        title={result.id}
                        tags={<ResultTags result={result} />}
                        selected={selection?.kind === 'result' && selection.id === result.id}
                        onClick={() => address.set({ sel: `result:${result.id}` })}
                    />
                ))}
            </ul>
            {results.data.total > CHILD_PAGE_SIZE && (
                <div className="ml-6 pl-2">
                    <Pager
                        offset={offset}
                        shown={rows.length}
                        total={results.data.total}
                        pageSize={CHILD_PAGE_SIZE}
                        showing={showingText(
                            t,
                            'operations.grid.jobs.resultsShowing',
                            offset,
                            rows.length,
                            results.data.total,
                        )}
                        onMove={(next) =>
                            address.set({ [param]: String(next / CHILD_PAGE_SIZE + 1) })
                        }
                    />
                </div>
            )}
        </>
    );
}

function WorkunitRows({ batch }: { readonly batch: BatchView }): ReactNode {
    const { t } = useTranslation();
    const address = useJobsAddress();
    const param = `wp.${batch.id}`;
    const page = numberParam(address.params.get(param), 1);
    const offset = (page - 1) * CHILD_PAGE_SIZE;
    const workunits = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'workunits', batch.id, offset],
        queryFn: () => api.gridJobWorkunits(batch.id, offset, CHILD_PAGE_SIZE),
        meta: QUIET,
    });
    const open = openIds(address.params);
    const selection = parseSelection(address.params.get('sel'));

    if (workunits.isError) {
        return <ReadFailure error={workunits.error} onRetry={() => void workunits.refetch()} />;
    }
    if (workunits.isPending) {
        return (
            <p className="px-2 text-xs text-ink-muted">
                {t('operations.grid.jobs.loadingWorkunits')}
            </p>
        );
    }
    const rows = workunits.data.rows;
    if (rows.length === 0) {
        return workunits.data.total > 0 ? (
            <PastTheLastPage param={param} address={address} />
        ) : (
            <p className="px-2 text-xs text-ink-muted">{t('operations.grid.jobs.noWorkunits')}</p>
        );
    }
    return (
        <>
            <ul className="ml-6 border-l border-line-subtle pl-2">
                {rows.map((workunit) => (
                    <TreeRow
                        key={workunit.id}
                        label={workunit.id.slice(0, 8)}
                        title={workunit.id}
                        note={t('operations.grid.jobs.workunitNote', {
                            priority: workunit.priority,
                            redundancy: workunit.target_redundancy,
                        })}
                        twist={open.has(workunit.id) ? 'open' : 'closed'}
                        selected={selection?.kind === 'workunit' && selection.id === workunit.id}
                        onClick={() =>
                            address.set(toggled(open, workunit.id, `workunit:${workunit.id}`))
                        }
                    >
                        {open.has(workunit.id) && <ResultRows workunit={workunit} />}
                    </TreeRow>
                ))}
            </ul>
            {workunits.data.total > CHILD_PAGE_SIZE && (
                <div className="ml-6 pl-2">
                    <Pager
                        offset={offset}
                        shown={rows.length}
                        total={workunits.data.total}
                        pageSize={CHILD_PAGE_SIZE}
                        showing={showingText(
                            t,
                            'operations.grid.jobs.workunitsShowing',
                            offset,
                            rows.length,
                            workunits.data.total,
                        )}
                        onMove={(next) =>
                            address.set({ [param]: String(next / CHILD_PAGE_SIZE + 1) })
                        }
                    />
                </div>
            )}
        </>
    );
}

function Tree(): ReactNode {
    const { t } = useTranslation();
    const address = useJobsAddress();
    const size = numberParam(address.params.get('size'), DEFAULT_PAGE_SIZE);
    const page = numberParam(address.params.get('page'), 1);
    const offset = (page - 1) * size;
    const batches = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'batches', offset, size],
        queryFn: () => api.gridJobBatches(offset, size),
        meta: QUIET,
    });
    const open = openIds(address.params);
    const selection = parseSelection(address.params.get('sel'));

    if (batches.isError) {
        return <ReadFailure error={batches.error} onRetry={() => void batches.refetch()} />;
    }
    if (batches.isPending) {
        return <p className="text-sm text-ink-muted">{t('operations.grid.jobs.loadingBatches')}</p>;
    }
    const rows = batches.data.rows;
    if (batches.data.total === 0) {
        return <Notice>{t('operations.grid.jobs.empty')}</Notice>;
    }
    return (
        <>
            {rows.length === 0 ? (
                <PastTheLastPage param="page" address={address} />
            ) : (
                <ul>
                    {rows.map((batch) => (
                        <TreeRow
                            key={batch.id}
                            label={batch.external_ref}
                            title={batch.id}
                            tags={<StatusTag status={batch.status} />}
                            twist={open.has(batch.id) ? 'open' : 'closed'}
                            selected={selection?.kind === 'batch' && selection.id === batch.id}
                            onClick={() =>
                                address.set(toggled(open, batch.id, `batch:${batch.id}`))
                            }
                        >
                            {open.has(batch.id) && <WorkunitRows batch={batch} />}
                        </TreeRow>
                    ))}
                </ul>
            )}
            <Pager
                offset={offset}
                shown={rows.length}
                total={batches.data.total}
                pageSize={size}
                showing={showingText(
                    t,
                    'operations.grid.jobs.batchesShowing',
                    offset,
                    rows.length,
                    batches.data.total,
                )}
                onMove={(next) => address.set({ page: String(Math.floor(next / size) + 1) })}
                onPageSize={(next) => address.set({ size: String(next), page: null })}
            />
        </>
    );
}

/** A reference that opens another record or screen without leaving the application. */
function RouteLink({
    to,
    children,
}: {
    readonly to: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <Link
            to={to}
            className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
        >
            {children}
        </Link>
    );
}

/** A value, or the word that says there is none. */
function orNone(value: string | null, none: string): string {
    return value === null || value === '' ? none : value;
}

function BatchDetails({ batch }: { readonly batch: BatchView }): ReactNode {
    const { t, language } = useTranslation();
    const none = t('operations.grid.jobs.none');
    return (
        <>
            <Detail
                label={t('operations.grid.jobs.fields.externalRef')}
                value={batch.external_ref}
            />
            <Detail
                label={t('operations.grid.jobs.fields.partyId')}
                value={orNone(batch.party_id, none)}
                mono
            />
            <Detail
                label={t('operations.grid.jobs.fields.status')}
                value={<StatusTag status={batch.status} />}
            />
            <Detail label={t('operations.grid.jobs.fields.modifiedBy')} value={batch.modified_by} />
            <Detail
                label={t('operations.grid.jobs.fields.recordedAt')}
                value={formatDateTime(batch.recorded_at, language)}
            />
        </>
    );
}

function WorkunitDetails({ workunit }: { readonly workunit: WorkunitView }): ReactNode {
    const { t, language } = useTranslation();
    const address = useJobsAddress();
    const none = t('operations.grid.jobs.none');
    return (
        <>
            <Detail
                label={t('operations.grid.jobs.fields.batchId')}
                value={
                    <RouteLink to={address.hrefWith({ sel: `batch:${workunit.batch_id}` })}>
                        {workunit.batch_id}
                    </RouteLink>
                }
                mono
            />
            <Detail
                label={t('operations.grid.jobs.fields.appVersionId')}
                value={
                    <RouteLink to={gridAppVersionPath(workunit.app_version_id)}>
                        {workunit.app_version_id}
                    </RouteLink>
                }
                mono
            />
            <Detail
                label={t('operations.grid.jobs.fields.inputUri')}
                value={workunit.input_uri}
                mono
            />
            <Detail
                label={t('operations.grid.jobs.fields.configUri')}
                value={orNone(workunit.config_uri, none)}
                mono
            />
            <Detail label={t('operations.grid.jobs.fields.priority')} value={workunit.priority} />
            <Detail
                label={t('operations.grid.jobs.fields.targetRedundancy')}
                value={workunit.target_redundancy}
            />
            {workunit.canonical_result_id === null ? (
                <Detail label={t('operations.grid.jobs.fields.canonicalResultId')} value={none} />
            ) : (
                <Detail
                    label={t('operations.grid.jobs.fields.canonicalResultId')}
                    value={
                        <RouteLink
                            to={address.hrefWith({ sel: `result:${workunit.canonical_result_id}` })}
                        >
                            {workunit.canonical_result_id}
                        </RouteLink>
                    }
                    mono
                />
            )}
            <Detail
                label={t('operations.grid.jobs.fields.modifiedBy')}
                value={workunit.modified_by}
            />
            <Detail
                label={t('operations.grid.jobs.fields.recordedAt')}
                value={formatDateTime(workunit.recorded_at, language)}
            />
        </>
    );
}

function ResultDetails({ result }: { readonly result: ResultView }): ReactNode {
    const { t, language } = useTranslation();
    const address = useJobsAddress();
    const none = t('operations.grid.jobs.none');
    const state = SERVER_STATE[result.server_state];
    const outcome = OUTCOME[result.outcome];
    return (
        <>
            <Detail
                label={t('operations.grid.jobs.fields.workunitId')}
                value={
                    <RouteLink to={address.hrefWith({ sel: `workunit:${result.workunit_id}` })}>
                        {result.workunit_id}
                    </RouteLink>
                }
                mono
            />
            {result.host_hidden ? (
                <Detail
                    label={t('operations.grid.jobs.fields.hostId')}
                    value={t('operations.grid.jobs.hostHidden')}
                />
            ) : result.host_id === null ? (
                <Detail label={t('operations.grid.jobs.fields.hostId')} value={none} />
            ) : (
                <Detail
                    label={t('operations.grid.jobs.fields.hostId')}
                    value={
                        <RouteLink to={gridNodePath(result.host_id)}>{result.host_id}</RouteLink>
                    }
                    mono
                />
            )}
            <Detail
                label={t('operations.grid.jobs.fields.serverState')}
                value={
                    state === undefined ? (
                        String(result.server_state)
                    ) : (
                        <Tag tone={state.tone}>
                            {t(`operations.grid.jobs.serverState.${state.name}`)}
                        </Tag>
                    )
                }
            />
            <Detail
                label={t('operations.grid.jobs.fields.outcome')}
                value={
                    outcome === undefined ? (
                        t('operations.grid.jobs.outcomeNotSet')
                    ) : (
                        <Tag tone={outcome.tone}>
                            {t(`operations.grid.jobs.outcome.${outcome.name}`)}
                        </Tag>
                    )
                }
            />
            <Detail
                label={t('operations.grid.jobs.fields.outputUri')}
                value={orNone(result.output_uri, none)}
                mono
            />
            <Detail
                label={t('operations.grid.jobs.fields.errorMessage')}
                value={orNone(result.error_message, none)}
            />
            <Detail
                label={t('operations.grid.jobs.fields.receivedAt')}
                value={
                    result.received_at === null
                        ? none
                        : formatDateTime(result.received_at, language)
                }
            />
            <Detail
                label={t('operations.grid.jobs.fields.modifiedBy')}
                value={result.modified_by}
            />
            <Detail
                label={t('operations.grid.jobs.fields.recordedAt')}
                value={formatDateTime(result.recorded_at, language)}
            />
        </>
    );
}

/** The record the details panel draws, tagged with the kind that says which it is. */
type SelectedRecord =
    | { readonly kind: 'batch'; readonly data: BatchView }
    | { readonly kind: 'workunit'; readonly data: WorkunitView }
    | { readonly kind: 'result'; readonly data: ResultView };

function Details(): ReactNode {
    const { t } = useTranslation();
    const address = useJobsAddress();
    const selection = parseSelection(address.params.get('sel'));
    const record = useQuery({
        queryKey: [GRID_JOBS_QUERY_KEY, 'record', selection?.kind, selection?.id],
        queryFn: async (): Promise<SelectedRecord> => {
            if (selection === null) {
                throw new Error('No record is selected.');
            }
            switch (selection.kind) {
                case 'batch':
                    return { kind: 'batch', data: await api.gridJobBatch(selection.id) };
                case 'workunit':
                    return { kind: 'workunit', data: await api.gridJobWorkunit(selection.id) };
                case 'result':
                    return { kind: 'result', data: await api.gridJobResult(selection.id) };
            }
        },
        enabled: selection !== null,
        meta: QUIET,
    });

    if (selection === null) {
        return <p className="text-sm text-ink-muted">{t('operations.grid.jobs.details.none')}</p>;
    }
    if (record.isError) {
        return <ReadFailure error={record.error} onRetry={() => void record.refetch()} />;
    }
    if (record.isPending) {
        return (
            <p className="text-sm text-ink-muted">{t('operations.grid.jobs.details.loading')}</p>
        );
    }
    const selected = record.data;
    return (
        <div>
            <p className="text-xs text-ink-faint">{t('operations.grid.jobs.fields.id')}</p>
            <p className="mb-3 break-all font-mono text-sm">{selected.data.id}</p>
            <dl className="grid gap-3 sm:grid-cols-2">
                {selected.kind === 'batch' && <BatchDetails batch={selected.data} />}
                {selected.kind === 'workunit' && <WorkunitDetails workunit={selected.data} />}
                {selected.kind === 'result' && <ResultDetails result={selected.data} />}
            </dl>
            {selected.kind !== 'batch' && (
                <div className="mt-4">
                    <LinkButton
                        to={gridJobPath(
                            selected.kind === 'workunit'
                                ? selected.data.id
                                : selected.data.workunit_id,
                        )}
                        variant="secondary"
                    >
                        {t('operations.grid.jobs.follow')}
                    </LinkButton>
                </div>
            )}
        </div>
    );
}

/** The Jobs tab. */
export function GridJobsTab(): ReactNode {
    const { t } = useTranslation();
    const address = useJobsAddress();
    const selection = parseSelection(address.params.get('sel'));
    return (
        <div className="grid gap-6 lg:grid-cols-2">
            <section aria-label={t('operations.grid.jobs.treeTitle')}>
                <h2 className="mb-3 text-sm font-medium">{t('operations.grid.jobs.treeTitle')}</h2>
                <Tree />
            </section>
            <section aria-label={t('operations.grid.jobs.details.record')}>
                <div className="mb-3 flex items-center justify-between">
                    <h2 className="text-sm font-medium">
                        {t(`operations.grid.jobs.details.${selection?.kind ?? 'record'}`)}
                    </h2>
                    {selection !== null && (
                        <Tag small tone="muted">
                            {t('operations.grid.jobs.details.readOnly')}
                        </Tag>
                    )}
                </div>
                <Details />
            </section>
        </div>
    );
}
