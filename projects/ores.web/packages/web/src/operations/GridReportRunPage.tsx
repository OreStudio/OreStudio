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

import type { ReactNode } from 'react';
import { useMemo, useState } from 'react';
import { Link, useParams, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { GridReportRunView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { HealthTag } from '../reporting/shared.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { gridBatchPath, reportPath } from '../shell/paths.js';
import { Detail, LinkButton, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { formatDateTime } from '../ui/Time.js';
import { ReadFailure } from './GridJobs.js';

/**
 * The report run page: the run of a report that dispatched a batch, and the
 * errors that run recorded.
 *
 * The page has two screens. *Run* states the run and, when it failed, says in
 * one line what went wrong and how many errors there are. *Errors* is where the
 * work happens: the same errors either grouped by the curve that could not be
 * built, or listed as rows beside the one being read.
 *
 * A batch names its run in its External Ref, and the job page links here. The
 * run belongs to a party, so a session that may not see the party gets the same
 * answer as a run that does not exist.
 */

/** Where the page's answer is cached, so a test can seed it. */
export const REPORT_RUN_QUERY_KEY = 'operations-grid-report-run';

/** Where a run's errors are cached, so the summary and the views share one read. */
export const REPORT_RUN_ERRORS_QUERY_KEY = 'operations-grid-report-run-errors';

const FIELD = 'operations.grid.reportRun';

const TABS = ['run', 'errors'] as const;

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

/** One error a run recorded, shaped for the two views. */
type RunError = {
    readonly id: string;
    readonly scope: string;
    readonly severity: string;
    readonly message: string;
    readonly subject: string;
    readonly subjectId: string;
    readonly version: number | null;
    readonly asOf: string;
    readonly time: string;
};

const asText = (value: unknown): string => (typeof value === 'string' ? value : '');

const toError = (raw: unknown): RunError => {
    const row = raw as Record<string, unknown>;
    return {
        id: asText(row['id']),
        scope: asText(row['scope']),
        severity: asText(row['severity']),
        message: asText(row['message']),
        subject: asText(row['subject']),
        subjectId: asText(row['subject_id']),
        version: typeof row['subject_version'] === 'number' ? row['subject_version'] : null,
        asOf: asText(row['subject_as_of']),
        time: asText(row['time']),
    };
};

/** A curve's name, from the id the engine states for it. */
const curveNameOf = (value: string): string => {
    const stated = /IndexCurve\([^,]*,\s*([^)]*)\)/.exec(value)?.[1];
    if (stated !== undefined) return stated.trim();
    return value.replace(/^\[/, '').replace(/\]$/, '');
};

/** The pricing date the engine worked at, when a message states one. */
const asOfDateOf = (rows: readonly RunError[]): string => {
    for (const row of rows) {
        const stated = /as of date (\d{4}-\d{2}-\d{2})/.exec(row.message)?.[1];
        if (stated !== undefined) return stated;
    }
    return '';
};

/** The first sentence of a message, for a line that must stay short. */
const firstSentenceOf = (message: string): string => {
    const cut = message.search(/\.\s/);
    const sentence = cut === -1 ? message : message.slice(0, cut + 1);
    return sentence.length > 160 ? `${sentence.slice(0, 157)}...` : sentence;
};

/** What the error is about, in a person's words rather than the engine's kind. */
const labelOf = (error: RunError): string => {
    if (error.scope === 'Trade') {
        const curve = /IndexCurve\([^,]*,\s*([^)]*)\)/.exec(error.message)?.[1];
        if (curve !== undefined) return curve.trim();
        return error.subjectId !== '' ? error.subjectId : 'a trade';
    }
    if (error.scope === 'Curve') {
        return curveNameOf(error.subjectId !== '' ? error.subjectId : error.message);
    }
    if (error.subjectId !== '') return error.subjectId;
    return firstSentenceOf(error.message);
};

const tradeHref = (error: RunError): string =>
    `/trades?trade=${error.subjectId}${error.version === null ? '' : `&version=${error.version}`}`;

/** The errors of one run, read once and shared by the two screens. */
function useRunErrors(id: string) {
    const query = useQuery({
        queryKey: [REPORT_RUN_ERRORS_QUERY_KEY, id],
        queryFn: () => api.gridReportRunErrors(id),
        meta: { quiet: true },
    });
    const rows = useMemo(() => (query.data ?? []).map(toError), [query.data]);
    return { query, rows };
}

/** A filter chip: the count it filters to, and whether it is the one chosen. */
function Chip({
    chosen,
    onClick,
    children,
}: {
    readonly chosen: boolean;
    readonly onClick: () => void;
    readonly children: ReactNode;
}) {
    return (
        <button
            type="button"
            aria-pressed={chosen}
            onClick={onClick}
            className={cx(
                'rounded-full border px-2 py-0.5 text-[11px] leading-tight',
                chosen
                    ? 'border-accent/50 bg-accent/10 text-accent-bright'
                    : 'border-line text-ink-muted hover:text-ink',
            )}
        >
            {children}
        </button>
    );
}

/** What the run did, in the words a person triaging it needs. */
function RunSummary({
    rows,
    onShowErrors,
}: {
    readonly rows: readonly RunError[];
    readonly onShowErrors: () => void;
}) {
    const curves = [
        ...new Set(
            rows
                .filter((error) => error.scope === 'Curve')
                .map((error) =>
                    curveNameOf(error.subjectId !== '' ? error.subjectId : error.message),
                ),
        ),
    ];
    const priced = asOfDateOf(rows);
    if (rows.length === 0) return null;

    return (
        <section className="card space-y-2 p-6">
            <p className="text-sm">
                {curves.length === 0
                    ? `This run recorded ${rows.length} problem(s).`
                    : `Market data is missing for ${curves.length} curve(s).`}
            </p>
            {priced !== '' && (
                <p className="text-sm text-ink-muted">
                    {`Priced at ${priced}, the last date the gathered market data carries.`}
                </p>
            )}
            <button type="button" onClick={onShowErrors} className="text-sm underline">
                {`View all ${rows.length} errors →`}
            </button>
        </section>
    );
}

/** The run's errors: grouped by the curve that failed, or as rows. */
function RunErrors({ id, outputMessage }: { readonly id: string; readonly outputMessage: string }) {
    const { language } = useTranslation();
    const { query, rows } = useRunErrors(id);
    const [view, setView] = useState<'grouped' | 'split'>('grouped');
    const [severity, setSeverity] = useState('all');
    const [scope, setScope] = useState('all');
    const [search, setSearch] = useState('');
    const [selected, setSelected] = useState<string | null>(null);

    const errors = rows.filter((row) => row.severity === 'error').length;
    const warnings = rows.filter((row) => row.severity === 'warning').length;
    const scopes = [...new Set(rows.map((row) => row.scope))];

    const shown = useMemo(
        () =>
            rows.filter((error) => {
                if (severity !== 'all' && error.severity !== severity) return false;
                if (scope !== 'all' && error.scope !== scope) return false;
                const needle = search.trim().toLowerCase();
                if (needle === '') return true;
                return [error.message, error.subjectId, error.subject]
                    .join(' ')
                    .toLowerCase()
                    .includes(needle);
            }),
        [rows, severity, scope, search],
    );

    const groups = useMemo(() => {
        const curves = shown.filter((error) => error.scope === 'Curve');
        const trades = shown.filter((error) => error.scope === 'Trade');
        return curves.map((curve) => {
            const name = curveNameOf(curve.subjectId !== '' ? curve.subjectId : curve.message);
            const matching = trades.filter((trade) => trade.message.includes(name));
            const unique = [...new Map(matching.map((trade) => [trade.subjectId, trade])).values()];
            return { name, curve, trades: unique };
        });
    }, [shown]);

    const others = shown.filter((error) => error.scope !== 'Curve' && error.scope !== 'Trade');

    if (query.isPending) return null;
    if (query.isError) {
        return <ReadFailure error={query.error} onRetry={() => void query.refetch()} />;
    }
    if (rows.length === 0) {
        return (
            <section className="space-y-2">
                <Notice tone="success">This run recorded no errors.</Notice>
                {outputMessage !== '' && (
                    <pre className="whitespace-pre-wrap font-mono text-xs text-ink-muted">
                        {outputMessage}
                    </pre>
                )}
            </section>
        );
    }

    const distinct = [
        ...new Map(
            shown.map((error) => [`${error.scope}|${error.subjectId}|${error.message}`, error]),
        ).values(),
    ];
    const current = shown.find((error) => error.id === selected) ?? shown[0] ?? null;

    return (
        <section className="space-y-4">
            <div className="flex flex-wrap items-center gap-2">
                <Chip chosen={severity === 'all'} onClick={() => setSeverity('all')}>
                    {`All ${rows.length}`}
                </Chip>
                <Chip chosen={severity === 'error'} onClick={() => setSeverity('error')}>
                    {`Errors ${errors}`}
                </Chip>
                <Chip chosen={severity === 'warning'} onClick={() => setSeverity('warning')}>
                    {`Warnings ${warnings}`}
                </Chip>
                <select
                    aria-label="Scope"
                    value={scope}
                    onChange={(event) => setScope(event.target.value)}
                    className="rounded border border-line bg-transparent px-2 py-1 text-xs"
                >
                    <option value="all">All scopes</option>
                    {scopes.map((name) => (
                        <option key={name} value={name}>
                            {name}
                        </option>
                    ))}
                </select>
                <input
                    type="search"
                    aria-label="Search the run's errors"
                    placeholder="Search message, curve or trade id"
                    value={search}
                    onChange={(event) => setSearch(event.target.value)}
                    className="min-w-56 flex-1 rounded border border-line bg-transparent px-2 py-1 text-xs"
                />
                <div className="ml-auto flex gap-1">
                    <Chip chosen={view === 'grouped'} onClick={() => setView('grouped')}>
                        Grouped
                    </Chip>
                    <Chip chosen={view === 'split'} onClick={() => setView('split')}>
                        Split
                    </Chip>
                </div>
            </div>

            {view === 'grouped' && (
                <ul className="space-y-3">
                    {groups.map((group) => (
                        <li key={group.curve.id} className="card space-y-2 p-4">
                            <div className="flex flex-wrap items-center gap-2">
                                <Tag tone={group.curve.severity === 'error' ? 'down' : 'warn'}>
                                    {group.curve.severity}
                                </Tag>
                                <p className="font-mono text-sm">{group.name}</p>
                                <p className="text-xs text-ink-muted">
                                    {`${group.trades.length} trade(s)`}
                                </p>
                            </div>
                            <pre className="whitespace-pre-wrap font-mono text-xs text-ink-muted">
                                {group.curve.message}
                            </pre>
                            {group.trades.length > 0 && (
                                <div className="flex flex-wrap gap-2">
                                    {group.trades.map((trade) => (
                                        <a
                                            key={trade.subjectId}
                                            href={tradeHref(trade)}
                                            title={`Open ${trade.subjectId} at version ${trade.version ?? 'unknown'}`}
                                            className="rounded-full border border-line px-2 py-0.5 font-mono text-[11px] hover:border-accent"
                                        >
                                            {`${trade.subjectId.slice(0, 8)}… · v${trade.version ?? '?'}${trade.asOf === '' ? '' : ` @ ${trade.asOf}`}`}
                                        </a>
                                    ))}
                                </div>
                            )}
                        </li>
                    ))}
                    {others.length > 0 && (
                        <li className="card space-y-2 p-4">
                            <p className="text-sm">Analytic and report failures</p>
                            <ul className="space-y-1">
                                {others.map((other) => (
                                    <li key={other.id} className="flex gap-2 font-mono text-xs">
                                        <Tag
                                            tone={other.severity === 'error' ? 'down' : 'warn'}
                                            small
                                        >
                                            {other.severity}
                                        </Tag>
                                        <span className="text-ink-muted">
                                            {other.message.startsWith("The run's compute job")
                                                ? firstSentenceOf(other.message)
                                                : other.message}
                                        </span>
                                    </li>
                                ))}
                            </ul>
                        </li>
                    )}
                </ul>
            )}

            {view === 'split' && (
                <div className="grid gap-4 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
                    <div className="card overflow-x-auto p-2">
                        {distinct.length < shown.length && (
                            <p className="px-2 pb-2 text-xs text-ink-muted">
                                {`${distinct.length} distinct reasons of ${shown.length} recorded rows.`}
                            </p>
                        )}
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="text-xs uppercase tracking-wide text-ink-faint">
                                    <th className="px-2 py-1">Severity</th>
                                    <th className="px-2 py-1">Scope</th>
                                    <th className="px-2 py-1">Subject</th>
                                    <th className="px-2 py-1">Trade</th>
                                    <th className="px-2 py-1">Version</th>
                                    <th className="px-2 py-1">Time</th>
                                </tr>
                            </thead>
                            <tbody>
                                {[...distinct]
                                    .sort((left, right) => {
                                        const rank = (error: RunError): number =>
                                            error.scope === 'Curve'
                                                ? 0
                                                : error.scope === 'Trade'
                                                  ? 1
                                                  : 2;
                                        return rank(left) - rank(right);
                                    })
                                    .map((error) => (
                                        <tr
                                            key={error.id}
                                            onClick={() => setSelected(error.id)}
                                            className={cx(
                                                'cursor-pointer border-t border-line-subtle',
                                                current?.id === error.id && 'bg-ink/5',
                                            )}
                                        >
                                            <td className="px-2 py-1">
                                                <Tag
                                                    tone={
                                                        error.severity === 'error' ? 'down' : 'warn'
                                                    }
                                                    small
                                                >
                                                    {error.severity}
                                                </Tag>
                                            </td>
                                            <td className="px-2 py-1">{error.scope}</td>
                                            <td className="px-2 py-1 font-mono">
                                                {labelOf(error)}
                                            </td>
                                            <td className="px-2 py-1 font-mono">
                                                {error.scope !== 'Trade' ||
                                                error.subjectId === '' ? (
                                                    '—'
                                                ) : (
                                                    <a
                                                        href={tradeHref(error)}
                                                        onClick={(event) => event.stopPropagation()}
                                                        className="underline"
                                                    >
                                                        {error.subjectId}
                                                    </a>
                                                )}
                                            </td>
                                            <td className="px-2 py-1 font-mono text-xs">
                                                {error.version === null
                                                    ? '—'
                                                    : `v${error.version}${error.asOf === '' ? '' : ` @ ${error.asOf}`}`}
                                            </td>
                                            <td className="whitespace-nowrap px-2 py-1 font-mono text-xs">
                                                {formatDateTime(error.time, language)}
                                            </td>
                                        </tr>
                                    ))}
                            </tbody>
                        </table>
                    </div>
                    {current !== null && (
                        <aside className="card space-y-3 p-4">
                            <p className="font-mono text-sm">{labelOf(current)}</p>
                            <p className="text-xs text-ink-muted">
                                {`${current.severity} · ${current.scope} · ${formatDateTime(current.time, language)}`}
                            </p>
                            <pre className="whitespace-pre-wrap font-mono text-xs">
                                {current.message}
                            </pre>
                            {current.scope === 'Trade' && current.subjectId !== '' && (
                                <p className="space-y-1 font-mono text-xs">
                                    <span className="block break-all">{current.subjectId}</span>
                                    <span className="block text-ink-muted">
                                        {`version ${current.version ?? 'unknown'}${current.asOf === '' ? '' : ` · as of ${current.asOf}`}`}
                                    </span>
                                    <a className="underline" href={tradeHref(current)}>
                                        {`Open the trade at version ${current.version ?? 'unknown'} →`}
                                    </a>
                                </p>
                            )}
                        </aside>
                    )}
                </div>
            )}
        </section>
    );
}

function Run({ run }: { readonly run: GridReportRunView }): ReactNode {
    const { t, language } = useTranslation();
    const none = <span className="text-ink-faint">{t('operations.grid.job.notYetKnown')}</span>;
    return (
        <section className="card space-y-4 p-6">
            <h2 className="text-lg font-medium">{t(`${FIELD}.run`)}</h2>
            <div>
                <p className="text-xs text-ink-faint">{t('operations.grid.jobs.fields.id')}</p>
                <p className="break-all font-mono text-sm">{run.id}</p>
                <div className="mt-2">
                    <HealthTag health={run.health} stateName={run.state_name} hasRun />
                </div>
            </div>
            <dl className="grid gap-3 sm:grid-cols-3">
                <Detail label={t(`${FIELD}.name`)} value={run.name} />
                <Detail label={t(`${FIELD}.kind`)} value={run.kind} />
                <Detail
                    label={t(`${FIELD}.party`)}
                    value={
                        run.party_name === null ? (
                            run.party_id
                        ) : (
                            <span title={run.party_id}>{run.party_name}</span>
                        )
                    }
                />
                <Detail label={t(`${FIELD}.state`)} value={run.state_name || none} />
                <Detail
                    label={t(`${FIELD}.startedAt`)}
                    value={
                        run.started_at === null ? none : formatDateTime(run.started_at, language)
                    }
                />
                <Detail
                    label={t(`${FIELD}.completedAt`)}
                    value={
                        run.completed_at === null
                            ? none
                            : formatDateTime(run.completed_at, language)
                    }
                />
                <Detail
                    label={t('operations.grid.jobs.fields.modifiedBy')}
                    value={run.modified_by}
                />
            </dl>
            {run.description !== '' && <p className="text-sm">{run.description}</p>}
        </section>
    );
}

function Links({ run }: { readonly run: GridReportRunView }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-6">
            <h2 className="text-lg font-medium">{t(`${FIELD}.links`)}</h2>
            <ul className="space-y-1 text-sm">
                <li>
                    <PageLink to={reportPath(run.definition_id)}>
                        {run.definition_name === null
                            ? t(`${FIELD}.report`)
                            : t(`${FIELD}.reportNamed`, { name: run.definition_name })}
                    </PageLink>
                </li>
                <li>
                    {run.batch_id === null ? (
                        <span className="text-ink-faint">
                            {run.health === 'failed'
                                ? t(`${FIELD}.noBatchFailed`)
                                : t(`${FIELD}.noBatch`)}
                        </span>
                    ) : (
                        <PageLink to={gridBatchPath(run.batch_id)}>{t(`${FIELD}.batch`)}</PageLink>
                    )}
                </li>
            </ul>
        </section>
    );
}

/** The report run page. */
export function GridReportRunPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const [, setSearchParams] = useSearchParams();
    const { tab, bar } = useTabs({
        label: t(`${FIELD}.title`),
        tabs: TABS,
        titleOf: (candidate) => (candidate === 'errors' ? t(`${FIELD}.errors`) : t(`${FIELD}.run`)),
    });
    const run = useQuery({
        queryKey: [REPORT_RUN_QUERY_KEY, id],
        queryFn: () => api.gridReportRun(id),
        meta: { quiet: true },
    });
    const { rows } = useRunErrors(id);

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail
                    area="operations"
                    steps={[{ label: t('operations.screens.grid'), to: '/operations/grid' }]}
                    screen={run.data?.name ?? id.slice(0, 8)}
                />
                <PageHeader
                    title={t(`${FIELD}.title`)}
                    description={t(`${FIELD}.description`)}
                    actions={
                        <LinkButton to="/operations/grid?tab=jobs" variant="secondary">
                            {t('operations.grid.job.back')}
                        </LinkButton>
                    }
                />
            </div>
            {bar}
            {run.isPending && <p className="text-sm text-ink-muted">{t(`${FIELD}.loading`)}</p>}
            {run.isError && <ReadFailure error={run.error} onRetry={() => void run.refetch()} />}
            {run.isSuccess && (
                <>
                    {run.data.kind !== 'production' && (
                        <Notice tone="warn">
                            {t(`${FIELD}.notProduction`, { kind: run.data.kind })}
                        </Notice>
                    )}
                    {tab === 'errors' ? (
                        <RunErrors id={run.data.id} outputMessage={run.data.output_message} />
                    ) : (
                        <>
                            <RunSummary
                                rows={rows}
                                onShowErrors={() => setSearchParams({ tab: 'errors' })}
                            />
                            <Run run={run.data} />
                            <Links run={run.data} />
                        </>
                    )}
                </>
            )}
        </div>
    );
}
