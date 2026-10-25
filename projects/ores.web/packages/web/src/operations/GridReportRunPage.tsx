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
import { Link, useParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { GridReportRunView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { HealthTag } from '../reporting/shared.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { gridBatchPath, reportPath } from '../shell/paths.js';
import { Detail, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';
import { ReadFailure } from './GridJobs.js';

/**
 * The report run page: the run of a report that dispatched a batch.
 *
 * A batch names its run in its External Ref, and the job page links here. The page
 * states the run's state and health, when it started and ended, the message it
 * ended with, and links back to the report and to the batch. The run belongs to a
 * party, so a session that may not see the party gets the same answer as a run that
 * does not exist.
 */

/** Where the page's answer is cached, so a test can seed it. */
export const REPORT_RUN_QUERY_KEY = 'operations-grid-report-run';

const FIELD = 'operations.grid.reportRun';

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


/** One error a run recorded, shaped for the two views. */
type RunError = {
    id: string;
    scope: string;
    severity: string;
    message: string;
    subject: string;
    subjectId: string;
    version: number | null;
    asOf: string;
    time: string;
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

const tradeHref = (error: RunError): string =>
    `/trades?trade=${error.subjectId}${error.version === null ? '' : `&version=${error.version}`}`;

/** The run's errors: grouped by cause, or as rows beside the selected one. */
function RunErrors({ id }: { id: string }): ReactNode {
    const [view, setView] = useState<'grouped' | 'split'>('grouped');
    const [severity, setSeverity] = useState('all');
    const [search, setSearch] = useState('');
    const [selected, setSelected] = useState<string | null>(null);

    const errors = useQuery({
        queryKey: ['report-run-errors', id],
        queryFn: () => api.gridReportRunErrors(id),
        meta: { quiet: true },
    });

    const rows = useMemo(
        () => (errors.data ?? []).map(toError),
        [errors.data],
    );

    const shown = useMemo(
        () =>
            rows.filter((error) => {
                if (severity !== 'all' && error.severity !== severity) return false;
                const needle = search.trim().toLowerCase();
                if (needle === '') return true;
                return [error.message, error.subjectId, error.subject]
                    .join(' ')
                    .toLowerCase()
                    .includes(needle);
            }),
        [rows, severity, search],
    );

    const groups = useMemo(() => {
        const curves = shown.filter((error) => error.scope === 'Curve');
        const trades = shown.filter((error) => error.scope === 'Trade');
        return curves.map((curve) => {
            const name = curveNameOf(curve.subjectId !== '' ? curve.subjectId : curve.message);
            return {
                name,
                curve,
                trades: trades.filter((trade) => trade.message.includes(name)),
            };
        });
    }, [shown]);

    const others = useMemo(
        () => shown.filter((error) => error.scope !== 'Curve' && error.scope !== 'Trade'),
        [shown],
    );

    if (errors.isPending) return null;
    if (errors.isError) {
        return <ReadFailure error={errors.error} onRetry={() => void errors.refetch()} />;
    }
    if (rows.length === 0) {
        return (
            <Notice tone="success">
                This run recorded no errors.
            </Notice>
        );
    }

    const current = shown.find((error) => error.id === selected) ?? shown[0] ?? null;

    return (
        <section className="space-y-3">
            <div className="flex flex-wrap items-center gap-2">
                <h2 className="text-base">{`Errors (${rows.length})`}</h2>
                <button type="button" onClick={() => setSeverity('all')}
                    aria-pressed={severity === 'all'}>All</button>
                <button type="button" onClick={() => setSeverity('error')}
                    aria-pressed={severity === 'error'}>Errors</button>
                <button type="button" onClick={() => setSeverity('warning')}
                    aria-pressed={severity === 'warning'}>Warnings</button>
                <input type="search" placeholder="Search message, curve or trade id"
                    value={search} onChange={(event) => setSearch(event.target.value)} />
                <button type="button" onClick={() => setView('grouped')}
                    aria-pressed={view === 'grouped'}>Grouped</button>
                <button type="button" onClick={() => setView('split')}
                    aria-pressed={view === 'split'}>Split</button>
            </div>
            {view === 'grouped' && (
                <ul className="space-y-3">
                    {groups.map((group) => (
                        <li key={group.name} className="space-y-2 rounded border border-line p-3">
                            <p className="font-mono text-sm">{group.name}</p>
                            <p className="text-xs text-ink-muted">
                                {`${group.curve.severity} · ${group.curve.scope} · ${group.trades.length} trade(s)`}
                            </p>
                            <pre className="whitespace-pre-wrap font-mono text-xs">
                                {group.curve.message}
                            </pre>
                            <div className="flex flex-wrap gap-2">
                                {group.trades.map((trade) => (
                                    <a key={trade.id} className="font-mono text-xs underline"
                                        href={tradeHref(trade)}>
                                        {`${trade.subjectId.slice(0, 8)}… · v${trade.version ?? '?'}${trade.asOf === '' ? '' : ` @ ${trade.asOf}`}`}
                                    </a>
                                ))}
                            </div>
                        </li>
                    ))}
                    {others.length > 0 && (
                        <li className="space-y-2 rounded border border-line p-3">
                            <p className="text-sm">Analytic and report failures</p>
                            <p className="text-xs text-ink-muted">{`${others.length} reason(s)`}</p>
                            <ul className="space-y-1">
                                {others.map((other) => (
                                    <li key={other.id} className="font-mono text-xs">
                                        {`${other.severity}: ${other.message}`}
                                    </li>
                                ))}
                            </ul>
                        </li>
                    )}
                </ul>
            )}
            {view === 'split' && (
                <div className="grid gap-4 md:grid-cols-2">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr>
                                <th>Severity</th><th>Scope</th><th>Subject</th>
                                <th>Trade</th><th>Version</th><th>Time</th>
                            </tr>
                        </thead>
                        <tbody>
                            {shown.map((error) => (
                                <tr key={error.id} className="cursor-pointer"
                                    aria-selected={current?.id === error.id}
                                    onClick={() => setSelected(error.id)}>
                                    <td>{error.severity}</td>
                                    <td>{error.scope}</td>
                                    <td className="font-mono">{error.subject || '—'}</td>
                                    <td className="font-mono">
                                        {error.subjectId === '' ? '—' : (
                                            <a className="underline" href={tradeHref(error)}
                                                onClick={(event) => event.stopPropagation()}>
                                                {error.subjectId}
                                            </a>
                                        )}
                                    </td>
                                    <td className="font-mono">
                                        {error.version === null ? '—' : `v${error.version}${error.asOf === '' ? '' : ` @ ${error.asOf}`}`}
                                    </td>
                                    <td className="font-mono text-xs">{error.time}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                    {current !== null && (
                        <div className="space-y-2">
                            <p className="font-mono text-sm">{current.subject || current.scope}</p>
                            <p className="text-xs text-ink-muted">{`${current.severity} · ${current.scope}`}</p>
                            <pre className="whitespace-pre-wrap font-mono text-xs">
                                {current.message}
                            </pre>
                            {current.subjectId !== '' && (
                                <p className="font-mono text-xs">
                                    {`${current.subjectId} · version ${current.version ?? 'unknown'}${current.asOf === '' ? '' : ` · as of ${current.asOf}`}`}
                                    <a className="ml-2 underline" href={tradeHref(current)}>
                                        Open the trade
                                    </a>
                                </p>
                            )}
                        </div>
                    )}
                </div>
            )}
        </section>
    );
}

/** The report run page. */
export function GridReportRunPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const run = useQuery({
        queryKey: [REPORT_RUN_QUERY_KEY, id],
        queryFn: () => api.gridReportRun(id),
        meta: { quiet: true },
    });
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
            {run.isPending && <p className="text-sm text-ink-muted">{t(`${FIELD}.loading`)}</p>}
            {run.isError && <ReadFailure error={run.error} onRetry={() => void run.refetch()} />}
            {run.isSuccess && (
                <>
                    {run.data.kind !== 'production' && (
                        <Notice tone="warn">
                            {t(`${FIELD}.notProduction`, { kind: run.data.kind })}
                        </Notice>
                    )}
                    <Run run={run.data} />
                    <RunErrors id={run.data.id} />
                    <Links run={run.data} />
                </>
            )}
        </div>
    );
}
