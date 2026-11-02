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
import { Link, Navigate, useLocation, useParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ReportRunDetailView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { Crumbs } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, gridBatchPath, reportPath, reportRunPath } from '../shell/paths.js';
import { Detail, Notice } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { formatDate, formatDateTime } from '../ui/Time.js';
import { RunErrorsTable } from './RunErrorsTable.js';
import { RunFilesTable } from './RunFilesTable.js';
import { RunResultsPanel } from './RunResultsPanel.js';
import { HealthTag, formatDuration } from './shared.js';

/** Where a run is cached, shared by every screen that reads it. */
export const reportRunKey = (runId: string) => ['reporting-run', runId] as const;

/** The tabs of a run: what it is, the figures it produced, its problems and its files. */
const RUN_TABS = ['details', 'results', 'errors', 'downloads'] as const;

const FIELD = 'reporting.run';

/**
 * One report run, in one place: what it is and the state it reached, the
 * figures it produced, the problems it recorded, and the files it read and
 * wrote. The run belongs to a party, so a session that may not see the party
 * gets the same answer as a run that does not exist.
 */
export function ReportRunPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const area = useAreaRoot('reporting');
    const { tab, bar } = useTabs({
        label: t(`${FIELD}.title`),
        tabs: RUN_TABS,
        titleOf: (candidate) => t(`${FIELD}.tabs.${candidate}`),
    });
    const run = useQuery({
        queryKey: reportRunKey(id),
        queryFn: () => api.reportRun(id),
        meta: { quiet: true },
    });

    const trail = [
        ...area,
        { label: t('reporting.title'), to: PATHS.reports },
        { label: run.data?.name ?? id.slice(0, 8) },
    ];

    return (
        <div className="space-y-6">
            <div>
                <Crumbs parts={trail} />
                <h1 className="text-xl font-medium">{t(`${FIELD}.title`)}</h1>
            </div>
            {bar}
            {run.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {run.isError && <ReadFailure error={run.error} onRetry={() => void run.refetch()} />}
            {run.isSuccess && (
                <>
                    {run.data.kind !== 'production' && (
                        <Notice tone="warn">
                            {t(`${FIELD}.notProduction`, { kind: run.data.kind })}
                        </Notice>
                    )}
                    {tab === 'details' && <RunDetails run={run.data} />}
                    {tab === 'results' && (
                        <RunResultsPanel
                            runId={id}
                            definitionId={run.data.definition_id}
                            pricedAt={run.data.priced_at?.slice(0, 10) ?? ''}
                        />
                    )}
                    {tab === 'errors' && <RunErrorsTable runId={id} />}
                    {tab === 'downloads' && <RunFilesTable runId={run.data.id} />}
                </>
            )}
        </div>
    );
}

/** What a run is, the state it reached, how long it took and the batch it made. */
function RunDetails({ run }: { readonly run: ReportRunDetailView }): ReactNode {
    const { t, language } = useTranslation();
    const none = '—';
    const batch =
        run.batch_id === null ? (
            <span className="text-ink-faint">
                {run.health === 'failed' ? t(`${FIELD}.noBatchFailed`) : t(`${FIELD}.noBatch`)}
            </span>
        ) : (
            <Link to={gridBatchPath(run.batch_id)} className="text-accent hover:text-accent-bright">
                {run.batch_id.slice(0, 8)}…
            </Link>
        );
    return (
        <section className="card space-y-4 p-6">
            <div className="flex flex-wrap items-center gap-3">
                <h2 className="text-lg font-medium">{run.name}</h2>
                <HealthTag health={run.health} stateName={run.state_name} hasRun />
            </div>
            <dl className="grid gap-3 sm:grid-cols-3">
                <Detail
                    label={t(`${FIELD}.definition`)}
                    value={
                        <Link
                            to={reportPath(run.definition_id)}
                            className="text-accent hover:text-accent-bright"
                        >
                            {run.definition_name ?? `${run.definition_id.slice(0, 8)}…`}
                        </Link>
                    }
                />
                <Detail
                    label={t(`${FIELD}.party`)}
                    value={run.party_name ?? run.party_id}
                />
                <Detail label={t(`${FIELD}.kind`)} value={run.kind} />
                <Detail label={t(`${FIELD}.state`)} value={run.state_name || none} />
                <Detail
                    label={t(`${FIELD}.started`)}
                    value={run.started_at === null ? none : formatDateTime(run.started_at, language)}
                />
                <Detail
                    label={t(`${FIELD}.completed`)}
                    value={
                        run.completed_at === null ? none : formatDateTime(run.completed_at, language)
                    }
                />
                <Detail
                    label={t(`${FIELD}.duration`)}
                    value={run.duration_ms === null ? none : formatDuration(run.duration_ms, t)}
                />
                <Detail
                    label={t(`${FIELD}.pricedAt`)}
                    value={run.priced_at === null ? none : formatDate(run.priced_at, language)}
                />
                <Detail label={t(`${FIELD}.batch`)} value={batch} />
                <Detail label={t(`${FIELD}.modifiedBy`)} value={run.modified_by} />
            </dl>
            {run.price_note !== '' && <p className="text-sm text-ink-muted">{run.price_note}</p>}
            {run.output_message !== '' && (
                <div className="space-y-1">
                    <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                        {t(`${FIELD}.message`)}
                    </h3>
                    <p className="whitespace-pre-wrap text-sm">{run.output_message}</p>
                </div>
            )}
        </section>
    );
}

/**
 * The address the operations area used for a run. A run has one page, so the
 * old address keeps working by sending the reader there.
 */
export function ReportRunRedirect(): ReactNode {
    const { id = '' } = useParams();
    const { search } = useLocation();
    return <Navigate to={`${reportRunPath(id)}${search}`} replace />;
}
