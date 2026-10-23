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
            {run.output_message !== '' && (
                <div>
                    <p className="text-xs text-ink-faint">{t(`${FIELD}.output`)}</p>
                    <p className="whitespace-pre-wrap text-sm">{run.output_message}</p>
                </div>
            )}
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
                    <PageLink to={reportPath(run.definition_id)}>{t(`${FIELD}.report`)}</PageLink>
                </li>
                <li>
                    {run.batch_id === null ? (
                        <span className="text-ink-faint">{t(`${FIELD}.noBatch`)}</span>
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
                    <Links run={run.data} />
                </>
            )}
        </div>
    );
}
