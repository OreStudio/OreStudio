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
import { Link, useParams, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RelativeTime } from '../ui/Time.js';
import { Notice } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { Crumbs } from '../refdata/shared.js';
import { PATHS, reportPath } from '../shell/paths.js';
import { RunResultsPanel } from './RunResultsPanel.js';

/**
 * One report run, in the reporting area: what the run says about itself, and
 * the figures it produced.
 *
 * The run is the subject. A person arrives from the runs of a report, and the
 * page names the report it came from, so a result is never read without the run
 * that produced it.
 */
export function ReportRunPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const area = useAreaRoot('reporting');
    const { tab, bar } = useTabs({
        label: t('reporting.run.title'),
        tabs: RUN_TABS,
        titleOf: (candidate) => t(`reporting.run.tabs.${candidate}`),
    });
    const run = useQuery({
        queryKey: ['reporting-run', id],
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
                <h1 className="text-xl font-medium">{t('reporting.run.title')}</h1>
            </div>
            {bar}
            {run.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {run.isError && <Notice tone="error">{run.error.message}</Notice>}
            {run.isSuccess && (
                <>
                    {tab === 'results' ? (
                        <RunResultsPanel
                            runId={id}
                            pricedAt={run.data.priced_at.slice(0, 10)}
                            queryKey={['reporting-run-results', id]}
                            load={() => api.reportRunResults(id)}
                        />
                    ) : (
                        <section className="card space-y-4 p-6">
                            <h2 className="text-lg font-medium">{run.data.name}</h2>
                            <dl className="grid gap-x-8 gap-y-3 sm:grid-cols-3">
                                <Field label={t('reporting.run.definition')}>
                                    <Link
                                        to={reportPath(run.data.definition_id)}
                                        className="text-accent hover:text-accent-bright"
                                    >
                                        {run.data.definition_id.slice(0, 8)}…
                                    </Link>
                                </Field>
                                <Field label={t('reporting.run.kind')}>{run.data.kind}</Field>
                                <Field label={t('reporting.run.state')}>
                                    {run.data.state_name === '' ? '—' : run.data.state_name}
                                </Field>
                                <Field label={t('reporting.run.started')}>
                                    {run.data.started_at === '' ? (
                                        '—'
                                    ) : (
                                        <RelativeTime at={run.data.started_at} />
                                    )}
                                </Field>
                                <Field label={t('reporting.run.completed')}>
                                    {run.data.completed_at === '' ? (
                                        '—'
                                    ) : (
                                        <RelativeTime at={run.data.completed_at} />
                                    )}
                                </Field>
                                <Field label={t('reporting.run.pricedAt')}>
                                    {run.data.priced_at === ''
                                        ? '—'
                                        : run.data.priced_at.slice(0, 10)}
                                    {run.data.price_note !== '' && (
                                        <span className="block text-xs text-ink-muted">
                                            {run.data.price_note}
                                        </span>
                                    )}
                                </Field>
                            </dl>
                            <div className="space-y-1">
                                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                                    {t('reporting.run.message')}
                                </h3>
                                <p className="text-sm">{run.data.output_message}</p>
                            </div>
                            <div className="space-y-1">
                                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                                    {t('reporting.run.output')}
                                </h3>
                                {run.data.output_storage_key === '' ? (
                                    <p className="text-sm text-ink-muted">
                                        {t('reporting.run.noOutput')}
                                    </p>
                                ) : (
                                    <>
                                        <p className="font-mono text-xs">
                                            {run.data.output_storage_key}
                                        </p>
                                        <a
                                            href={api.reportRunOutputUrl(run.data.id)}
                                            className="text-sm text-accent hover:text-accent-bright"
                                        >
                                            {t('reporting.run.download')}
                                        </a>
                                    </>
                                )}
                            </div>
                            <p className="text-sm">
                                <Link
                                    to={`/operations/grid/report-runs/${encodeURIComponent(run.data.id)}?tab=errors`}
                                    className="text-accent hover:text-accent-bright"
                                >
                                    {t('reporting.run.errors')}
                                </Link>
                            </p>
                        </section>
                    )}
                </>
            )}
        </div>
    );
}

/** The tabs of a run: the run itself, and the figures it produced. */
const RUN_TABS = ['run', 'results'] as const;

function Field({
    label,
    children,
}: {
    readonly label: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <div>
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="text-sm">{children}</dd>
        </div>
    );
}
