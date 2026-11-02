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
import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import type { ReportRunErrorView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable } from '../refdata/RecordList.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { CountFilter } from './shared.js';

/** The key a run's errors are cached under, shared by every screen that reads them. */
export const runErrorsKey = (runId: string) => ['reporting-run-errors', runId] as const;

/** A run's problems as the server states them, one row each. */
export function useRunErrors(runId: string) {
    return useQuery({
        queryKey: runErrorsKey(runId),
        queryFn: () => api.reportRunErrors(runId),
        meta: { quiet: true },
    });
}

const SEVERITIES = ['all', 'error', 'warning'] as const;
type SeverityChoice = (typeof SEVERITIES)[number];

/** The severity of a problem, coloured as every severity in the application is. */
export function SeverityTag({ severity }: { readonly severity: string }): ReactNode {
    return <Tag tone={severity === 'error' ? 'down' : 'warn'}>{severity}</Tag>;
}

/**
 * The problems a run recorded, one row each: how severe it is, the part of the
 * run it belongs to, the one thing it names and what the engine said.
 */
export function RunErrorsTable({ runId }: { readonly runId: string }): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.errors';
    const errors = useRunErrors(runId);
    const [severity, setSeverity] = useState<SeverityChoice>('all');

    if (errors.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (errors.isError) {
        return <ReadFailure error={errors.error} onRetry={() => void errors.refetch()} />;
    }
    const rows = errors.data;
    if (rows.length === 0) {
        return <Notice tone="success">{t(`${field}.none`)}</Notice>;
    }
    const counts: Readonly<Record<SeverityChoice, number>> = {
        all: rows.length,
        error: rows.filter((row) => row.severity === 'error').length,
        warning: rows.filter((row) => row.severity === 'warning').length,
    };
    const shown = severity === 'all' ? rows : rows.filter((row) => row.severity === severity);

    return (
        <section className="space-y-3">
            <CountFilter
                choices={SEVERITIES}
                chosen={severity}
                countOf={(choice) => counts[choice]}
                labelOf={(choice, count) => t(`${field}.filter.${choice}`, { count })}
                onChoose={setSeverity}
            />
            <RecordTable<ReportRunErrorView>
                source={localSource<ReportRunErrorView>({
                    key: `run-errors-${runId}`,
                    rows: shown,
                    revision: `${severity}:${shown.length}`,
                    searchOf: (row) => `${row.scope} ${row.about} ${row.message}`,
                })}
                plural={t(`${field}.plural`)}
                keyOf={(row) => row.id}
                columns={[
                    {
                        id: 'severity',
                        header: t(`${field}.severity`),
                        cell: (row) => <SeverityTag severity={row.severity} />,
                    },
                    { id: 'scope', header: t(`${field}.scope`), cell: (row) => row.scope || '—' },
                    {
                        id: 'about',
                        header: t(`${field}.about`),
                        mono: true,
                        cell: (row) => row.about || '—',
                    },
                    {
                        id: 'message',
                        header: t(`${field}.message`),
                        cell: (row) => <span className="whitespace-pre-wrap">{row.message}</span>,
                    },
                ]}
            />
        </section>
    );
}
