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
import { Link } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ReportRunFileView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable } from '../refdata/RecordList.js';
import { gridJobPath } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { CountFilter, formatBytes } from './shared.js';

const DIRECTIONS = ['all', 'input', 'output'] as const;
type DirectionChoice = (typeof DIRECTIONS)[number];

/**
 * Every file a run read and wrote, inputs first, each downloadable as the
 * engine read or wrote it. The server builds the list from compute's records,
 * so the screen names a file only by its id in that list.
 */
export function RunFilesTable({ runId }: { readonly runId: string }): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.files';
    const files = useQuery({
        queryKey: ['reporting-run-files', runId],
        queryFn: () => api.reportRunFiles(runId),
        meta: { quiet: true },
    });
    const [direction, setDirection] = useState<DirectionChoice>('all');

    if (files.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (files.isError) {
        return <ReadFailure error={files.error} onRetry={() => void files.refetch()} />;
    }
    const rows = files.data;
    if (rows.length === 0) {
        return <Notice tone="info">{t(`${field}.none`)}</Notice>;
    }
    const countOf = (choice: DirectionChoice): number =>
        choice === 'all' ? rows.length : rows.filter((row) => row.direction === choice).length;
    const shown = direction === 'all' ? rows : rows.filter((row) => row.direction === direction);

    return (
        <section className="space-y-3">
            <CountFilter
                choices={DIRECTIONS}
                chosen={direction}
                countOf={countOf}
                labelOf={(choice, count) => t(`${field}.filter.${choice}`, { count })}
                onChoose={setDirection}
            />
            <RecordTable<ReportRunFileView>
                source={localSource<ReportRunFileView>({
                    key: `run-files-${runId}`,
                    rows: shown,
                    revision: `${direction}:${shown.length}`,
                    searchOf: (row) => `${row.name} ${row.family}`,
                })}
                plural={t(`${field}.plural`)}
                keyOf={(row) => row.id}
                columns={[
                    {
                        id: 'direction',
                        header: t(`${field}.direction`),
                        cell: (row) => (
                            <Tag tone={row.direction === 'input' ? 'accent' : 'up'}>
                                {t(`${field}.directions.${row.direction}`)}
                            </Tag>
                        ),
                    },
                    {
                        id: 'name',
                        header: t(`${field}.name`),
                        mono: true,
                        cell: (row) => (
                            <a
                                href={api.reportRunFileUrl(runId, row.id)}
                                download
                                className="text-accent hover:text-accent-bright"
                            >
                                {row.name}
                            </a>
                        ),
                    },
                    {
                        id: 'kind',
                        header: t(`${field}.kind`),
                        cell: (row) => t(`${field}.kinds.${row.kind}`),
                    },
                    {
                        id: 'family',
                        header: t(`${field}.family`),
                        cell: (row) =>
                            row.direction === 'input' || row.kind === 'archive'
                                ? '—'
                                : row.family || t(`${field}.noFamily`),
                    },
                    {
                        id: 'size',
                        header: t(`${field}.size`),
                        cell: (row) => (row.bytes === null ? '—' : formatBytes(row.bytes)),
                    },
                    {
                        id: 'digest',
                        header: t(`${field}.digest`),
                        mono: true,
                        cell: (row) =>
                            row.sha256 === null ? (
                                '—'
                            ) : (
                                <span title={row.sha256}>{row.sha256.slice(0, 12)}</span>
                            ),
                    },
                    {
                        id: 'job',
                        header: t(`${field}.job`),
                        mono: true,
                        cell: (row) =>
                            row.job === null ? (
                                '—'
                            ) : (
                                <Link
                                    to={gridJobPath(row.job)}
                                    className="text-accent hover:text-accent-bright"
                                >
                                    {row.job.slice(0, 8)}
                                </Link>
                            ),
                    },
                ]}
            />
        </section>
    );
}
