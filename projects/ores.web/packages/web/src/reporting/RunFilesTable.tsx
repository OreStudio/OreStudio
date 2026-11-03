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
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ReportRunFileNodeView, ReportRunFileView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable, type ListColumn } from '../refdata/RecordList.js';
import type { Translator } from '../i18n/translate.js';
import { TreeRail } from '../ui/TreeRail.js';
import { gridJobPath, reportRunFilePath } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { formatBytes } from './shared.js';

/** The node ids under a node, the node itself included. */
function subtreeOf(tree: readonly ReportRunFileNodeView[], root: string): ReadonlySet<string> {
    const within = new Set([root]);
    let grew = true;
    while (grew) {
        grew = false;
        for (const node of tree) {
            if (node.id !== '' && within.has(node.parent) && !within.has(node.id)) {
                within.add(node.id);
                grew = true;
            }
        }
    }
    return within;
}

/**
 * Every file a run read and wrote, in the tree they hang in: the run, its
 * batch, each job, and each job's input package and output. Choosing a node
 * narrows the table to the files under it. The server builds the tree from
 * compute's records, so the screen names a file only by its id in the list.
 */
export function RunFilesTable({ runId }: { readonly runId: string }): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.files';
    const [search, setSearch] = useSearchParams();
    const chosen = search.get('node') ?? '';
    const files = useQuery({
        queryKey: ['reporting-run-files', runId],
        queryFn: () => api.reportRunFiles(runId),
        meta: { quiet: true },
    });

    if (files.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (files.isError) {
        return <ReadFailure error={files.error} onRetry={() => void files.refetch()} />;
    }
    const { tree, files: rows } = files.data;
    if (rows.length === 0) {
        return <Notice tone="info">{t(`${field}.none`)}</Notice>;
    }
    const choose = (node: string): void => {
        const params = new URLSearchParams(search);
        if (node === '') {
            params.delete('node');
        } else {
            params.set('node', node);
        }
        setSearch(params, { replace: true });
    };
    const within = subtreeOf(tree, chosen);
    const shown = rows.filter((row) => within.has(row.node));
    const rail = tree.map((node) => ({
        ...node,
        label: t(`${field}.nodes.${node.kind}`, { ref: node.ref.slice(0, 8) }),
    }));

    return (
        <div className="flex flex-col gap-4 lg:flex-row lg:items-start">
            <aside className="shrink-0 rounded-card border border-line bg-surface p-3 lg:w-72">
                <h2 className="text-sm font-semibold">{t(`${field}.tree`)}</h2>
                <TreeRail
                    label={t(`${field}.tree`)}
                    nodes={rail}
                    selected={chosen}
                    onSelect={choose}
                />
            </aside>
            <div className="min-w-0 flex-1">
                <RecordTable<ReportRunFileView>
                    source={localSource<ReportRunFileView>({
                        key: `run-files-${runId}`,
                        rows: shown,
                        revision: `${chosen}:${shown.length}`,
                        searchOf: (row) => `${row.name} ${row.ore_file_name ?? ''}`,
                    })}
                    plural={t(`${field}.plural`)}
                    keyOf={(row) => row.id}
                    pathOf={(row) => reportRunFilePath(runId, row.id)}
                    columns={columnsOf(t, runId)}
                />
            </div>
        </div>
    );
}

function columnsOf(t: Translator['t'], runId: string): readonly ListColumn<ReportRunFileView>[] {
    const field = 'reporting.run.files';
    return [
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
            id: 'oreFile',
            header: t(`${field}.oreFile`),
            cell: (row) =>
                row.ore_file_name ?? (
                    <span className="text-ink-faint">{t(`${field}.notCatalogued`)}</span>
                ),
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
    ];
}
