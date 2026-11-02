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
import type {
    OreFileParameterView,
    OreFileView,
    ReportRunFileView,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable } from '../refdata/RecordList.js';
import { Crumbs } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, gridJobPath, reportRunPath } from '../shell/paths.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';
import { reportRunKey } from './ReportRunPage.js';
import { formatBytes } from './shared.js';

const FIELD = 'reporting.run.file';

/**
 * One file a run read or wrote: what the file is, as the ORE file catalogue
 * describes it, the facts the run holds about this copy of it, the parameters
 * of a run document that can name it, and a way to download it.
 */
export function RunFilePage(): ReactNode {
    const { t } = useTranslation();
    const { id = '', file = '' } = useParams();
    const area = useAreaRoot('reporting');
    const run = useQuery({
        queryKey: reportRunKey(id),
        queryFn: () => api.reportRun(id),
        meta: { quiet: true },
    });
    const info = useQuery({
        queryKey: ['reporting-run-file', id, file],
        queryFn: () => api.reportRunFileInfo(id, file),
        meta: { quiet: true },
    });

    const trail = [
        ...area,
        { label: t('reporting.title'), to: PATHS.reports },
        { label: run.data?.name ?? id.slice(0, 8), to: `${reportRunPath(id)}?tab=downloads` },
        { label: info.data?.file.name ?? t(`${FIELD}.title`) },
    ];

    return (
        <div className="space-y-6">
            <div>
                <Crumbs parts={trail} />
                <h1 className="font-mono text-xl font-medium">
                    {info.data?.file.name ?? t(`${FIELD}.title`)}
                </h1>
            </div>
            {info.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {info.isError && <ReadFailure error={info.error} onRetry={() => void info.refetch()} />}
            {info.isSuccess && (
                <>
                    <OreFileCard oreFile={info.data.ore_file} />
                    <FileFacts runId={id} file={info.data.file} oreFile={info.data.ore_file} />
                    {info.data.ore_file !== null && (
                        <Parameters
                            oreFile={info.data.ore_file.code}
                            parameters={info.data.parameters}
                        />
                    )}
                </>
            )}
        </div>
    );
}

/** What the file is, as the ORE file catalogue describes it. */
function OreFileCard({ oreFile }: { readonly oreFile: OreFileView | null }): ReactNode {
    const { t } = useTranslation();
    if (oreFile === null) {
        return <Notice tone="info">{t(`${FIELD}.notCatalogued`)}</Notice>;
    }
    return (
        <section className="card space-y-4 p-6">
            <div className="flex flex-wrap items-center gap-3">
                <h2 className="text-lg font-medium">{oreFile.name}</h2>
                <Tag>{oreFile.category_name}</Tag>
                <Tag tone="muted">{t(`${FIELD}.origins.${oreFile.origin}`)}</Tag>
            </div>
            <p className="whitespace-pre-line text-sm leading-relaxed">{oreFile.description}</p>
            <div className="rounded-md border border-line-subtle bg-surface-raised px-4 py-3">
                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                    {oreFile.category_name}
                </h3>
                <p className="mt-1 text-sm text-ink-muted">{oreFile.category_description}</p>
            </div>
        </section>
    );
}

/** The facts about this copy of the file and about the kind of file it is. */
function FileFacts({
    runId,
    file,
    oreFile,
}: {
    readonly runId: string;
    readonly file: ReportRunFileView;
    readonly oreFile: OreFileView | null;
}): ReactNode {
    const { t } = useTranslation();
    const none = '—';
    return (
        <section className="card space-y-4 p-6">
            <div className="flex flex-wrap items-center justify-between gap-3">
                <h2 className="text-lg font-medium">{t(`${FIELD}.facts`)}</h2>
                <a
                    href={api.reportRunFileUrl(runId, file.id)}
                    download
                    className="rounded-md bg-accent px-3 py-1.5 text-sm font-medium text-ink-inverse hover:bg-accent-bright"
                >
                    {t(`${FIELD}.download`)}
                </a>
            </div>
            <dl className="grid gap-x-8 gap-y-3 sm:grid-cols-2 lg:grid-cols-3">
                <Detail
                    label={t(`${FIELD}.name`)}
                    value={<span className="font-mono">{file.name}</span>}
                />
                <Detail
                    label={t(`${FIELD}.direction`)}
                    value={t(`reporting.run.files.directions.${file.direction}`)}
                />
                <Detail
                    label={t(`${FIELD}.kind`)}
                    value={t(`reporting.run.files.kinds.${file.kind}`)}
                />
                <Detail
                    label={t(`${FIELD}.size`)}
                    value={file.bytes === null ? none : formatBytes(file.bytes)}
                />
                <Detail
                    label={t(`${FIELD}.digest`)}
                    value={
                        file.sha256 === null ? (
                            none
                        ) : (
                            <span className="break-all font-mono text-xs">{file.sha256}</span>
                        )
                    }
                />
                <Detail
                    label={t(`${FIELD}.job`)}
                    value={
                        file.job === null ? (
                            none
                        ) : (
                            <Link
                                to={gridJobPath(file.job)}
                                className="text-accent hover:text-accent-bright"
                            >
                                {file.job.slice(0, 8)}
                            </Link>
                        )
                    }
                />
                {oreFile !== null && (
                    <>
                        <Detail
                            label={t(`${FIELD}.format`)}
                            value={t(`${FIELD}.formats.${oreFile.format}`)}
                        />
                        <Detail
                            label={t(`${FIELD}.xsd`)}
                            value={
                                oreFile.xsd_file === null ? (
                                    none
                                ) : (
                                    <span className="font-mono">{oreFile.xsd_file}</span>
                                )
                            }
                        />
                        <Detail
                            label={t(`${FIELD}.typicalName`)}
                            value={<span className="font-mono">{oreFile.typical_filename}</span>}
                        />
                        <Detail
                            label={t(`${FIELD}.namePattern`)}
                            value={
                                <span className="break-all font-mono text-xs">
                                    {oreFile.filename_regex}
                                </span>
                            }
                        />
                        <Detail
                            label={t(`${FIELD}.configurationType`)}
                            value={oreFile.configuration_type_code ?? none}
                        />
                        <Detail
                            label={t(`${FIELD}.guideSection`)}
                            value={oreFile.guide_section ?? none}
                        />
                        <Detail
                            label={t(`${FIELD}.code`)}
                            value={<span className="font-mono">{oreFile.code}</span>}
                        />
                    </>
                )}
            </dl>
        </section>
    );
}

/** The parameters of a run document that can name the file, each with what it is for. */
function Parameters({
    oreFile,
    parameters,
}: {
    readonly oreFile: string;
    readonly parameters: readonly OreFileParameterView[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="space-y-3">
            <h2 className="text-lg font-medium">{t(`${FIELD}.parameters`)}</h2>
            {parameters.length === 0 ? (
                <p className="text-sm text-ink-muted">{t(`${FIELD}.noParameters`)}</p>
            ) : (
                <RecordTable<OreFileParameterView>
                    source={localSource<OreFileParameterView>({
                        key: `ore-file-parameters-${oreFile}`,
                        rows: parameters,
                        revision: String(parameters.length),
                        searchOf: (row) => `${row.code} ${row.description}`,
                    })}
                    plural={t(`${FIELD}.parametersPlural`)}
                    keyOf={(row) => row.code}
                    columns={[
                        {
                            id: 'parameter',
                            header: t(`${FIELD}.parameter`),
                            mono: true,
                            cell: (row) => row.code,
                        },
                        {
                            id: 'direction',
                            header: t(`${FIELD}.direction`),
                            cell: (row) => (
                                <Tag tone={row.direction === 'input' ? 'accent' : 'up'}>
                                    {t(`reporting.run.files.directions.${row.direction}`)}
                                </Tag>
                            ),
                        },
                        {
                            id: 'name',
                            header: t(`${FIELD}.parameterName`),
                            mono: true,
                            cell: (row) => row.name_pattern,
                        },
                        {
                            id: 'default',
                            header: t(`${FIELD}.default`),
                            cell: (row) => (row.is_default ? t('common.yes') : t('common.no')),
                        },
                        {
                            id: 'description',
                            header: t(`${FIELD}.parameterDescription`),
                            cell: (row) => (
                                <span className="whitespace-pre-line">{row.description}</span>
                            ),
                        },
                    ]}
                />
            )}
        </section>
    );
}
