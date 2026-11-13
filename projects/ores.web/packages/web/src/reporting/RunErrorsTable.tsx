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
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ReportRunErrorView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { localSource, RecordTable } from '../refdata/RecordList.js';
import { tradePath } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { Donut, problemsOf } from './Donut.js';
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

const SEVERITIES = ['all', 'error', 'warning', 'info'] as const;
type SeverityChoice = (typeof SEVERITIES)[number];

/**
 * How a row's severity is drawn. A failure is red and a warning is amber; a
 * notice is quiet, because a notice says what the engine was given rather than
 * that anything went wrong.
 */
const TONES = { error: 'down', warning: 'warn', info: 'muted' } as const;

/** The severity of a row, coloured as every severity in the application is. */
export function SeverityTag({ severity }: { readonly severity: string }): ReactNode {
    const tone = TONES[severity as keyof typeof TONES] ?? 'neutral';
    return <Tag tone={tone}>{severity}</Tag>;
}

/** What an error is about: a link to the trade when it names one, text otherwise. */
export function ErrorSubject({ error }: { readonly error: ReportRunErrorView }): ReactNode {
    if (error.subject === 'trade' && error.subject_id !== null) {
        return (
            <Link to={tradePath(error.subject_id)} className="text-accent hover:text-accent-bright">
                {error.about}
            </Link>
        );
    }
    return error.about || '—';
}

const VIEWS = ['overview', 'list'] as const;

/** The chip that means every log, as distinct from the group that came from none. */
const EVERY_LOG = 'all';

const OUTER = { tab: 'log' } as const;

/** The address of the list view narrowed to one scope. */
function scopeAddress(scope: string): string {
    return `?${new URLSearchParams({ ...OUTER, view: 'list', scope }).toString()}`;
}

/** The address of the list view narrowed to one log. */
function fileAddress(file: string): string {
    return `?${new URLSearchParams({ ...OUTER, view: 'list', file }).toString()}`;
}

/**
 * The problems of a run, as two views: an overview of how many fell in each
 * part of the run, and the list with one row each. A scope in the overview
 * opens the list narrowed to it.
 */
export function RunErrors({ runId }: { readonly runId: string }): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.errors';
    const errors = useRunErrors(runId);
    const { tab: view, bar } = useTabs({
        label: t('reporting.run.tabs.log'),
        tabs: VIEWS,
        param: 'view',
        keep: OUTER,
        titleOf: (name) => t(`${field}.views.${name}`),
    });

    if (errors.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (errors.isError) {
        return <ReadFailure error={errors.error} onRetry={() => void errors.refetch()} />;
    }
    if (errors.data.length === 0) {
        return <Notice tone="success">{t(`${field}.none`)}</Notice>;
    }
    return (
        <section className="space-y-4">
            {bar}
            {view === 'overview' ? (
                <RunErrorsOverview rows={errors.data} />
            ) : (
                <RunErrorsTable runId={runId} rows={errors.data} />
            )}
        </section>
    );
}

/** One group's rows, counted apart by severity. */
export interface Tally {
    readonly errors: number;
    readonly warnings: number;
    readonly infos: number;
}

/** A scope's rows, counted apart by severity. */
export interface ScopeCount extends Tally {
    readonly scope: string;
}

/** A log file's rows, counted apart by severity. */
export interface FileCount extends Tally {
    /** The log the rows came from, or empty for a row the platform wrote itself. */
    readonly file: string;
}

/** Every group's three figures while they are still being counted. */
interface MutableTally {
    errors: number;
    warnings: number;
    infos: number;
}

/** The rows counted by whatever names a group, from the most to the fewest. */
function tallyBy<Count extends Tally>(
    rows: readonly ReportRunErrorView[],
    keyOf: (row: ReportRunErrorView) => string,
    build: (key: string, tally: MutableTally) => Count,
): readonly Count[] {
    const counts = new Map<string, MutableTally>();
    for (const row of rows) {
        const key = keyOf(row);
        const count = counts.get(key) ?? { errors: 0, warnings: 0, infos: 0 };
        if (row.severity === 'error') {
            count.errors += 1;
        } else if (row.severity === 'warning') {
            count.warnings += 1;
        } else {
            count.infos += 1;
        }
        counts.set(key, count);
    }
    const total = (count: MutableTally): number => count.errors + count.warnings + count.infos;
    return [...counts.entries()]
        .map(([key, count]) => build(key, count))
        .sort((left, right) => total(right) - total(left));
}

/** How many rows fell in each scope, by severity, from the most to the fewest. */
export function scopeCounts(rows: readonly ReportRunErrorView[]): readonly ScopeCount[] {
    return tallyBy(
        rows,
        (row) => row.scope,
        (scope, tally) => ({ scope, ...tally }),
    );
}

/** How many rows came from each log, by severity, from the most to the fewest. */
export function fileCounts(rows: readonly ReportRunErrorView[]): readonly FileCount[] {
    return tallyBy(
        rows,
        (row) => row.filename ?? '',
        (file, tally) => ({ file, ...tally }),
    );
}

/**
 * One grouping's figures: the group, how many rows of each severity it holds and
 * the total. The name opens the list narrowed to that group when the caller
 * states where it leads.
 */
function TallyTable({
    heading,
    nameHeader,
    nameOf,
    counts,
    addressOf,
}: {
    readonly heading: string;
    readonly nameHeader: string;
    readonly nameOf: (name: string) => string;
    readonly counts: readonly (Tally & { readonly name: string })[];
    readonly addressOf?: (name: string) => string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.errors';
    return (
        <section className="card p-6">
            <h3 className="mb-3 text-sm font-medium">{heading}</h3>
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-2 py-2 font-medium">{nameHeader}</th>
                        <th className="px-2 py-2 text-right font-medium">
                            {t(`${field}.errorsCount`)}
                        </th>
                        <th className="px-2 py-2 text-right font-medium">
                            {t(`${field}.warningsCount`)}
                        </th>
                        <th className="px-2 py-2 text-right font-medium">
                            {t(`${field}.infosCount`)}
                        </th>
                        <th className="px-2 py-2 text-right font-medium">{t(`${field}.total`)}</th>
                    </tr>
                </thead>
                <tbody>
                    {counts.map((count) => {
                        const address = addressOf?.(count.name);
                        return (
                            <tr key={count.name} className="border-b border-line-subtle">
                                <td className="px-2 py-2">
                                    {address === undefined ? (
                                        nameOf(count.name)
                                    ) : (
                                        <Link
                                            to={address}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {nameOf(count.name)}
                                        </Link>
                                    )}
                                </td>
                                <td className="px-2 py-2 text-right tabular-nums">
                                    {count.errors}
                                </td>
                                <td className="px-2 py-2 text-right tabular-nums">
                                    {count.warnings}
                                </td>
                                <td className="px-2 py-2 text-right tabular-nums">{count.infos}</td>
                                <td className="px-2 py-2 text-right tabular-nums">
                                    {count.errors + count.warnings + count.infos}
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
        </section>
    );
}

/**
 * The rows of a run in two groupings, because a row states two different things:
 * the part of the run it is about, which is its scope, and the log it came from,
 * which is its file. A run leaves several logs and each says something the others
 * do not, so the two are never one distribution: the engine's whole account and
 * the platform's own word about the run are counted apart, log by log.
 */
export function RunErrorsOverview({
    rows,
}: {
    readonly rows: readonly ReportRunErrorView[];
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.errors';
    const scopeName = (scope: string): string => scope || t(`${field}.unscoped`);
    const fileName = (file: string): string => file || t(`${field}.noLog`);
    return (
        <section className="space-y-4">
            <div className="grid gap-4 lg:grid-cols-2">
                <section className="card p-6">
                    <Donut
                        title={t(`${field}.byLog`)}
                        caption={t(`${field}.byLogCaption`)}
                        parts={fileCounts(rows).map((count) => ({
                            name: fileName(count.file),
                            value: count.errors + count.warnings + count.infos,
                        }))}
                        format={(value) => String(value)}
                    />
                </section>
                <section className="card p-6">
                    <Donut
                        title={t(`${field}.byScope`)}
                        caption={t(`${field}.byScopeCaption`)}
                        parts={problemsOf(rows, t(`${field}.unscoped`), t(`${field}.otherScopes`))}
                        format={(value) => String(value)}
                    />
                </section>
            </div>
            <div className="grid gap-4 lg:grid-cols-2">
                <TallyTable
                    heading={t(`${field}.byLogTable`)}
                    nameHeader={t(`${field}.logFile`)}
                    nameOf={fileName}
                    counts={fileCounts(rows).map((count) => ({
                        name: count.file,
                        errors: count.errors,
                        warnings: count.warnings,
                        infos: count.infos,
                    }))}
                    addressOf={fileAddress}
                />
                <TallyTable
                    heading={t(`${field}.byScopeTable`)}
                    nameHeader={t(`${field}.scope`)}
                    nameOf={scopeName}
                    counts={scopeCounts(rows).map((count) => ({
                        name: count.scope,
                        errors: count.errors,
                        warnings: count.warnings,
                        infos: count.infos,
                    }))}
                    addressOf={scopeAddress}
                />
            </div>
        </section>
    );
}

/**
 * The rows a run recorded, one row each: how severe it is, the part of the run
 * it belongs to, the one thing it names, what the engine said and where in the
 * engine it said it. A scope in the address narrows the list to it.
 *
 * A notice is a row like any other, and the filter is how a reader puts the
 * engine's notices aside when they are looking for a failure.
 */
export function RunErrorsTable({
    runId,
    rows: all,
}: {
    readonly runId: string;
    readonly rows: readonly ReportRunErrorView[];
}): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.run.errors';
    const [search, setSearch] = useSearchParams();
    const scope = search.get('scope');
    const file = search.get('file');
    const [severity, setSeverity] = useState<SeverityChoice>('all');
    const inScope = scope === null ? all : all.filter((row) => row.scope === scope);
    const rows = file === null ? inScope : inScope.filter((row) => (row.filename ?? '') === file);
    const files = [...new Set(inScope.map((row) => row.filename ?? ''))].sort();
    const chooseFile = (chosen: string): void => {
        const next = new URLSearchParams(search);
        if (chosen === EVERY_LOG) {
            next.delete('file');
        } else {
            next.set('file', chosen);
        }
        setSearch(next, { replace: true });
    };
    const counts: Readonly<Record<SeverityChoice, number>> = {
        all: rows.length,
        error: rows.filter((row) => row.severity === 'error').length,
        warning: rows.filter((row) => row.severity === 'warning').length,
        info: rows.filter((row) => row.severity === 'info').length,
    };
    const shown = severity === 'all' ? rows : rows.filter((row) => row.severity === severity);

    return (
        <section className="space-y-3">
            {scope !== null && (
                <p className="flex flex-wrap items-center gap-3 text-sm">
                    <span>
                        {t(`${field}.scopeShown`, { scope: scope || t(`${field}.unscoped`) })}
                    </span>
                    <Link
                        to={`?${new URLSearchParams({ ...OUTER, view: 'list' }).toString()}`}
                        className="text-accent hover:text-accent-bright"
                    >
                        {t(`${field}.everyScope`)}
                    </Link>
                </p>
            )}
            <CountFilter
                choices={SEVERITIES}
                chosen={severity}
                countOf={(choice) => counts[choice]}
                labelOf={(choice, count) => t(`${field}.filter.${choice}`, { count })}
                onChoose={setSeverity}
            />
            {files.length > 1 && (
                <CountFilter
                    choices={[EVERY_LOG, ...files]}
                    chosen={file ?? EVERY_LOG}
                    countOf={(choice) =>
                        choice === EVERY_LOG
                            ? inScope.length
                            : inScope.filter((row) => (row.filename ?? '') === choice).length
                    }
                    labelOf={(choice, count) =>
                        `${choice === EVERY_LOG ? t(`${field}.everyLog`) : choice === '' ? t(`${field}.noLogChip`) : choice} (${count})`
                    }
                    onChoose={chooseFile}
                />
            )}
            <RecordTable<ReportRunErrorView>
                source={localSource<ReportRunErrorView>({
                    key: `run-errors-${runId}`,
                    rows: shown,
                    revision: `${scope ?? ''}:${severity}:${shown.length}`,
                    searchOf: (row) =>
                        `${row.scope} ${row.about} ${row.message} ${row.filename ?? ''} ${row.location ?? ''}`,
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
                        cell: (row) => <ErrorSubject error={row} />,
                    },
                    {
                        id: 'message',
                        header: t(`${field}.message`),
                        cell: (row) => <span className="whitespace-pre-wrap">{row.message}</span>,
                    },
                    {
                        id: 'filename',
                        header: t(`${field}.filename`),
                        mono: true,
                        cell: (row) => (
                            <span className="text-ink-muted">{row.filename ?? '—'}</span>
                        ),
                    },
                    {
                        id: 'location',
                        header: t(`${field}.location`),
                        mono: true,
                        cell: (row) => (
                            <span className="text-ink-muted">{row.location ?? '—'}</span>
                        ),
                    },
                ]}
            />
        </section>
    );
}
