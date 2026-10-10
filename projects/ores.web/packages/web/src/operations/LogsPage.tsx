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

/**
 * Read the telemetry logs, from
 * doc/knowledge/journeys/operations/journey_read_the_telemetry_logs.org.
 *
 * The filter bar is the prototype's, in its order: range, level, source,
 * component, tag and message, combining with AND as the read's query does. The
 * table is the prototype's columns, and the paging line states the entries a
 * page shows out of everything the filter matches.
 *
 * The source list offers `server` alone. Every stored entry is stamped
 * `source=server`, nothing publishes a client entry, and the journey says the
 * screen must not offer a filter value that can never match; the `client`
 * value the prototype drew is therefore left out rather than answered with an
 * empty page forever. The level list carries the store's own lower-case words
 * for the same reason.
 *
 * Times are the entries' UTC emission times, labelled as such. The read time
 * is the deployment's, stamped by the BFF, because no entry carries it and a
 * browser's own clock would date the reading by a time the deployment never
 * measured.
 */

import { useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';
import type { LogsView } from '@ores/wire-protocol/browser';
import { api, type LogsRange } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Input, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';
import { AreaTrail } from '../shell/AreaTrail.js';

/** The range presets the screen offers, in the order it shows them. */
const RANGES: readonly LogsRange[] = ['15m', '1h', '6h', '24h'];

/**
 * The severity words the store holds, lower case as the sink writes them.
 *
 * The read's level filter is an equality, so the words offered must be the
 * words stored. The prototype offered them upper case, which could never
 * match.
 */
const LEVELS = ['error', 'warn', 'info', 'debug'] as const;
type LogLevel = (typeof LEVELS)[number];

/**
 * The source words the store can hold.
 *
 * `server` alone: the ingest stamps every entry it stores as a server entry,
 * and no client publisher exists, so offering `client` would be a filter that
 * can never match.
 */
const SOURCES = ['server'] as const;

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    '7B820710-161C-4926-B5AA-5EF2772A3652',
    'EE26E58C-F389-4E82-BF96-EC5324F9F795',
    '22AC8DD8-A440-4330-9992-47A5E9985473',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
];

/** Where the logs read's answer is cached, by the filter and page it read. */
export const LOGS_QUERY_KEY = 'operations-logs' as const;

/** The entries one page holds, which the reply states beside its total. */
export const LOGS_PAGE_SIZE = 100;

/** The filters the screen holds, before any of them is applied. */
export interface LogsFilter {
    readonly range: LogsRange;
    readonly level: string;
    readonly source: string;
    readonly component: string;
    readonly tag: string;
    readonly message: string;
}

/** The filter the screen opens on: a recent range, with everything else off. */
export const DEFAULT_LOGS_FILTER: LogsFilter = {
    range: '1h',
    level: '',
    source: '',
    component: '',
    tag: '',
    message: '',
};

/**
 * The instant a stored entry was emitted, as a wall clock a person reads, in
 * UTC.
 *
 * Nothing when the time is unreadable, because a line dated wrongly is worse
 * than a line left undated.
 */
export function logTime(at: string | null): string | undefined {
    if (at === null || !isWireTimestamp(at)) {
        return undefined;
    }
    return `${fromWireTimestamp(at).toISOString().slice(11, 19)} UTC`;
}

/** Whether two filters name the same read, field by field. */
export function sameFilter(left: LogsFilter, right: LogsFilter): boolean {
    return (
        left.range === right.range &&
        left.level === right.level &&
        left.source === right.source &&
        left.component === right.component &&
        left.tag === right.tag &&
        left.message === right.message
    );
}

/** The entries a page shows, counting from one, or zeros when it shows none. */
export function shownRange(view: LogsView): { readonly from: number; readonly to: number } {
    const rows = view.entries.length;
    return rows === 0 ? { from: 0, to: 0 } : { from: view.offset + 1, to: view.offset + rows };
}

/** Whether a page waits before this one. */
export function hasPrevious(view: LogsView): boolean {
    return view.offset > 0;
}

/** Whether the total reaches past this page. */
export function hasNext(view: LogsView): boolean {
    return view.offset + view.limit < view.total;
}

const LEVEL_TONES: Readonly<Record<LogLevel, 'warn' | 'muted' | 'neutral'>> = {
    error: 'warn',
    warn: 'warn',
    info: 'neutral',
    debug: 'muted',
};

/** The tone one level is painted with, for the levels the store holds. */
export function levelTone(level: string): 'warn' | 'muted' | 'neutral' {
    const known = LEVELS.find((candidate) => candidate === level);
    return known === undefined ? 'neutral' : LEVEL_TONES[known];
}

/** The filter bar: the prototype's controls, in the prototype's order. */
function FilterBar({
    draft,
    onChange,
    onSearch,
}: {
    readonly draft: LogsFilter;
    readonly onChange: (next: LogsFilter) => void;
    readonly onSearch: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card flex flex-wrap items-end gap-4 p-4">
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.range.label')}
                <Select
                    value={draft.range}
                    onChange={(event) =>
                        onChange({ ...draft, range: event.target.value as LogsRange })
                    }
                >
                    {RANGES.map((option) => (
                        <option key={option} value={option}>
                            {t(`operations.logs.range.${option}`)}
                        </option>
                    ))}
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.level.label')}
                <Select
                    value={draft.level}
                    onChange={(event) => onChange({ ...draft, level: event.target.value })}
                >
                    <option value="">{t('operations.logs.level.any')}</option>
                    {LEVELS.map((option) => (
                        <option key={option} value={option}>
                            {option}
                        </option>
                    ))}
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.source.label')}
                <Select
                    value={draft.source}
                    onChange={(event) => onChange({ ...draft, source: event.target.value })}
                >
                    <option value="">{t('operations.logs.source.any')}</option>
                    {SOURCES.map((option) => (
                        <option key={option} value={option}>
                            {option}
                        </option>
                    ))}
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.component.label')}
                <Input
                    value={draft.component}
                    placeholder={t('operations.logs.component.placeholder')}
                    onChange={(event) => onChange({ ...draft, component: event.target.value })}
                />
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.tag.label')}
                <Input
                    value={draft.tag}
                    placeholder={t('operations.logs.tag.placeholder')}
                    onChange={(event) => onChange({ ...draft, tag: event.target.value })}
                />
            </label>
            <label className="flex flex-1 flex-col gap-1 text-xs text-ink-muted">
                {t('operations.logs.message.label')}
                <Input
                    value={draft.message}
                    placeholder={t('operations.logs.message.placeholder')}
                    onChange={(event) => onChange({ ...draft, message: event.target.value })}
                />
            </label>
            <Button variant="secondary" onClick={onSearch}>
                {t('operations.logs.search')}
            </Button>
        </section>
    );
}

/** The read-only table of entries the filter selected, and the paging line. */
function EntriesPanel({
    view,
    onPrevious,
    onNext,
}: {
    readonly view: LogsView;
    readonly onPrevious: () => void;
    readonly onNext: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const { from, to } = shownRange(view);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.logs.entries.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('operations.logs.entries.count', {
                        shown: view.entries.length,
                        total: view.total,
                    })}
                </span>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">{t('operations.logs.columns.time')}</th>
                        <th className="py-1 font-normal">{t('operations.logs.columns.level')}</th>
                        <th className="py-1 font-normal">{t('operations.logs.columns.source')}</th>
                        <th className="py-1 font-normal">{t('operations.logs.columns.name')}</th>
                        <th className="py-1 font-normal">
                            {t('operations.logs.columns.component')}
                        </th>
                        <th className="py-1 font-normal">{t('operations.logs.columns.message')}</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {view.entries.map((entry) => (
                        <tr key={entry.id}>
                            <td className="py-2 font-mono">{logTime(entry.timestamp) ?? '—'}</td>
                            <td className="py-2">
                                <Tag tone={levelTone(entry.level)}>{entry.level}</Tag>
                            </td>
                            <td className="py-2 font-mono">{entry.source}</td>
                            <td className="py-2 font-mono">{entry.source_name}</td>
                            <td className="py-2 font-mono">{entry.component}</td>
                            <td className="py-2">{entry.message}</td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <div className="flex flex-wrap items-center gap-3">
                <span className="text-xs text-ink-faint">
                    {t('operations.logs.entries.showing', { from, to, total: view.total })}
                </span>
                <Button
                    size="sm"
                    variant="secondary"
                    disabled={!hasPrevious(view)}
                    onClick={onPrevious}
                >
                    {t('operations.logs.entries.previous')}
                </Button>
                <Button size="sm" variant="secondary" disabled={!hasNext(view)} onClick={onNext}>
                    {t('operations.logs.entries.next')}
                </Button>
                <span className="text-xs text-ink-faint">
                    {t('operations.logs.entries.paging')}
                </span>
            </div>
        </section>
    );
}

/** The empty answer: a filter that matched nothing, stated as a state. */
function EmptyPanel(): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.logs.entries.title')}</h2>
                <Tag tone="warn">{t('operations.logs.entries.nothingMatches')}</Tag>
            </header>
            <p className="text-sm text-ink-muted">{t('operations.logs.entries.empty')}</p>
            <p className="text-xs text-ink-faint">{t('operations.logs.entries.emptyHint')}</p>
        </section>
    );
}

export function LogsPage(): ReactNode {
    const { t } = useTranslation();
    const [draft, setDraft] = useState<LogsFilter>(DEFAULT_LOGS_FILTER);
    const [applied, setApplied] = useState<LogsFilter>(DEFAULT_LOGS_FILTER);
    const [offset, setOffset] = useState(0);
    const logs = useQuery({
        queryKey: [LOGS_QUERY_KEY, applied, offset],
        queryFn: () => api.logs({ ...applied, offset, limit: LOGS_PAGE_SIZE }),
    });

    /*
     * Apply re-reads: a new filter moves the query to its own key and starts
     * at the first page, and the same filter asks the current key again, so
     * the person can refresh a reading without changing what they asked for.
     */
    const search = (): void => {
        if (offset === 0 && sameFilter(draft, applied)) {
            void logs.refetch();
            return;
        }
        setOffset(0);
        setApplied(draft);
    };

    const readAt = logs.data === undefined ? undefined : logTime(logs.data.read_at);
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.logs.gap.client.title'),
            body: t('operations.logs.gap.client.body'),
            journey: t('operations.journeys.logs'),
        },
        {
            title: t('operations.logs.gap.and.title'),
            body: t('operations.logs.gap.and.body'),
            journey: t('operations.journeys.logs'),
        },
        {
            title: t('operations.logs.gap.suggest.title'),
            body: t('operations.logs.gap.suggest.body'),
            journey: t('operations.journeys.logs'),
        },
        {
            title: t('operations.logs.gap.sql.title'),
            body: t('operations.logs.gap.sql.body'),
            journey: t('operations.journeys.logs'),
        },
        {
            title: t('operations.logs.gap.stats.title'),
            body: t('operations.logs.gap.stats.body'),
            journey: t('operations.journeys.logs'),
        },
        {
            title: t('operations.logs.gap.permission.title'),
            body: t('operations.logs.gap.permission.body'),
            journey: t('operations.journeys.logs'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.logs')} />
                <PageHeader
                    title={t('operations.logs.title')}
                    description={t('operations.logs.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            {readAt !== undefined && (
                                <span className="text-xs text-ink-faint">
                                    {t('operations.logs.readAt', { at: readAt })}
                                </span>
                            )}
                            <OperationsBack />
                        </div>
                    }
                />
            </div>

            <FilterBar draft={draft} onChange={setDraft} onSearch={search} />

            {logs.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : logs.isError ? (
                <Notice tone="error">{logs.error.message}</Notice>
            ) : logs.data.entries.length === 0 ? (
                <EmptyPanel />
            ) : (
                <EntriesPanel
                    view={logs.data}
                    onPrevious={() => setOffset(Math.max(0, offset - LOGS_PAGE_SIZE))}
                    onNext={() => setOffset(offset + LOGS_PAGE_SIZE)}
                />
            )}

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
