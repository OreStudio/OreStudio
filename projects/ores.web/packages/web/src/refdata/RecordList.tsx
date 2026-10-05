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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { useNavigate, useSearchParams } from 'react-router';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { FlagOf, type FlagSource } from '../images/flags.js';
import { Icon } from '../ui/Icon.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { Button, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { show, useRecordPermissions, useRegistry } from './records.js';
import { Crumbs } from './shared.js';

/** How long the search waits after the last keystroke before it asks the server. */
const SEARCH_PAUSE_MS = 300;

/**
 * One column of a list. `sort` names the server field the column orders by,
 * and is honoured only when the source declares that field sortable.
 * A `hidden` column starts hidden and is offered in the column menu.
 */
export interface ListColumn<Row = RecordRow> {
    readonly id: string;
    readonly header: string;
    readonly cell: (row: Row) => ReactNode;
    readonly mono?: boolean;
    readonly numeric?: boolean;
    readonly sort?: string;
    readonly hidden?: boolean;
    /** The column holds a code of this source, drawn with its flag. */
    readonly flag?: { readonly source: FlagSource; readonly code: (row: Row) => string };
}

/**
 * Where a list's rows come from: one page at a time from the server, with the
 * search and the orders the server supports. `key` names the list, for its
 * cache and for the columns a person chose. Every record list, refdata or not,
 * draws through =RecordList= from a source.
 */
export interface ListSource<Row> {
    readonly key: string;
    readonly read: (page: PageRequest) => Promise<{
        readonly rows: readonly Row[];
        readonly total: number;
    }>;
    readonly search: boolean;
    readonly sortable: readonly string[];
    readonly mayAdd: boolean;
    /** Reads a row's audit field by its server name; a source without one shows no audit columns. */
    readonly audit?: (row: Row, field: string) => unknown;
}

/** How a list asks the server for one page. */
export interface PageRequest {
    readonly offset: number;
    readonly limit: number;
    readonly search: string;
    readonly sort: string;
    readonly descending: boolean;
}

/** The source of a refdata record kind: the registry says what it searches and sorts. */
export function useRecordSource(resource: string): ListSource<RecordRow> {
    const registry = useRegistry();
    const may = useRecordPermissions(resource);
    const entry = registry.data?.find((candidate) => candidate.key === resource);
    return {
        key: resource,
        read: (page) => api.recordPage(resource, page),
        search: entry?.search === true,
        sortable: entry?.sortable ?? [],
        mayAdd: may.write,
        audit: (row, field) => (field === 'version' ? row.version : row[field]),
    };
}

/** The audit columns every versioned list offers, hidden by default. */
export function auditColumns<Row>(
    t: (key: string) => string,
    field: (row: Row, name: string) => unknown,
): readonly ListColumn<Row>[] {
    const text = (name: string, hidden = true, mono = false): ListColumn<Row> => ({
        id: name,
        header: t(`refdata.fields.${name}`),
        cell: (row) => show(field(row, name)),
        mono,
        hidden,
    });
    return [
        { ...text('version', true, true), numeric: true },
        text('modified_by'),
        text('performed_by'),
        {
            id: 'recorded_at',
            header: t('refdata.fields.recorded_at'),
            cell: (row) => <RelativeTime at={show(field(row, 'recorded_at'))} />,
            mono: true,
            hidden: true,
        },
        text('change_reason_code', true, true),
        text('change_commentary'),
    ];
}

/** The page a list shows, held in the address so a link and Back restore it. */
export function useListState(): {
    readonly offset: number;
    readonly limit: number;
    readonly search: string;
    readonly sort: string;
    readonly descending: boolean;
    readonly move: (offset: number) => void;
    readonly resize: (limit: number) => void;
    readonly find: (search: string) => void;
    readonly order: (sort: string, descending: boolean) => void;
} {
    const [params, setParams] = useSearchParams();
    const number = (name: string, fallback: number): number => {
        const value = Number.parseInt(params.get(name) ?? '', 10);
        return Number.isNaN(value) || value < 0 ? fallback : value;
    };
    const update = (changes: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(params);
        for (const [name, value] of Object.entries(changes)) {
            if (value === '') {
                next.delete(name);
            } else {
                next.set(name, value);
            }
        }
        setParams(next, { replace: true });
    };
    return {
        offset: number('from', 0),
        limit: Math.min(Math.max(number('size', DEFAULT_PAGE_SIZE), 1), 1000),
        search: params.get('q') ?? '',
        sort: params.get('sort') ?? '',
        descending: params.get('desc') === '1',
        move: (offset) => update({ from: offset === 0 ? '' : String(offset) }),
        resize: (limit) =>
            update({ from: '', size: limit === DEFAULT_PAGE_SIZE ? '' : String(limit) }),
        find: (search) => update({ from: '', q: search }),
        order: (sort, descending) => update({ from: '', sort, desc: descending ? '1' : '' }),
    };
}

/** Which columns a person hid or showed, remembered in this browser per list. */
function useColumnChoice<Row>(
    list: string,
    columns: readonly ListColumn<Row>[],
): {
    readonly visible: readonly ListColumn<Row>[];
    readonly isShown: (column: ListColumn<Row>) => boolean;
    readonly toggle: (column: ListColumn<Row>) => void;
} {
    const key = `ores.columns.v1.${list}`;
    const [choice, setChoice] = useState<Readonly<Record<string, boolean>>>(() => {
        try {
            return JSON.parse(window.localStorage.getItem(key) ?? '{}') as Record<string, boolean>;
        } catch {
            return {};
        }
    });
    const isShown = (column: ListColumn<Row>): boolean =>
        choice[column.id] ?? column.hidden !== true;
    const toggle = (column: ListColumn<Row>): void => {
        const next = { ...choice, [column.id]: !isShown(column) };
        setChoice(next);
        try {
            window.localStorage.setItem(key, JSON.stringify(next));
        } catch {
            return;
        }
    };
    return { visible: columns.filter(isShown), isShown, toggle };
}

/**
 * The list of one record kind, as the record screen standard sets it: the
 * header with Refresh and Add, the server search, the table with server
 * sorting and a column menu, the states, and the pager. Every record list is
 * this component; a screen supplies its columns and the address of a row.
 */
export function RecordList<Row>({
    source,
    title,
    lead,
    crumbs,
    columns,
    pathOf,
    addLabel,
    onAdd,
}: {
    readonly source: ListSource<Row>;
    readonly title: string;
    readonly lead: string;
    readonly crumbs: readonly { readonly label: string; readonly to?: string }[];
    readonly columns: readonly ListColumn<Row>[];
    readonly pathOf: (row: Row) => string;
    readonly addLabel?: string;
    readonly onAdd?: () => void;
}): ReactNode {
    const { t, language } = useTranslation();
    const navigate = useNavigate();
    const state = useListState();
    const sortable = new Set(source.sortable);
    const mayAdd = source.mayAdd && onAdd !== undefined && addLabel !== undefined;
    const plural = title.toLocaleLowerCase(language);
    const all = [...columns, ...(source.audit === undefined ? [] : auditColumns(t, source.audit))];
    const { visible, isShown, toggle } = useColumnChoice(source.key, all);
    const [typed, setTyped] = useState(state.search);
    const page = {
        offset: state.offset,
        limit: state.limit,
        search: state.search,
        sort: state.sort,
        descending: state.descending,
    };
    const rows = useQuery({
        queryKey: ['records', source.key, 'page', page],
        queryFn: () => source.read(page),
        placeholderData: keepPreviousData,
    });

    const find = useRef(state.find);
    find.current = state.find;
    useEffect(() => {
        if (typed.trim() === state.search) {
            return undefined;
        }
        const timer = window.setTimeout(() => find.current(typed.trim()), SEARCH_PAUSE_MS);
        return () => window.clearTimeout(timer);
    }, [typed, state.search]);

    const refresh = (): void => {
        state.move(0);
        void rows.refetch();
    };
    const sortBy = (field: string): void =>
        state.order(field, state.sort === field ? !state.descending : false);
    const open = (row: Row): void => void navigate(pathOf(row));
    const data = rows.data;
    const bounds = pageBounds(state.offset, data?.rows.length ?? 0);

    return (
        <div className="space-y-4">
            <div>
                <Crumbs parts={crumbs} />
                <PageHeader
                    title={title}
                    description={lead}
                    actions={
                        <div className="flex gap-2">
                            <Button icon="refresh" onClick={refresh} pending={rows.isFetching}>
                                {t('refdata.records.refresh')}
                            </Button>
                            {mayAdd && (
                                <Button variant="primary" icon="add" onClick={onAdd}>
                                    {addLabel}
                                </Button>
                            )}
                        </div>
                    }
                />
            </div>
            <section className="overflow-hidden rounded-md border border-line">
                <div className="flex flex-wrap items-center gap-3 border-b border-line p-3">
                    {source.search && (
                        <label className="relative min-w-60 flex-1">
                            <span className="pointer-events-none absolute inset-y-0 left-3 flex items-center text-ink-faint">
                                <Icon name="search" size={16} />
                            </span>
                            <Input
                                type="search"
                                className="pl-9"
                                value={typed}
                                placeholder={t('refdata.records.search')}
                                aria-label={t('refdata.records.search')}
                                onChange={(event) => setTyped(event.target.value)}
                            />
                        </label>
                    )}
                    <details className="relative ml-auto text-sm">
                        <summary className="cursor-pointer rounded-md px-2 py-1 text-ink-muted hover:bg-surface-hover hover:text-ink">
                            {t('refdata.records.columns')}
                        </summary>
                        <div className="absolute right-0 z-10 mt-1 w-56 rounded-md border border-line bg-surface-raised p-2 shadow-lg">
                            {all.map((column) => (
                                <label
                                    key={column.id}
                                    className="flex items-center gap-2 px-1 py-0.5"
                                >
                                    <input
                                        type="checkbox"
                                        checked={isShown(column)}
                                        onChange={() => toggle(column)}
                                    />
                                    {column.header}
                                </label>
                            ))}
                        </div>
                    </details>
                </div>
                {rows.isError && (
                    <div className="p-3">
                        <Notice tone="error">
                            <span className="mr-3">{rows.error.message}</span>
                            <Button size="sm" onClick={() => void rows.refetch()}>
                                {t('refdata.records.retry')}
                            </Button>
                        </Notice>
                    </div>
                )}
                <div className="overflow-x-auto">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {visible.map((column) => {
                                    const canSort =
                                        column.sort !== undefined && sortable.has(column.sort);
                                    const sorted = canSort && state.sort === column.sort;
                                    return (
                                        <th
                                            key={column.id}
                                            aria-sort={
                                                sorted
                                                    ? state.descending
                                                        ? 'descending'
                                                        : 'ascending'
                                                    : undefined
                                            }
                                            className={`px-4 py-2 font-medium ${column.numeric === true ? 'text-right' : ''}`}
                                        >
                                            {canSort && column.sort !== undefined ? (
                                                <button
                                                    type="button"
                                                    className="hover:text-ink"
                                                    onClick={() => sortBy(column.sort ?? '')}
                                                >
                                                    {column.header}
                                                    {sorted ? (state.descending ? ' ↓' : ' ↑') : ''}
                                                </button>
                                            ) : (
                                                column.header
                                            )}
                                        </th>
                                    );
                                })}
                            </tr>
                        </thead>
                        <tbody className={rows.isPlaceholderData ? 'opacity-60' : ''}>
                            {data === undefined && !rows.isError && (
                                <tr>
                                    <td
                                        colSpan={visible.length}
                                        className="px-4 py-3 text-ink-muted"
                                    >
                                        {t('common.loading')}
                                    </td>
                                </tr>
                            )}
                            {data !== undefined && data.rows.length === 0 && (
                                <tr>
                                    <td
                                        colSpan={visible.length}
                                        className="px-4 py-3 text-ink-muted"
                                    >
                                        {state.search === '' ? (
                                            <span className="flex items-center gap-3">
                                                {t('refdata.records.noRecords', { plural })}
                                                {mayAdd && (
                                                    <Button size="sm" icon="add" onClick={onAdd}>
                                                        {addLabel}
                                                    </Button>
                                                )}
                                            </span>
                                        ) : (
                                            <span className="flex items-center gap-3">
                                                {t('refdata.records.noMatchSearch', { plural })}
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    onClick={() => {
                                                        setTyped('');
                                                        state.find('');
                                                    }}
                                                >
                                                    {t('refdata.records.clearSearch')}
                                                </Button>
                                            </span>
                                        )}
                                    </td>
                                </tr>
                            )}
                            {(data?.rows ?? []).map((row) => (
                                <tr
                                    key={pathOf(row)}
                                    tabIndex={0}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover focus:bg-surface-hover focus:outline-none"
                                    onClick={() => open(row)}
                                    onKeyDown={(event) => {
                                        if (event.key === 'Enter') {
                                            open(row);
                                        }
                                    }}
                                >
                                    {visible.map((column) => (
                                        <td
                                            key={column.id}
                                            className={[
                                                'px-4 py-2',
                                                column.mono === true
                                                    ? 'font-mono text-xs text-ink-muted'
                                                    : '',
                                                column.numeric === true
                                                    ? 'text-right tabular-nums'
                                                    : '',
                                            ].join(' ')}
                                        >
                                            {column.flag === undefined ? (
                                                column.cell(row)
                                            ) : (
                                                <span className="inline-flex items-center gap-2">
                                                    <FlagOf
                                                        source={column.flag.source}
                                                        code={column.flag.code(row)}
                                                    />
                                                    {column.cell(row)}
                                                </span>
                                            )}
                                        </td>
                                    ))}
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
                {data !== undefined && (
                    <div className="border-t border-line px-4 pb-3">
                        <Pager
                            offset={state.offset}
                            shown={data.rows.length}
                            total={data.total}
                            pageSize={state.limit}
                            showing={t(
                                rows.isPlaceholderData
                                    ? 'refdata.records.refreshing'
                                    : 'refdata.records.showing',
                                {
                                    first: String(bounds.first),
                                    last: String(bounds.last),
                                    total: String(data.total),
                                },
                            )}
                            onMove={state.move}
                            onPageSize={state.resize}
                        />
                    </div>
                )}
            </section>
        </div>
    );
}
