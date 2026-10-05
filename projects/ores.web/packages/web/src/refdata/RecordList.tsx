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
 * and is honoured only when the resource declares that field sortable.
 * A `hidden` column starts hidden and is offered in the column menu.
 */
export interface ListColumn {
    readonly id: string;
    readonly header: string;
    readonly cell: (row: RecordRow) => ReactNode;
    readonly mono?: boolean;
    readonly numeric?: boolean;
    readonly sort?: string;
    readonly hidden?: boolean;
    /** The column holds a code of this source in this field, drawn with its flag. */
    readonly flag?: { readonly source: FlagSource; readonly field: string };
}

/** The audit columns every versioned list offers, hidden by default. */
export function auditColumns(t: (key: string) => string): readonly ListColumn[] {
    return [
        {
            id: 'version',
            header: t('refdata.fields.version'),
            cell: (row) => show(row.version),
            mono: true,
            numeric: true,
            hidden: true,
        },
        {
            id: 'modified_by',
            header: t('refdata.fields.modified_by'),
            cell: (row) => show(row['modified_by']),
            hidden: true,
        },
        {
            id: 'performed_by',
            header: t('refdata.fields.performed_by'),
            cell: (row) => show(row['performed_by']),
            hidden: true,
        },
        {
            id: 'recorded_at',
            header: t('refdata.fields.recorded_at'),
            cell: (row) => <RelativeTime at={show(row['recorded_at'])} />,
            mono: true,
            hidden: true,
        },
        {
            id: 'change_reason_code',
            header: t('refdata.fields.change_reason_code'),
            cell: (row) => show(row['change_reason_code']),
            mono: true,
            hidden: true,
        },
        {
            id: 'change_commentary',
            header: t('refdata.fields.change_commentary'),
            cell: (row) => show(row['change_commentary']),
            hidden: true,
        },
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
function useColumnChoice(
    resource: string,
    columns: readonly ListColumn[],
): {
    readonly visible: readonly ListColumn[];
    readonly isShown: (column: ListColumn) => boolean;
    readonly toggle: (column: ListColumn) => void;
} {
    const key = `ores.columns.v1.${resource}`;
    const [choice, setChoice] = useState<Readonly<Record<string, boolean>>>(() => {
        try {
            return JSON.parse(window.localStorage.getItem(key) ?? '{}') as Record<string, boolean>;
        } catch {
            return {};
        }
    });
    const isShown = (column: ListColumn): boolean => choice[column.id] ?? column.hidden !== true;
    const toggle = (column: ListColumn): void => {
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
export function RecordList({
    resource,
    title,
    lead,
    crumbs,
    columns,
    pathOf,
    addLabel,
    onAdd,
}: {
    readonly resource: string;
    readonly title: string;
    readonly lead: string;
    readonly crumbs: readonly { readonly label: string; readonly to?: string }[];
    readonly columns: readonly ListColumn[];
    readonly pathOf: (row: RecordRow) => string;
    readonly addLabel: string;
    readonly onAdd: () => void;
}): ReactNode {
    const { t, language } = useTranslation();
    const navigate = useNavigate();
    const state = useListState();
    const registry = useRegistry();
    const may = useRecordPermissions(resource);
    const entry = registry.data?.find((candidate) => candidate.key === resource);
    const sortable = new Set(entry?.sortable ?? []);
    const plural = title.toLocaleLowerCase(language);
    const all = [...columns, ...auditColumns(t)];
    const { visible, isShown, toggle } = useColumnChoice(resource, all);
    const [typed, setTyped] = useState(state.search);
    const page = {
        offset: state.offset,
        limit: state.limit,
        search: state.search,
        sort: state.sort,
        descending: state.descending,
    };
    const rows = useQuery({
        queryKey: ['records', resource, 'page', page],
        queryFn: () => api.recordPage(resource, page),
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
    const open = (row: RecordRow): void => void navigate(pathOf(row));
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
                            {may.write && (
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
                    {entry?.search === true && (
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
                                                {may.write && (
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
                                                        code={show(row[column.flag.field])}
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
