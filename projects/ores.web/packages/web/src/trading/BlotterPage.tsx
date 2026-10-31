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

import { useMemo, useState, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link, useNavigate } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { blotter, type BlotterNode, type BlotterRow, type BlotterView } from '../api/blotter.js';
import { RecordList, type ListColumn, type ListSource } from '../refdata/RecordList.js';
import { Button, cx, Select, Tag } from '../ui/Primitives.js';
import { Tooltip } from '../ui/Tooltip.js';
import { bookPath, counterpartyPath, PATHS, tradePath } from '../shell/paths.js';
import { useAreaParts } from '../shell/AreaTrail.js';
import { assetClassColour } from '../marketdata/assetClassColour.js';

/** The sorts the read answers, in the order the table offers them. */
const SORTS = ['trade_date', 'instrument_type', 'status', 'row_kind'] as const;

/** The rail as a tree: each node's children, in the order the read stated them. */
function childrenOf(nodes: readonly BlotterNode[]): ReadonlyMap<string, readonly BlotterNode[]> {
    const children = new Map<string, BlotterNode[]>();
    for (const node of nodes) {
        if (node.id === '') {
            continue;
        }
        children.set(node.parent, [...(children.get(node.parent) ?? []), node]);
    }
    return children;
}

/**
 * One node of the rail: its label and the number of rows placed under it, on the
 * same line, indented by its depth.
 *
 * A node with nothing under it is still drawn: a book nobody has booked to is a
 * fact about the party, not a row to hide.
 */
function RailNode({
    node,
    depth,
    selected,
    onSelect,
}: {
    readonly node: BlotterNode;
    readonly depth: number;
    readonly selected: string;
    readonly onSelect: (id: string) => void;
}): ReactNode {
    return (
        <button
            type="button"
            onClick={() => {
                onSelect(node.id === selected ? '' : node.id);
            }}
            aria-pressed={node.id === selected}
            className={
                node.id === selected
                    ? 'flex w-full items-center justify-between gap-2 rounded px-2 py-1 text-left text-accent-bright bg-accent/10'
                    : 'flex w-full items-center justify-between gap-2 rounded px-2 py-1 text-left hover:bg-surface-raised'
            }
            style={{ paddingLeft: `${0.5 + depth * 0.9}rem` }}
        >
            <span className="flex min-w-0 items-center gap-2">
                {node.kind === 'instrument' ? (
                    <span
                        aria-hidden="true"
                        className={cx(
                            'size-2 shrink-0 rounded-sm',
                            assetClassColour(node.id.replace('instrument:', '')).dot,
                        )}
                    />
                ) : null}
                <span className="truncate text-sm">{node.label}</span>
            </span>
            <span className="shrink-0 text-xs tabular-nums text-ink-muted">{node.count}</span>
        </button>
    );
}

function Rail({
    nodes,
    selected,
    onSelect,
}: {
    readonly nodes: readonly BlotterNode[];
    readonly selected: string;
    readonly onSelect: (id: string) => void;
}): ReactNode {
    const children = useMemo(() => childrenOf(nodes), [nodes]);
    const party = nodes.find((node) => node.id === '');
    const walk = (parent: string, depth: number): ReactNode[] =>
        (children.get(parent) ?? []).flatMap((node) => [
            <RailNode
                key={node.id}
                node={node}
                depth={depth}
                selected={selected}
                onSelect={onSelect}
            />,
            ...walk(node.id, depth + 1),
        ]);
    return (
        <nav aria-label="rail" className="mt-2 flex flex-col gap-0.5">
            {party === undefined ? null : (
                <RailNode node={party} depth={0} selected={selected} onSelect={onSelect} />
            )}
            {walk('', 1)}
        </nav>
    );
}

/**
 * The blotter: the trades and structures a person may see, read by the grouping
 * they choose.
 *
 * The rows are the same whichever grouping is chosen — the trades no structure
 * holds, and each structure as one row — and only the rail changes: the party's
 * books, or the counterparties the rows face. A structure whose legs span books
 * is placed on the node that holds them all, and its row says so.
 */
export function BlotterPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [view, setView] = useState<BlotterView>('portfolio');
    const [node, setNode] = useState('');
    const [flat, setFlat] = useState(false);
    /*
     * The rail is its own read. It cannot ride on the list's page: the list
     * caches a page by its key, so returning to a grouping it has already read
     * answers from the cache and the rail would wait behind a request that never
     * happens.
     */
    const rail = useQuery({
        queryKey: ['blotter-rail', view],
        queryFn: () =>
            blotter.page({
                view,
                // The rail is the whole party's: choosing a node narrows the list,
                // not the tree.
                node: '',
                flat: false,
                search: '',
                offset: 0,
                limit: 1,
                sort: 'trade_date',
                descending: true,
            }),
    });

    /*
     * The rail is read with the page, not before it: its figures are the rows
     * placed on each node, so they come back in the answer to the page the list
     * already asks for.
     */
    const source: ListSource<BlotterRow> = useMemo(
        () => ({
            key: `blotter:${view}:${node}:${flat ? 'flat' : 'top'}`,
            read: async (page) => {
                const answer = await blotter.page({
                    view,
                    node,
                    flat,
                    search: page.search,
                    offset: page.offset,
                    limit: page.limit,
                    sort: page.sort,
                    descending: page.descending,
                });
                return { rows: answer.rows, total: answer.total };
            },
            search: true,
            sortable: [...SORTS],
            mayAdd: false,
        }),
        [view, node, flat],
    );

    const nodes = rail.data?.tree ?? [];
    const columns: readonly ListColumn<BlotterRow>[] = [
        {
            id: 'identifier',
            header: t('trading.blotter.identifier'),
            mono: true,
            cell: (row) => (
                <>
                    <Link
                        className="block max-w-[11rem] truncate text-accent-bright hover:underline"
                        title={row.reference || row.id}
                        to={
                            row.row_kind === 'structure'
                                ? `${PATHS.structures}/${encodeURIComponent(row.reference || row.id)}`
                                : tradePath(row.id)
                        }
                    >
                        {row.reference || row.id}
                    </Link>
                    {row.spans_books ? (
                        <>
                            {' '}
                            <Tag tone="warn" small>
                                {t('trading.blotter.spansBooks')}
                            </Tag>
                        </>
                    ) : null}
                </>
            ),
        },
        {
            id: 'structure',
            header: t('trading.blotter.structureColumn'),
            mono: true,
            hidden: !flat,
            cell: (row) =>
                row.structure_reference === '' ? (
                    '—'
                ) : (
                    <span className="block max-w-[7rem] truncate" title={row.structure_reference}>
                        {row.structure_reference}
                    </span>
                ),
        },
        {
            /*
             * The product a row belongs to, which is short enough to read down a
             * column; the instrument's own name is the cell's tooltip, and holds
             * the whole of it.
             */
            id: 'what',
            header: t('trading.blotter.what'),
            sort: 'instrument_type',
            cell: (row) => {
                const code = row.product_code || row.instrument_type;
                if (code === '') {
                    return '—';
                }
                return (
                    <span className="whitespace-nowrap">
                        <span
                            className="font-mono"
                            title={
                                row.product_name || row.instrument_type_name || row.instrument_type
                            }
                        >
                            <Tag small>{code.toUpperCase()}</Tag>
                        </span>
                    </span>
                );
            },
        },
        {
            id: 'book',
            header: t('trading.blotter.book'),
            mono: true,
            /*
             * A structure states the books its legs sit in and has no book of its
             * own to link to; a trade has one, and its name is the way to it.
             */
            cell: (row) => {
                if (row.book_name === '') {
                    return '—';
                }
                return row.book_id === '' ? (
                    <span className="block max-w-[8rem] truncate" title={row.book_name}>
                        {row.book_name}
                    </span>
                ) : (
                    <Link
                        className="block max-w-[8rem] truncate hover:underline"
                        title={row.book_name}
                        to={bookPath(row.book_id)}
                    >
                        {row.book_name}
                    </Link>
                );
            },
        },
        {
            id: 'counterparty',
            header: t('trading.blotter.counterparty'),
            cell: (row) =>
                row.counterparty_id === '' ? (
                    '—'
                ) : (
                    <Link
                        className="block max-w-[9rem] truncate hover:underline"
                        title={row.counterparty_name}
                        to={counterpartyPath(row.counterparty_id)}
                    >
                        {row.counterparty_name || '—'}
                    </Link>
                ),
        },
        {
            id: 'date',
            header: t('trading.blotter.date'),
            mono: true,
            sort: 'trade_date',
            cell: (row) => (
                <span className="block w-[5rem] whitespace-nowrap tabular-nums">
                    {row.trade_date || '—'}
                </span>
            ),
        },
        {
            id: 'state',
            header: t('trading.blotter.state'),
            sort: 'status',
            cell: (row) => <span className="whitespace-nowrap">{row.status || '—'}</span>,
        },
    ];

    return (
        <div className="flex flex-col gap-4 lg:flex-row lg:items-start">
            <aside className="shrink-0 rounded-card border border-line bg-surface p-3 lg:w-72">
                <div className="flex items-center justify-between gap-2">
                    <h2 className="text-sm font-semibold">{t('trading.blotter.rail')}</h2>
                </div>
                <Select
                    aria-label={t('trading.blotter.view')}
                    className="mt-2 w-full"
                    value={view}
                    onChange={(event) => {
                        const chosen = event.target.value;
                        setView(
                            chosen === 'counterparty' || chosen === 'instrument'
                                ? chosen
                                : 'portfolio',
                        );
                        setNode('');
                    }}
                >
                    <option value="portfolio">{t('trading.blotter.viewPortfolio')}</option>
                    <option value="counterparty">{t('trading.blotter.viewCounterparty')}</option>
                    <option value="instrument">{t('trading.blotter.viewInstrument')}</option>
                </Select>
                {nodes.length === 0 ? (
                    <p className="mt-3 px-2 text-sm text-ink-muted">
                        {t('trading.blotter.railLoading')}
                    </p>
                ) : (
                    <Rail nodes={nodes} selected={node} onSelect={setNode} />
                )}
            </aside>
            <div className="min-w-0 flex-1">
                <RecordList
                    source={source}
                    title={t('trading.blotter.title')}
                    lead={t('trading.blotter.lead')}
                    crumbs={useAreaParts('trading', t('trading.blotter.title'))}
                    columns={columns}
                    keyOf={(row) => `${row.row_kind}:${row.id}`}
                    pathOf={(row) =>
                        row.row_kind === 'structure'
                            ? `${PATHS.structures}/${encodeURIComponent(row.reference || row.id)}`
                            : tradePath(row.id)
                    }
                    actions={
                        <div className="flex items-center gap-3">
                            <label className="flex items-center gap-2 text-sm">
                                <input
                                    type="checkbox"
                                    checked={flat}
                                    onChange={(event) => {
                                        setFlat(event.target.checked);
                                    }}
                                />
                                {t('trading.blotter.flat')}
                            </label>
                            <Button
                                type="button"
                                variant="secondary"
                                onClick={() => {
                                    void navigate(PATHS.groupStructures);
                                }}
                            >
                                {t('trading.group.open')}
                            </Button>
                            <Button
                                type="button"
                                variant="secondary"
                                onClick={() => {
                                    void navigate(PATHS.newTrade);
                                }}
                            >
                                {t('trading.booking.open')}
                            </Button>
                        </div>
                    }
                />
            </div>
        </div>
    );
}
