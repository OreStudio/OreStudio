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

import { useMemo, type ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { trades, TRADE_SORTS, type TradeRow } from '../api/trades.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { show } from '../refdata/records.js';
import { useAreaParts } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Detail, Tag } from '../ui/Primitives.js';

/** The trade types the filter offers. The read model holds the catalogue's code. */
const TYPES = [
    'Swap',
    'FxForward',
    'FxOption',
    'Bond',
    'BondRepo',
    'CapFloor',
    'Swaption',
    'CompositeTrade',
];

/** The states a trade may be in, as the trade status machine states them. */
const STATES = ['live', 'draft', 'expired', 'cancelled'];

/**
 * The trade list of the blotter.
 *
 * Every filter, the search, the order and the page go to the server, which is
 * the only party that knows the total: the read model answers the page and the
 * BFF names what each row holds. The screen never reads every row to filter it.
 *
 * A row is a trade or a structure. A structure stands for several trades and
 * has no state, date or version of its own, so those cells stay blank and the
 * first column states what the row is instead of a reference it does not have.
 */
export function TradesPage(): ReactNode {
    const { t } = useTranslation();
    const source: ListSource<TradeRow> = {
        key: 'trades',
        read: (page) =>
            trades.page({
                offset: page.offset,
                limit: page.limit,
                search: page.search,
                sort: (TRADE_SORTS as readonly string[]).includes(page.sort) ? page.sort : '',
                descending: page.descending,
                status: page.filters['status'] ?? '',
                type: page.filters['type'] ?? '',
            }),
        search: true,
        sortable: [...TRADE_SORTS],
        filters: [
            {
                kind: 'choice',
                id: 'status',
                label: t('trading.trades.filterStatus'),
                all: t('trading.trades.filterAllStates'),
                choices: STATES.map((state) => ({
                    value: state,
                    label: stateLabel(t, state),
                })),
            },
            {
                kind: 'choice',
                id: 'type',
                label: t('trading.trades.filterType'),
                all: t('trading.trades.filterAllTypes'),
                choices: TYPES.map((type) => ({ value: type, label: type })),
            },
        ],
        mayAdd: true,
    };

    const navigate = useNavigate();
    const [search] = useSearchParams();
    const tradeId = search.get('trade') ?? '';
    const version = Number(search.get('version') ?? '');
    const asRead =
        tradeId !== '' && Number.isInteger(version) && version > 0 ? (
            <TradeAsRead id={tradeId} version={version} />
        ) : null;

    return (
        <>
            {asRead}
            <RecordList
                source={source}
                title={t('trading.trades.title')}
                lead={t('trading.trades.lead')}
                crumbs={useAreaParts('trading', t('trading.trades.title'))}
                keyOf={(row) => row.id}
                addLabel={t('trading.booking.open')}
                onAdd={() => {
                    void navigate(PATHS.newTrade);
                }}
                pathOf={(row) =>
                    row.row_kind === 'structure'
                        ? `${PATHS.structures}/${row.id}`
                        : `${PATHS.trades}/${row.id}`
                }
                columns={[
                    {
                        id: 'identifier',
                        header: t('trading.fields.identifier'),
                        cell: (row) => <IdentifierCell row={row} />,
                        mono: true,
                    },
                    {
                        id: 'instrument_type',
                        header: t('trading.fields.instrumentType'),
                        cell: (row) => show(row.instrument_type),
                        sort: 'instrument_type',
                    },
                    {
                        id: 'counterparty',
                        header: t('trading.fields.counterparty'),
                        cell: (row) =>
                            row.counterparty_id === null ? (
                                show(row.counterparty_name)
                            ) : (
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`${PATHS.counterparties}/${row.counterparty_id}`}
                                >
                                    {show(row.counterparty_name)}
                                </Link>
                            ),
                    },
                    {
                        id: 'book',
                        header: t('trading.fields.book'),
                        cell: (row) =>
                            row.book_id === null ? (
                                show(row.book_name)
                            ) : (
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`${PATHS.books}/${row.book_id}`}
                                >
                                    {show(row.book_name)}
                                </Link>
                            ),
                    },
                    {
                        id: 'status',
                        header: t('trading.fields.state'),
                        cell: (row) =>
                            row.status === null ? (
                                t('trading.trades.structureHolds', { count: row.trade_count })
                            ) : (
                                <Tag tone={stateTone(row.status)}>{stateLabel(t, row.status)}</Tag>
                            ),
                        sort: 'status',
                    },
                    {
                        id: 'authorisation',
                        header: t('trading.fields.authorisation'),
                        cell: (row) => <Tag tone="muted">{row.authorisation_status}</Tag>,
                    },
                    {
                        id: 'trade_date',
                        header: t('trading.fields.tradeDate'),
                        cell: (row) => show(row.trade_date),
                        sort: 'trade_date',
                    },
                    {
                        id: 'version',
                        header: t('trading.fields.version'),
                        cell: (row) => (row.version === null ? '' : String(row.version)),
                        mono: true,
                        numeric: true,
                    },
                    {
                        id: 'external_version',
                        header: t('trading.fields.externalVersion'),
                        cell: (row) =>
                            row.external_version === null ? '' : String(row.external_version),
                        mono: true,
                        numeric: true,
                    },
                ]}
            />
        </>
    );
}

/** The tone a state is drawn in: live runs, a draft waits, and a dead trade is muted. */
function stateTone(state: string): 'up' | 'warn' | 'muted' | 'neutral' {
    switch (state) {
        case 'live':
            return 'up';
        case 'draft':
            return 'warn';
        case 'expired':
        case 'cancelled':
            return 'muted';
        default:
            return 'neutral';
    }
}

/** The words for a state, and the code itself for one this screen does not know. */
function stateLabel(t: (key: string) => string, state: string): string {
    return (STATES as readonly string[]).includes(state) ? t(`trading.states.${state}`) : state;
}

/** The first column: a trade's reference, or the kind of a row that has no trade behind it. */
function IdentifierCell({ row }: { readonly row: TradeRow }): ReactNode {
    const { t } = useTranslation();
    if (row.row_kind === 'structure') {
        // The structure's own reference, when it has one
        // is addressed by the reference it was given, not by its kind.
        return row.identifier === '' ? t('trading.trades.structureRow') : show(row.identifier);
    }
    return show(row.identifier);
}

/** The trade a run read, at the version the run recorded. */
function TradeAsRead({ id, version }: { readonly id: string; readonly version: number }) {
    const { t } = useTranslation();
    const read = useQuery({
        queryKey: ['trade-as-read', id, version],
        queryFn: () => api.tradeVersion(id, version),
        meta: { quiet: true },
    });
    const trade = useMemo(() => {
        const raw = read.data;
        return raw === null || raw === undefined ? null : (raw as Record<string, unknown>);
    }, [read.data]);
    const field = (name: string): string => {
        const value = trade?.[name];
        return typeof value === 'string' ? value : '';
    };

    return (
        <section className="card space-y-2 p-6">
            <div className="flex flex-wrap items-center gap-2">
                <h2 className="text-lg font-medium">{t('trading.trades.asReadTitle')}</h2>
                <Tag tone="accent">{t('trading.trades.asReadVersion', { version })}</Tag>
            </div>
            {read.isPending && (
                <p className="text-sm text-ink-muted">{t('trading.trades.asReadLoading')}</p>
            )}
            {read.isSuccess && trade === null && (
                <p className="text-sm text-ink-muted">
                    {t('trading.trades.asReadMissing', { version })}
                </p>
            )}
            {trade !== null && (
                <>
                    <p className="break-all font-mono text-sm">{field('id') || id}</p>
                    <dl className="grid gap-2 text-sm sm:grid-cols-3">
                        <Detail
                            label={t('trading.fields.instrumentType')}
                            value={field('instrument_type') || '—'}
                        />
                        <Detail label={t('trading.fields.state')} value={field('status') || '—'} />
                        <Detail
                            label={t('trading.fields.tradeDate')}
                            value={field('trade_date') || '—'}
                        />
                    </dl>
                    <p className="text-sm text-ink-muted">{t('trading.trades.asReadNotCurrent')}</p>
                </>
            )}
            <Link to="/trades" className="text-sm underline">
                {t('trading.trades.asReadDismiss')}
            </Link>
        </section>
    );
}
