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
import { trades, TRADE_SORTS, type TradeRow } from '../api/trades.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { show } from '../refdata/records.js';

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
                party: '',
                book: '',
                from: '',
                to: '',
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
                    label: t(`trading.states.${state}`),
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
        mayAdd: false,
    };

    return (
        <RecordList
            source={source}
            title={t('trading.trades.title')}
            lead={t('trading.trades.lead')}
            crumbs={[
                { label: t('shell.menu.home'), to: '/' },
                { label: t('trading.trades.title') },
            ]}
            keyOf={(row) => row.id}
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
                    cell: (row) => show(row.counterparty_name),
                },
                {
                    id: 'book',
                    header: t('trading.fields.book'),
                    cell: (row) => show(row.book_name),
                },
                {
                    id: 'status',
                    header: t('trading.fields.state'),
                    cell: (row) =>
                        row.status === null
                            ? t('trading.trades.structureHolds', { count: row.trade_count })
                            : t(`trading.states.${row.status}`),
                    sort: 'status',
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
                    cell: (row) =>
                        row.version === null
                            ? ''
                            : t('trading.trades.versionPair', {
                                  version: row.version,
                                  external: row.external_version ?? 0,
                              }),
                    mono: true,
                    numeric: true,
                },
            ]}
        />
    );
}

/** The first column: a trade's reference, or the kind of a row that has no trade behind it. */
function IdentifierCell({ row }: { readonly row: TradeRow }): ReactNode {
    const { t } = useTranslation();
    if (row.row_kind === 'structure') {
        return t('trading.trades.structureRow');
    }
    return show(row.identifier);
}
