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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { trades, type TradePageRecord } from '../api/trades.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';

/** The page's tabs, in the order the prototype settled: the trade first, then its parts. */
const TABS = ['details', 'identifiers', 'links', 'history'] as const;

/** One fact of the trade, as a label and a value. */
function Fact({ label, value }: { readonly label: string; readonly value: string }): ReactNode {
    return (
        <div>
            <dt className="text-xs uppercase tracking-wide text-ink-faint">{label}</dt>
            <dd className="text-sm text-ink">{value === '' ? '—' : value}</dd>
        </div>
    );
}

/** A table of the trade's parts, with the columns each part has. */
function PartTable({
    caption,
    columns,
    rows,
    empty,
}: {
    readonly caption: string;
    readonly columns: readonly string[];
    readonly rows: readonly (readonly string[])[];
    readonly empty: string;
}): ReactNode {
    return (
        <section className="overflow-hidden rounded-md border border-line">
            <h2 className="border-b border-line bg-surface-2 px-3 py-2 text-sm font-medium text-ink">
                {caption}
            </h2>
            {rows.length === 0 ? (
                <p className="px-3 py-3 text-sm text-ink-muted">{empty}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                            {columns.map((column) => (
                                <th key={column} className="px-3 py-2 font-medium">
                                    {column}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows.map((cells, index) => (
                            <tr key={index} className="border-b border-line last:border-0">
                                {cells.map((cell, cellIndex) => (
                                    <td key={cellIndex} className="px-3 py-2 text-ink">
                                        {cell === '' ? '—' : cell}
                                    </td>
                                ))}
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

/** The tone a state is drawn in, so the badge says more than the word does. */
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

/** The tabs of one trade, drawn from the one read the page makes. */
export function TradePage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const open = useQuery({
        queryKey: ['trades', id],
        queryFn: () => trades.one(id),
        enabled: id !== '',
    });
    const tabs = useTabs({
        label: t('trading.page.tabs.label'),
        tabs: TABS,
        titleOf: (tab) => t(`trading.page.tabs.${tab}`),
    });
    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.trades.title'), to: PATHS.trades },
    ];

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('trading.page.title')}
                description={t('trading.page.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: id }]} />}
                title={t('trading.page.title')}
                description={t('trading.page.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('trading.page.notFound')}</Notice>
                    <Link
                        to={PATHS.trades}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('trading.page.backToList')}
                    </Link>
                </div>
            </ScreenFrame>
        );
    }

    const record: TradePageRecord = open.data;
    const trade = record.trade;
    const heading = trade.identifier === '' ? trade.id : trade.identifier;

    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail, { label: heading }]} />}
            title={heading}
            description={t(`trading.states.${trade.status ?? 'live'}`)}
        >
            <div className="space-y-4">
                {tabs.bar}
                {tabs.tab === 'details' && (
                    <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                        <Fact label={t('trading.fields.identifier')} value={trade.identifier} />
                        <Fact
                            label={t('trading.fields.instrumentType')}
                            value={trade.instrument_type}
                        />
                        <div>
                            <dt className="text-xs uppercase tracking-wide text-ink-faint">
                                {t('trading.page.state')}
                            </dt>
                            <dd className="text-sm text-ink">
                                {trade.status === null ? (
                                    '—'
                                ) : (
                                    <Tag tone={stateTone(trade.status)}>
                                        {t(`trading.states.${trade.status}`)}
                                    </Tag>
                                )}
                            </dd>
                        </div>
                        <div>
                            <dt className="text-xs uppercase tracking-wide text-ink-faint">
                                {t('trading.fields.authorisation')}
                            </dt>
                            <dd className="text-sm text-ink">
                                <Tag tone="muted">{trade.authorisation_status}</Tag>
                            </dd>
                        </div>
                        <Fact
                            label={t('trading.fields.tradeDate')}
                            value={trade.trade_date ?? ''}
                        />
                        <Fact
                            label={t('trading.fields.counterparty')}
                            value={trade.counterparty_name}
                        />
                        <Fact label={t('trading.fields.book')} value={trade.book_name} />
                        <Fact
                            label={t('trading.fields.version')}
                            value={trade.version === null ? '' : String(trade.version)}
                        />
                        <Fact
                            label={t('trading.fields.externalVersion')}
                            value={
                                trade.external_version === null
                                    ? ''
                                    : String(trade.external_version)
                            }
                        />
                        <Fact
                            label={t('trading.page.bookingDate')}
                            value={record.booking?.trade_date ?? ''}
                        />
                        <Fact
                            label={t('trading.page.execution')}
                            value={record.booking?.execution_timestamp ?? ''}
                        />
                    </dl>
                )}
                {tabs.tab === 'identifiers' && (
                    <div className="space-y-4">
                        <PartTable
                            caption={t('trading.page.identifiers')}
                            columns={[t('trading.page.idType'), t('trading.page.idValue')]}
                            rows={record.identifiers.map((entry) => [
                                entry.id_type,
                                entry.id_value,
                            ])}
                            empty={t('trading.page.noIdentifiers')}
                        />
                        <PartTable
                            caption={t('trading.page.roles')}
                            columns={[t('trading.page.role'), t('trading.page.party')]}
                            rows={record.roles.map((entry) => [entry.role_type, entry.party_id])}
                            empty={t('trading.page.noRoles')}
                        />
                    </div>
                )}
                {tabs.tab === 'links' && (
                    <div className="space-y-4">
                        <PartTable
                            caption={t('trading.page.linksFrom')}
                            columns={[t('trading.page.linkType'), t('trading.page.otherTrade')]}
                            rows={record.links.from.map((entry) => [
                                entry.link_type,
                                entry.to_trade_id,
                            ])}
                            empty={t('trading.page.noLinks')}
                        />
                        <PartTable
                            caption={t('trading.page.linksTo')}
                            columns={[t('trading.page.linkType'), t('trading.page.otherTrade')]}
                            rows={record.links.to.map((entry) => [
                                entry.link_type,
                                entry.from_trade_id,
                            ])}
                            empty={t('trading.page.noLinks')}
                        />
                    </div>
                )}
                {tabs.tab === 'history' && (
                    <PartTable
                        caption={t('trading.page.history')}
                        columns={[t('trading.fields.version')]}
                        rows={record.history.map((entry) => [String(entry.version)])}
                        empty={t('trading.page.noHistory')}
                    />
                )}
            </div>
        </ScreenFrame>
    );
}
