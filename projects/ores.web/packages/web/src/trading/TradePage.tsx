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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { trades, type TradeLink, type TradePageRecord } from '../api/trades.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Fact, Field, Notice, Select, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';

/** The page's tabs, in the order the prototype settled: the trade first, then its parts. */
const TABS = ['details', 'instrument', 'identifiers', 'links', 'history'] as const;

/** A table of the trade's parts, with the columns each part has. */
function PartTable({
    caption,
    columns,
    rows,
    empty,
}: {
    readonly caption: string;
    readonly columns: readonly string[];
    readonly rows: readonly (readonly ReactNode[])[];
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

/** The value, unless it is a uuid, which is not something to put on a screen. */
function withoutId(value: string): string {
    const looksLikeId = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(
        value,
    );
    return looksLikeId ? '' : value;
}

/**
 * The links at both ends, and the form that records one.
 *
 * A link is a fact about two trades — a hedge, a close-out, a novation — so the
 * type is chosen by the words that describe it and the other trade by the
 * identifier the desk knows it by. The write is the trading service's; this
 * states the request and reads the answer.
 */
function LinksTab({ record }: { readonly record: TradePageRecord }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const id = record.trade.id;
    const types = useQuery({ queryKey: ['trade-link-types'], queryFn: () => trades.linkTypes() });
    // Only a type this tenant can read can be recorded, so the form waits for
    // that answer rather than offering an empty picker or flashing an apology.
    const offered = (types.data ?? []).length > 0;
    const others = useQuery({
        queryKey: ['trades', 'link-picker'],
        queryFn: () =>
            trades.page({
                offset: 0,
                limit: LINK_PICKER_LIMIT,
                search: '',
                sort: 'trade_date',
                descending: true,
                status: '',
                type: '',
            }),
        enabled: offered,
    });
    const [linkType, setLinkType] = useState('');
    const [otherTradeId, setOtherTradeId] = useState('');
    const link = useMutation({
        mutationFn: () => trades.addLink(id, linkType, otherTradeId),
        onSuccess: async () => {
            setOtherTradeId('');
            await queries.invalidateQueries({ queryKey: ['trades', id] });
        },
    });

    const candidates = (others.data?.rows ?? []).filter(
        (row) => row.row_kind === 'trade' && row.id !== id,
    );
    const truncated = (others.data?.total ?? 0) > candidates.length;

    return (
        <div className="space-y-5">
            <LinkTable
                caption={t('trading.page.linksFrom')}
                links={record.links.from}
                otherOf={(entry) => entry.to_trade_id}
                empty={t('trading.page.noLinks')}
            />
            <LinkTable
                caption={t('trading.page.linksTo')}
                links={record.links.to}
                otherOf={(entry) => entry.from_trade_id}
                empty={t('trading.page.noLinks')}
            />
            <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-medium text-ink">{t('trading.page.addLink')}</h2>
                {types.isError && <Notice tone="error">{t('trading.page.linkTypesFailed')}</Notice>}
                {types.isSuccess && !offered && (
                    /*
                     * The link types are system-tenant reference data, which a
                     * tenant session cannot read; the capture says so. A form
                     * with an empty picker would look like a screen that forgot
                     * to load.
                     */
                    <Notice tone="info">{t('trading.page.linkTypesUnreadable')}</Notice>
                )}
                {offered && (
                    <>
                        {link.isError && <Notice tone="error">{link.error.message}</Notice>}
                        <div className="grid gap-3 md:grid-cols-2">
                            <Field label={t('trading.page.linkType')}>
                                <Select
                                    value={linkType}
                                    onChange={(event) => setLinkType(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {(types.data ?? []).map((row) => (
                                        <option key={row.code} value={row.code}>
                                            {row.description === '' ? row.code : row.description}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field
                                label={t('trading.page.otherTrade')}
                                {...(truncated
                                    ? {
                                          hint: t('trading.page.linkPickerTruncated', {
                                              shown: String(candidates.length),
                                              total: String(others.data?.total ?? 0),
                                          }),
                                      }
                                    : {})}
                            >
                                <Select
                                    value={otherTradeId}
                                    onChange={(event) => setOtherTradeId(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {candidates.map((row) => (
                                        <option key={row.id} value={row.id}>
                                            {row.identifier}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                        </div>
                        <Button
                            type="button"
                            variant="primary"
                            pending={link.isPending}
                            disabled={linkType === '' || otherTradeId === ''}
                            onClick={() => link.mutate()}
                        >
                            {t('trading.page.addLinkSubmit')}
                        </Button>
                    </>
                )}
            </section>
        </div>
    );
}

/** How many trades the link picker offers before it says so. */
const LINK_PICKER_LIMIT = 200;

/**
 * One end of the trade's links, whatever the link type.
 *
 * Both ends read the same: the link's own type, then the trade at the other end
 * named by its identifier, or by its id when it has none.
 */
function LinkTable({
    caption,
    links,
    otherOf,
    empty,
}: {
    readonly caption: string;
    readonly links: readonly TradeLink[];
    readonly otherOf: (link: TradeLink) => string;
    readonly empty: string;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <PartTable
            caption={caption}
            columns={[t('trading.page.linkType'), t('trading.page.otherTrade')]}
            rows={links.map((entry) => [
                entry.link_type,
                <Link
                    className="text-accent hover:text-accent-bright"
                    key={otherOf(entry)}
                    to={`${PATHS.trades}/${otherOf(entry)}`}
                >
                    {entry.other_label === '' ? otherOf(entry) : entry.other_label}
                </Link>,
            ])}
            empty={empty}
        />
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

/**
 * The trade's terms, from the table its type is stored in.
 *
 * A type with no table states that plainly: the trade is understood, its terms
 * are simply not held anywhere yet. A type with a table and no row says that
 * instead, because the two are different facts about the trade.
 */
function InstrumentTab({ record }: { readonly record: TradePageRecord }): ReactNode {
    const { t } = useTranslation();
    const terms = record.terms;
    if (record.refused) {
        return <Notice tone="info">{t('trading.page.instrumentRefused')}</Notice>;
    }
    if (!record.routed) {
        return (
            <Notice tone="info">
                {t('trading.page.instrumentNotRouted', { type: record.trade.instrument_type })}
            </Notice>
        );
    }
    if (terms === null) {
        return <Notice tone="info">{t('trading.page.instrumentNoTerms')}</Notice>;
    }
    const issue = terms['issue'] as Readonly<Record<string, unknown>> | null | undefined;
    const value = (given: unknown): string =>
        given === undefined || given === null ? '' : String(given);
    const calendar = value(issue?.['calendar']);
    return (
        <div className="space-y-6">
            <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                <Fact
                    label={t('trading.page.tradeTypeCode')}
                    value={record.trade.instrument_type}
                />
                <Fact label={t('trading.page.notional')} value={value(terms['notional'])} />
            </dl>
            <section className="space-y-3">
                <h2 className="text-sm font-medium text-ink">{t('trading.page.issue')}</h2>
                {issue === null || issue === undefined ? (
                    <Notice tone="info">{t('trading.page.issueMissing')}</Notice>
                ) : (
                    <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                        <Fact
                            label={t('trading.page.securityId')}
                            value={value(issue['security_id'])}
                        />
                        <Fact
                            label={t('trading.page.issuer')}
                            value={withoutId(value(issue['issuer']))}
                        />
                        <Fact
                            label={t('trading.page.faceValue')}
                            value={value(issue['face_value'])}
                        />
                        <Fact
                            label={t('trading.page.issueDate')}
                            value={value(issue['issue_date'])}
                        />
                        <Fact
                            label={t('trading.page.settlementDays')}
                            value={value(issue['settlement_days'])}
                        />
                        <Fact label={t('trading.page.calendar')}>
                            {calendar === '' ? (
                                '—'
                            ) : (
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`${PATHS.calendars}/${encodeURIComponent(calendar)}`}
                                >
                                    {calendar}
                                </Link>
                            )}
                        </Fact>
                        <Fact
                            label={t('trading.page.creditGroup')}
                            value={value(issue['credit_group'])}
                        />
                    </dl>
                )}
            </section>
        </div>
    );
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
                        <Fact label={t('trading.fields.counterparty')}>
                            {trade.counterparty_id === null ? (
                                trade.counterparty_name
                            ) : (
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`${PATHS.counterparties}/${trade.counterparty_id}`}
                                >
                                    {trade.counterparty_name}
                                </Link>
                            )}
                        </Fact>
                        <Fact label={t('trading.fields.book')}>
                            {trade.book_id === null ? (
                                trade.book_name
                            ) : (
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`${PATHS.books}/${trade.book_id}`}
                                >
                                    {trade.book_name}
                                </Link>
                            )}
                        </Fact>
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
                {tabs.tab === 'instrument' && <InstrumentTab record={record} />}
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
                {tabs.tab === 'links' && <LinksTab record={record} />}
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
