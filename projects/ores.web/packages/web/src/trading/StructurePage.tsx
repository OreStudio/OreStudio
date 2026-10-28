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
import {
    structures,
    type StructurePageRecord,
    type StructureTreeNode,
    type StructureVersion,
} from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Field, Notice, Select, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';

/** The page's tabs, in the order the prototype settled: the parts, the price, the record. */
const TABS = ['members', 'price', 'history'] as const;

/**
 * The kinds that confirm as one document.
 *
 * The kind is chosen at grouping, and it decides what the customer receives: a
 * package's legs each confirm on their own, and every other kind confirms as one
 * document. The page states which it is, because a person should not have to
 * know the ladder by heart to read the screen.
 */
const CONFIRMS_AS_ONE = ['Strategy', 'Typed', 'Dynamic'];

/** The tone a kind is drawn in, so the tree reads at a glance. */
function kindTone(kind: string): 'neutral' | 'muted' | 'up' {
    if (kind === 'Package') {
        return 'muted';
    }
    return CONFIRMS_AS_ONE.includes(kind) ? 'neutral' : 'up';
}

/** The path of a structure, by the reference the desk knows it by. */
function structurePath(node: { readonly id: string; readonly reference: string }): string {
    return `${PATHS.structures}/${encodeURIComponent(node.reference === '' ? node.id : node.reference)}`;
}

/** One node of the tree, and the nodes under it. */
function TreeNodeRow({
    node,
    depth,
    current,
}: {
    readonly node: StructureTreeNode;
    readonly depth: number;
    readonly current: string;
}): ReactNode {
    const { t } = useTranslation();
    const isStructure = node.kind !== '' && node.template_code !== '' ? true : node.children.length > 0;
    const isHere = node.id === current;
    return (
        <li>
            <div
                className={`flex items-center gap-2 py-1 ${isHere ? 'font-medium text-ink' : 'text-ink-muted'}`}
                style={{ paddingLeft: `${depth * 16}px` }}
            >
                <Tag tone={kindTone(node.kind)}>{node.kind === '' ? 'trade' : node.kind}</Tag>
                {node.reference === '' ? (
                    <span className="font-mono text-xs">{node.id}</span>
                ) : isStructure ? (
                    <Link
                        className="font-mono text-xs text-accent hover:text-accent-bright"
                        to={structurePath(node)}
                    >
                        {node.reference}
                    </Link>
                ) : (
                    <Link
                        className="font-mono text-xs text-accent hover:text-accent-bright"
                        to={`${PATHS.trades}/${node.id}`}
                    >
                        {node.reference}
                    </Link>
                )}
                {node.role === '' ? null : (
                    <span className="text-xs text-ink-faint">{node.role}</span>
                )}
            </div>
            {node.children.length === 0 ? null : (
                <ul className="m-0 list-none p-0">
                    {node.children.map((child) => (
                        <TreeNodeRow
                            key={child.id}
                            node={child}
                            depth={depth + 1}
                            current={current}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}

/** The whole tree of the root, beside the page. */
function TreePanel({
    record,
}: {
    readonly record: StructurePageRecord;
}): ReactNode {
    const { t } = useTranslation();
    if (record.tree === null) {
        return null;
    }
    return (
        <aside className="h-fit rounded-md border border-line bg-surface-raised p-3">
            <h2 className="mb-2 text-sm font-medium text-ink">{t('trading.structure.tree')}</h2>
            <ul className="m-0 list-none p-0">
                <TreeNodeRow node={record.tree} depth={0} current={record.id} />
            </ul>
            <p className="mt-3 text-xs text-ink-faint">{t('trading.structure.treeNote')}</p>
        </aside>
    );
}

/** The legs of the deal, as the members tab draws them. */
function MembersTab({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [tradeId, setTradeId] = useState('');
    const [role, setRole] = useState(record.roles[0] ?? '');

    const candidates = useQuery({
        queryKey: ['structure', record.id, 'leg-candidates'],
        queryFn: () => structures.legCandidates(record.reference === '' ? record.id : record.reference),
    });
    const roles = record.roles.length > 0 ? record.roles : ['leg'];
    const add = useMutation({
        mutationFn: () =>
            structures.addLeg(record.reference === '' ? record.id : record.reference, tradeId, role),
        onSuccess: async () => {
            setTradeId('');
            // The page is keyed by the reference the address carries, and the
            // picker by the id, so the whole set is invalidated rather than one
            // key this form would have to guess.
            await queries.invalidateQueries({ queryKey: ['structure'] });
        },
    });
    const rows = candidates.data ?? [];

    return (
        <div className="space-y-4">
            <section className="overflow-hidden rounded-md border border-line">
                <h2 className="border-b border-line bg-surface-2 px-3 py-2 text-sm font-medium text-ink">
                    {t('trading.structure.legsCaption')}
                </h2>
                {record.legs.length === 0 ? (
                    <p className="px-3 py-3 text-sm text-ink-muted">
                        {t('trading.structure.noLegs')}
                    </p>
                ) : (
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structure.sequence')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structure.trade')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structure.product')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structure.role')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('trading.structure.externalVersion')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {record.legs.map((leg) => (
                                <tr
                                    key={leg.trade_id}
                                    className="border-b border-line last:border-0"
                                >
                                    <td className="px-3 py-2 text-ink-muted">
                                        {leg.sequence_number}
                                    </td>
                                    <td className="px-3 py-2 font-mono text-ink">
                                        <Link
                                            className="text-accent hover:text-accent-bright"
                                            to={`${PATHS.trades}/${leg.trade_id}`}
                                        >
                                            {leg.reference === ''
                                                ? leg.trade_id
                                                : leg.reference}
                                        </Link>
                                    </td>
                                    <td className="px-3 py-2 text-ink">{leg.kind}</td>
                                    <td className="px-3 py-2 text-ink">{leg.role}</td>
                                    <td className="px-3 py-2 text-ink-muted">
                                        {leg.external_version ?? '—'}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </section>

            <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-medium text-ink">{t('trading.structure.addLeg')}</h2>
                <p className="text-sm text-ink-muted">{t('trading.structure.addLegLead')}</p>
                {candidates.isError && <Notice tone="error">{candidates.error.message}</Notice>}
                {add.isError && <Notice tone="error">{add.error.message}</Notice>}
                {rows.length === 0 && candidates.isSuccess && (
                    <Notice tone="info">{t('trading.structure.noCandidates')}</Notice>
                )}
                <div className="grid gap-3 md:grid-cols-2">
                    <Field label={t('trading.structure.trade')}>
                        <Select
                            value={tradeId}
                            onChange={(event) => setTradeId(event.target.value)}
                        >
                            <option value="">{t('trading.booking.choose')}</option>
                            {rows.map((row) => (
                                <option key={row.id} value={row.id}>
                                    {row.reference === '' ? row.id : row.reference}
                                </option>
                            ))}
                        </Select>
                    </Field>
                    <Field label={t('trading.structure.role')}>
                        <Select value={role} onChange={(event) => setRole(event.target.value)}>
                            {roles.map((value) => (
                                <option key={value} value={value}>
                                    {value}
                                </option>
                            ))}
                        </Select>
                    </Field>
                </div>
                <Button
                    type="button"
                    variant="primary"
                    pending={add.isPending}
                    disabled={tradeId === '' || role === ''}
                    onClick={() => add.mutate()}
                >
                    {t('trading.structure.addLegSubmit')}
                </Button>
            </section>
        </div>
    );
}

/** The versions the structure was written at, newest first. */
function HistoryTab({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    if (record.history.length === 0) {
        return <Notice tone="info">{t('trading.structure.noHistory')}</Notice>;
    }
    return (
        <section className="overflow-hidden rounded-md border border-line">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                        <th className="px-3 py-2 font-medium">
                            {t('trading.structure.version')}
                        </th>
                        <th className="px-3 py-2 font-medium">{t('trading.structure.by')}</th>
                        <th className="px-3 py-2 font-medium">{t('trading.structure.when')}</th>
                        <th className="px-3 py-2 font-medium">
                            {t('trading.structure.reason')}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {record.history.map((version: StructureVersion) => (
                        <tr key={version.version} className="border-b border-line last:border-0">
                            <td className="px-3 py-2 text-ink-muted">{version.version}</td>
                            <td className="px-3 py-2 text-ink">{version.modified_by}</td>
                            <td className="px-3 py-2 text-ink-muted">{version.recorded_at}</td>
                            <td className="px-3 py-2 text-ink">
                                {version.change_commentary === ''
                                    ? version.change_reason_code
                                    : version.change_commentary}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/**
 * The price tab.
 *
 * FIXME: pricing is not read yet. The prototype reads a market snapshot and says
 * when it was read, and the read that would serve it does not exist: an
 * inception P&L report is the work that comes first. The tab states that rather
 * than showing a number nothing produced.
 */
function PriceTab(): ReactNode {
    const { t } = useTranslation();
    return <Notice tone="info">{t('trading.structure.priceNotRead')}</Notice>;
}

/** The frame a structure's page is drawn in, shared by its states. */
function Frame({
    trail,
    title,
    description,
    children,
}: {
    readonly trail: readonly { readonly label: string; readonly to?: string }[];
    readonly title: string;
    readonly description: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail]} />}
            title={title}
            description={description}
        >
            {children}
        </ScreenFrame>
    );
}

/**
 * The page a deal opens on: what it is, the tree it sits in, and its parts.
 *
 * The address carries the reference the desk gave the deal, not its id, because a
 * reference is what a person says. The tree shows the whole tree of the root
 * whichever node is open, which is what lets a person see where the deal they are
 * reading sits.
 */
export function StructurePage(): ReactNode {
    const { t } = useTranslation();
    const { reference = '' } = useParams();
    const open = useQuery({
        queryKey: ['structure', reference],
        queryFn: () => structures.one(reference),
        enabled: reference !== '',
    });
    const tabs = useTabs({
        label: t('trading.structure.tabsLabel'),
        tabs: TABS,
        titleOf: (tab) => t(`trading.structure.tab.${tab}`),
    });
    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.trades.title'), to: PATHS.trades },
    ];

    if (open.isPending) {
        return (
            <Frame
                trail={trail}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </Frame>
        );
    }

    if (open.isError) {
        return (
            <Frame
                trail={[...trail, { label: reference }]}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('trading.structure.notFound')}</Notice>
                    <Link
                        to={PATHS.trades}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('trading.structure.backToList')}
                    </Link>
                </div>
            </Frame>
        );
    }

    const record = open.data;
    const heading = record.reference === '' ? record.id : record.reference;
    const confirms = CONFIRMS_AS_ONE.includes(record.kind)
        ? t('trading.structure.confirmsAsOne')
        : t('trading.structure.confirmsLegByLeg');
    const prices = CONFIRMS_AS_ONE.includes(record.kind)
        ? t('trading.structure.pricesAsWhole')
        : t('trading.structure.pricesLegByLeg');

    return (
        <Frame
            trail={[...trail, { label: heading }]}
            title={heading}
            description={t('trading.structure.lead')}
        >
            <div className="space-y-4">
                <div className="flex flex-wrap items-center gap-2">
                    <Tag tone={kindTone(record.kind)}>{record.kind}</Tag>
                    <Tag tone="muted">{confirms}</Tag>
                    <Tag tone="muted">{prices}</Tag>
                    {record.counterparty_id === '' ? null : (
                        <Link
                            className="text-sm text-accent hover:text-accent-bright"
                            to={`${PATHS.counterparties}/${record.counterparty_id}`}
                        >
                            {record.counterparty_name}
                        </Link>
                    )}
                </div>
                <div className="grid gap-5 lg:grid-cols-[320px_minmax(0,1fr)]">
                    <TreePanel record={record} />
                    <div className="space-y-4">
                        {tabs.bar}
                        {tabs.tab === 'members' && <MembersTab record={record} />}
                        {tabs.tab === 'price' && <PriceTab />}
                        {tabs.tab === 'history' && <HistoryTab record={record} />}
                    </div>
                </div>
            </div>
        </Frame>
    );
}
