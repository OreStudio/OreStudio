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
import { Link, useNavigate, useParams } from 'react-router';
import {
    structures,
    type StructureLeg,
    type StructurePageRecord,
    type StructureTreeNode,
} from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { RecordTable, type ListSource } from '../refdata/RecordList.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Fact, Field, Notice, Select, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { structurePathFor } from './StructuresPage.js';

const TABS = ['details', 'members', 'price', 'history'] as const;

function kindTone(kind: string): 'neutral' | 'muted' | 'up' {
    return kind === 'Package' ? 'muted' : 'neutral';
}

function Card({
    title,
    version,
    actions,
    children,
}: {
    readonly title: string;
    readonly version?: string;
    readonly actions?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <header className="flex items-center gap-2 border-b border-line px-4 py-2">
                <h2 className="text-sm font-medium text-ink">{title}</h2>
                {version === undefined ? null : <Tag tone="muted">{version}</Tag>}
                <div className="ml-auto flex items-center gap-2">{actions}</div>
            </header>
            <div className="space-y-4 px-4 py-4">{children}</div>
        </section>
    );
}

function Explained({
    label,
    value,
    prose,
}: {
    readonly label: string;
    readonly value: ReactNode;
    readonly prose?: string;
}): ReactNode {
    return (
        <div className="space-y-1">
            <div className="text-xs uppercase tracking-wide text-ink-faint">{label}</div>
            <div className="text-sm text-ink">{value}</div>
            {prose === undefined ? null : <p className="text-xs text-ink-faint">{prose}</p>}
        </div>
    );
}

function TreeNodeRow({
    node,
    depth,
    current,
}: {
    readonly node: StructureTreeNode;
    readonly depth: number;
    readonly current: string;
}): ReactNode {
    const isStructure = node.role === 'structure' || node.children.length > 0;
    const isHere = node.id === current;
    return (
        <li>
            <div
                className={`flex items-start gap-2 py-1 ${isHere ? 'text-ink' : 'text-ink-muted'}`}
                style={{ paddingLeft: `${depth * 14}px` }}
            >
                <Tag tone={kindTone(node.kind)}>{node.kind === '' ? 'trade' : node.kind}</Tag>
                <div className="min-w-0 flex-1">
                    {node.product === '' ? null : (
                        <div className="truncate text-sm text-ink" title={node.product}>
                            {node.product}
                        </div>
                    )}
                    <div className="font-mono text-xs">
                        {isStructure ? (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={structurePathFor(node)}
                            >
                                {node.reference === '' ? node.id : node.reference}
                            </Link>
                        ) : (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.trades}/${node.id}`}
                            >
                                {node.reference === '' ? node.id : node.reference}
                            </Link>
                        )}
                        {node.role === '' ? null : <span> · {node.role}</span>}
                    </div>
                </div>
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

function TreePanel({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    if (record.tree === null) {
        return null;
    }
    const inner = record.tree.children.filter(
        (child) => child.role === 'structure' || child.children.length > 0,
    ).length;
    return (
        <aside className="h-fit min-w-0 rounded-md border border-line bg-surface-raised">
            <header className="flex items-baseline gap-2 border-b border-line px-3 py-2">
                <h2 className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('trading.structure.tree')}
                </h2>
                <span className="ml-auto text-xs text-ink-faint">
                    {t('trading.structure.treeMeta', {
                        inner: String(inner),
                        root: record.tree.reference,
                    })}
                </span>
            </header>
            <ul className="m-0 list-none p-3">
                <TreeNodeRow node={record.tree} depth={0} current={record.id} />
            </ul>
            <p className="border-t border-line px-3 py-2 text-xs text-ink-faint">
                {t('trading.structure.treeNote')}
            </p>
        </aside>
    );
}

function IdentityCard({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    return (
        <Card title={t('trading.structure.identity')} version="v1">
            <div className="grid gap-4 md:grid-cols-2">
                <Explained
                    label={t('trading.structure.party')}
                    value={
                        record.party.id === '' ? (
                            '—'
                        ) : (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.parties}/${record.party.id}`}
                            >
                                {record.party.name === '' ? record.party.id : record.party.name}
                            </Link>
                        )
                    }
                    prose={
                        record.party.shared
                            ? t('trading.structure.partyShared')
                            : t('trading.structure.partyMixed')
                    }
                />
                <Explained
                    label={t('trading.structure.counterpartyLabel')}
                    value={
                        record.counterparty_id === '' ? (
                            '—'
                        ) : (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.counterparties}/${record.counterparty_id}`}
                            >
                                {record.counterparty_name}
                            </Link>
                        )
                    }
                    prose={t('trading.structure.counterpartyShared')}
                />
                <Explained
                    label={t('trading.structure.kind')}
                    value={<Tag tone={kindTone(record.kind)}>{record.kind}</Tag>}
                    prose={
                        record.kind === 'Package'
                            ? t('trading.structure.kindProsePackage')
                            : t('trading.structure.kindProseOther')
                    }
                />
                <Explained
                    label={t('trading.structure.inside')}
                    value={record.parent_id === '' ? '—' : record.parent_id}
                    prose={
                        record.parent_id === ''
                            ? t('trading.structure.insideRoot')
                            : t('trading.structure.insideChild')
                    }
                />
            </div>
        </Card>
    );
}

function VerdictCard({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const asOne = record.kind !== 'Package';
    return (
        <Card title={t('trading.structure.verdicts')} version="v1">
            <div className="grid gap-4 md:grid-cols-2">
                <Explained
                    label={t('trading.structure.confirmation')}
                    value={record.confirms}
                    prose={
                        asOne
                            ? t('trading.structure.confirmsProseOne')
                            : t('trading.structure.confirmsProseLegs')
                    }
                />
                <Explained
                    label={t('trading.structure.pricing')}
                    value={record.prices}
                    prose={
                        asOne
                            ? t('trading.structure.pricesProseOne')
                            : t('trading.structure.pricesProseLegs')
                    }
                />
                <Explained
                    label={t('trading.structure.externalVersion')}
                    value={
                        asOne
                            ? t('trading.structure.ofTheDeal')
                            : t('trading.structure.ofEachTrade')
                    }
                    prose={
                        asOne
                            ? t('trading.structure.externalProseOne')
                            : t('trading.structure.externalProseLegs')
                    }
                />
            </div>
        </Card>
    );
}

/**
 * The legs of the deal.
 *
 * The table is the application's own `RecordTable`, which every embedded list of
 * records uses: its columns, its empty state and its keyboard handling are the
 * ones a person already knows. Its source reads the legs the page already holds,
 * because a deal's legs arrive with the deal: there is no page to ask a server
 * for.
 */
function LegsTable({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const key = record.reference === '' ? record.id : record.reference;
    const unlink = useMutation({
        mutationFn: (tradeId: string) => structures.unlink(key, tradeId),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['structure'] });
            await queries.invalidateQueries({ queryKey: ['structures'] });
        },
    });

    const source: ListSource<StructureLeg> = {
        key: `structure-legs-${record.id}`,
        read: async (page) => {
            const needle = page.search.toLowerCase();
            const matched = record.legs.filter(
                (leg) =>
                    needle === '' ||
                    [leg.reference, leg.product, leg.kind, leg.state, leg.role]
                        .join(' ')
                        .toLowerCase()
                        .includes(needle),
            );
            const ordered = [...matched].sort((left, right) =>
                page.descending
                    ? right.sequence_number - left.sequence_number
                    : left.sequence_number - right.sequence_number,
            );
            return {
                rows: ordered.slice(page.offset, page.offset + page.limit),
                total: ordered.length,
            };
        },
        search: true,
        sortable: ['sequence_number'],
        mayAdd: false,
    };

    return (
        <div className="space-y-2">
            {unlink.isError && <Notice tone="error">{unlink.error.message}</Notice>}
            <RecordTable
                source={source}
                plural={t('trading.structure.legs')}
                keyOf={(leg) => leg.trade_id}
                columns={[
                    {
                        id: 'leg',
                        header: t('trading.structure.leg'),
                        cell: (leg) => String(leg.sequence_number),
                    },
                    {
                        id: 'trade',
                        header: t('trading.structure.trade'),
                        mono: true,
                        cell: (leg) => (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.trades}/${leg.trade_id}`}
                            >
                                {leg.reference === '' ? leg.trade_id : leg.reference}
                            </Link>
                        ),
                    },
                    {
                        id: 'type',
                        header: t('trading.structure.type'),
                        cell: (leg) => (leg.product === '' ? leg.kind : leg.product),
                    },
                    {
                        id: 'state',
                        header: t('trading.structure.state'),
                        cell: (leg) =>
                            leg.state === '' ? '—' : <Tag tone="up">{leg.state}</Tag>,
                    },
                    {
                        id: 'external_version',
                        header: t('trading.structure.externalVersion'),
                        cell: (leg) =>
                            leg.external_version === null ? '—' : `ext ${leg.external_version}`,
                    },
                    {
                        id: 'unlink',
                        header: '',
                        cell: (leg) => (
                            <Button
                                type="button"
                                variant="secondary"
                                pending={unlink.isPending}
                                onClick={() => unlink.mutate(leg.trade_id)}
                            >
                                {t('trading.structure.unlink')}
                            </Button>
                        ),
                    },
                ]}
            />
        </div>
    );
}


function DetailsTab({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const asOne = record.kind !== 'Package';
    return (
        <div className="space-y-4">
            <IdentityCard record={record} />
            <VerdictCard record={record} />
            <Card
                title={t('trading.structure.trades')}
                version={t('trading.structure.legCount', { count: String(record.legs.length) })}
                actions={
                    <Button
                        type="button"
                        variant="secondary"
                        onClick={() => {
                            void navigate(PATHS.newTrade);
                        }}
                    >
                        {t('trading.structure.bookLeg')}
                    </Button>
                }
            >
                <p className="text-xs text-ink-muted">
                    {asOne
                        ? t('trading.structure.tradesProseOne')
                        : t('trading.structure.tradesProseLegs')}
                </p>
                <LegsTable record={record} />
            </Card>
        </div>
    );
}

function MembersTab({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [tradeId, setTradeId] = useState('');
    const [role, setRole] = useState(record.roles[0] ?? '');
    const key = record.reference === '' ? record.id : record.reference;

    const candidates = useQuery({
        queryKey: ['structure', record.id, 'leg-candidates'],
        queryFn: () => structures.legCandidates(key),
    });
    const roles = record.roles.length > 0 ? record.roles : ['leg'];
    const add = useMutation({
        mutationFn: () => structures.addLeg(key, tradeId, role),
        onSuccess: async () => {
            setTradeId('');
            await queries.invalidateQueries({ queryKey: ['structure'] });
            await queries.invalidateQueries({ queryKey: ['structures'] });
        },
    });

    const rows = candidates.data ?? [];
    return (
        <div className="space-y-4">
            <LegsTable record={record} />
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
                        <th className="px-3 py-2 font-medium">{t('trading.structure.reason')}</th>
                    </tr>
                </thead>
                <tbody>
                    {record.history.map((version) => (
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
 * when it was read, and the read that would serve it does not exist: an inception
 * P&L report is the work that comes first.
 */
function PriceTab(): ReactNode {
    const { t } = useTranslation();
    return <Notice tone="info">{t('trading.structure.priceNotRead')}</Notice>;
}

/**
 * The page a deal opens on: what it is, the tree it sits in, and its parts.
 *
 * The title is the kind, which is what the customer receives, and the reference
 * sits under it with the kind's chip: a person reading the screen knows what they
 * are looking at before they read its name.
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
        { label: t('trading.structures.title'), to: PATHS.structures },
    ];

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
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
                trail={<Crumbs parts={[...trail, { label: reference }]} />}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('trading.structure.notFound')}</Notice>
                    <Link
                        to={PATHS.structures}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('trading.structure.backToList')}
                    </Link>
                </div>
            </ScreenFrame>
        );
    }

    const record = open.data;
    const heading = record.kind === '' ? t('trading.structure.title') : record.kind;
    const subheading = record.reference === '' ? record.id : record.reference;

    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail, { label: subheading }]} />}
            title={heading}
            description={
                record.template_code === ''
                    ? t('trading.structure.lead')
                    : t('trading.structure.withTemplate', { template: record.template_code })
            }
        >
            <div className="space-y-4">
                <div className="flex flex-wrap items-center gap-2">
                    <span className="font-mono text-sm text-ink-muted">{subheading}</span>
                    <Tag tone={kindTone(record.kind)}>{record.kind}</Tag>
                </div>
                <div className="grid gap-5 xl:grid-cols-[360px_minmax(0,1fr)]">
                    <TreePanel record={record} />
                    <div className="space-y-4">
                        {tabs.bar}
                        {tabs.tab === 'details' && <DetailsTab record={record} />}
                        {tabs.tab === 'members' && <MembersTab record={record} />}
                        {tabs.tab === 'price' && <PriceTab />}
                        {tabs.tab === 'history' && <HistoryTab record={record} />}
                    </div>
                </div>
            </div>
        </ScreenFrame>
    );
}
