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

import { useQueries, useQuery, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import {
    fieldValue,
    type ReportingTreeNode,
    type Timeline as Stream,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { personPath } from '../access/PeoplePage.js';
import { ReportingLineDialog } from '../access/PersonForms.js';
import { useHolds } from '../access/holds.js';
import { roleLabel } from '../access/words.js';
import { Crumbs } from '../refdata/shared.js';
import { Timeline } from '../timeline/Timeline.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Detail, Field, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { OrgChart } from './OrgChart.js';
import { PersonSearch } from './PersonSearch.js';

/** The role that makes a person their tenant's administrator. */
const TENANT_ADMIN = 'TenantAdmin';

/** The entity whose versions carry a person's reporting line. */
const ACCOUNT_ENTITY = 'ores.iam.account';

/**
 * A person's story narrowed to their reporting line.
 *
 * The line is a field of the account, so its history is the account's
 * versions. Each version keeps only the line, with the manager named rather
 * than identified, and the timeline hides the versions where it did not move.
 * Every version stays in the stream so each is read against the one before it,
 * which is how the timeline finds a change.
 */
export function lineTimeline(
    source: Stream | undefined,
    fieldName: string,
    nameOf: (accountId: string | null) => string,
): Stream {
    const events = (source?.events ?? [])
        .filter((event) => event.entityType === ACCOUNT_ENTITY)
        .map((event) => {
            const raw = fieldValue(event.fields, 'Reports To Account ID');
            return {
                ...event,
                fields: [{ name: fieldName, value: nameOf(raw === '' ? null : raw) }],
            };
        });
    return { subject: 'person', id: source?.id ?? '', events, gaps: [] };
}

/**
 * Hierarchy: who reports to whom, as a tree, as an org chart, and as history.
 *
 * It opens on the signed-in person, because that is the place a reader starts
 * from. The tree and the chart are one read drawn two ways, and either selects
 * a person; the card beside them is that person, and the history tab is the
 * standard timeline of their line. The line is changed in a dialog with the
 * reason it states, like any other record, and the manager picker cannot offer
 * anybody who reports to the person, so the screen cannot ask for a ring.
 */
export function ReportingLinesPage({ me }: { readonly me: string }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const holds = useHolds();
    const mayChange = holds('iam::accounts:update');
    const mayReadRoles = holds('iam::roles:read');
    const [selectedId, setSelectedId] = useState('');
    const [collapsed, setCollapsed] = useState<ReadonlySet<string>>(new Set());
    const [editing, setEditing] = useState(false);
    const { tab, bar } = useTabs({
        label: t('access.hub.hierarchy'),
        tabs: ['tree', 'chart', 'history'],
        titleOf: (part) => t(`membership.reporting.tabs.${part}`),
    });

    const tree = useQuery({ queryKey: ['reporting-tree'], queryFn: () => api.reportingTree() });
    const reasons = useQuery({ queryKey: ['amend-reasons'], queryFn: api.amendReasons });

    const nodes = tree.data?.nodes ?? [];
    const children = useMemo(() => {
        const byManager = new Map<string, ReportingTreeNode[]>();
        for (const node of nodes) {
            const managerId = node.reportsToAccountId;
            if (managerId === null) {
                continue;
            }
            const siblings = byManager.get(managerId) ?? [];
            siblings.push(node);
            byManager.set(managerId, siblings);
        }
        return byManager;
    }, [nodes]);

    const mine = nodes.find((node) => node.username === me);
    const selected = nodes.find((node) => node.accountId === selectedId) ?? mine;

    /*
     * The tenant administrator is a role, and a role is read one account at a
     * time. The tree is read for everyone, so the roles are read for the places
     * an administrator is looked for: the roots, the person selected and the
     * signed-in person. Anyone who may not read roles sees no mark rather than
     * a refusal.
     */
    const lookedAt = useMemo(() => {
        const ids = new Set<string>(
            nodes.filter((node) => node.reportsToAccountId === null).map((node) => node.accountId),
        );
        if (selected !== undefined) ids.add(selected.accountId);
        if (mine !== undefined) ids.add(mine.accountId);
        return [...ids];
    }, [nodes, selected, mine]);
    const access = useQueries({
        queries: lookedAt.map((accountId) => ({
            queryKey: ['account-access', accountId],
            queryFn: () => api.accountAccess(accountId),
            enabled: mayReadRoles,
            retry: false,
        })),
    });
    const admins = new Set<string>(
        lookedAt.filter((_id, index) =>
            access[index]?.data?.roles.some((role) => role.name === TENANT_ADMIN),
        ),
    );

    /*
     * The tree states the shape and the manager, and the account read states
     * the version the write must state back, so the write is refused as a
     * conflict when somebody else has changed the row since this card read it.
     */
    const account = useQuery({
        queryKey: ['account', selected?.username ?? ''],
        queryFn: () => api.account(selected?.username ?? ''),
        enabled: selected !== undefined,
        retry: false,
    });
    const story = useQuery({
        queryKey: ['timeline', 'person', selected?.username ?? ''],
        queryFn: () => api.timeline('person', selected?.username ?? ''),
        enabled: selected !== undefined,
        retry: false,
    });

    const refresh = (): void => {
        void queries.invalidateQueries({ queryKey: ['reporting-tree'] });
        void queries.invalidateQueries({ queryKey: ['account'] });
        void queries.invalidateQueries({ queryKey: ['account-access'] });
        void queries.invalidateQueries({ queryKey: ['timeline', 'person'] });
    };

    if (tree.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (tree.isError) {
        return <Notice tone="error">{tree.error.message}</Notice>;
    }

    const roots = nodes.filter((node) => node.reportsToAccountId === null);
    const unrooted = nodes.filter((node) => node.depth < 0);
    const nameOf = (accountId: string | null): string => {
        if (accountId === null) return t('membership.reporting.noManager');
        const node = nodes.find((candidate) => candidate.accountId === accountId);
        return node === undefined || node.fullName === '' ? accountId : node.fullName;
    };
    const card = (
        <PersonCard
            selected={selected}
            nodes={nodes}
            isMe={selected?.accountId === mine?.accountId}
            isAdmin={selected !== undefined && admins.has(selected.accountId)}
            canEdit={mayChange && account.data != null}
            onEdit={() => setEditing(true)}
        />
    );

    return (
        <div className="space-y-6">
            <div>
                <Crumbs
                    parts={[
                        { label: t('shell.menu.home'), to: '/' },
                        { label: t('access.hub.title'), to: '/organisation' },
                        { label: t('access.hub.hierarchy') },
                    ]}
                />
                <PageHeader
                    title={t('access.hub.hierarchy')}
                    description={t('membership.reporting.lead')}
                    actions={<RefreshButton onClick={refresh} pending={tree.isFetching} />}
                />
            </div>

            {bar}

            {tab === 'tree' && (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,1.3fr)_minmax(20rem,1fr)]">
                    <section className="card">
                        <div className="flex items-center justify-between gap-3 border-b border-line px-4 py-3">
                            <h2 className="text-sm font-semibold">
                                {t('membership.reporting.tree')}
                            </h2>
                            <div className="flex items-center gap-2">
                                <Tag tone="muted">
                                    {t('membership.reporting.people', { count: nodes.length })}
                                </Tag>
                                <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={() => setCollapsed(new Set())}
                                >
                                    {t('membership.reporting.expandAll')}
                                </Button>
                                <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={() =>
                                        setCollapsed(new Set(nodes.map((n) => n.accountId)))
                                    }
                                >
                                    {t('membership.reporting.collapseAll')}
                                </Button>
                            </div>
                        </div>
                        <ul className="max-h-[36rem] overflow-auto p-3">
                            {roots.map((node) => (
                                <TreeNode
                                    key={node.accountId}
                                    node={node}
                                    children={children}
                                    collapsed={collapsed}
                                    selectedId={selected?.accountId ?? ''}
                                    meId={mine?.accountId ?? ''}
                                    admins={admins}
                                    onSelect={setSelectedId}
                                    onToggle={(id) =>
                                        setCollapsed((previous) => {
                                            const next = new Set(previous);
                                            if (next.has(id)) {
                                                next.delete(id);
                                            } else {
                                                next.add(id);
                                            }
                                            return next;
                                        })
                                    }
                                />
                            ))}
                        </ul>
                    </section>
                    {card}
                </div>
            )}

            {tab === 'chart' && (
                <section className="card p-4">
                    <OrgChart
                        roots={roots}
                        children={children}
                        meId={mine?.accountId ?? ''}
                        admins={admins}
                    />
                </section>
            )}

            {tab === 'history' && (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,1.3fr)_minmax(20rem,1fr)]">
                    <section className="space-y-4">
                        <Field label={t('membership.reporting.person')}>
                            <PersonSearch
                                nodes={nodes}
                                value={selected?.accountId ?? ''}
                                onChange={setSelectedId}
                            />
                        </Field>
                        {story.isPending && selected !== undefined && (
                            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                        )}
                        {story.isError && <Notice tone="error">{story.error.message}</Notice>}
                        {story.data !== undefined && (
                            <Timeline
                                hideUnchanged
                                timeline={lineTimeline(
                                    story.data,
                                    t('membership.reporting.reportsTo'),
                                    nameOf,
                                )}
                            />
                        )}
                    </section>
                    {card}
                </div>
            )}

            {unrooted.length > 0 && (
                <Notice tone="warn">
                    {t('membership.reporting.unrooted', { count: unrooted.length })}
                </Notice>
            )}

            {editing && account.data != null && (
                <ReportingLineDialog
                    account={account.data}
                    reasons={reasons.data ?? []}
                    onClose={() => setEditing(false)}
                    onSaved={async () => {
                        refresh();
                    }}
                />
            )}
        </div>
    );
}

/** The selected person: who they are, who they report to, and the way to change it. */
function PersonCard({
    selected,
    nodes,
    isMe,
    isAdmin,
    canEdit,
    onEdit,
}: {
    readonly selected: ReportingTreeNode | undefined;
    readonly nodes: readonly ReportingTreeNode[];
    readonly isMe: boolean;
    readonly isAdmin: boolean;
    readonly canEdit: boolean;
    readonly onEdit: () => void;
}): ReactNode {
    const { t } = useTranslation();
    if (selected === undefined) {
        return (
            <section className="card p-5">
                <p className="text-sm text-ink-muted">{t('membership.reporting.pickSomeone')}</p>
            </section>
        );
    }
    const manager =
        selected.reportsToAccountId === null
            ? undefined
            : nodes.find((node) => node.accountId === selected.reportsToAccountId);
    const reports = nodes
        .filter((node) => node.reportsToAccountId === selected.accountId)
        .sort((a, b) => a.fullName.localeCompare(b.fullName));
    return (
        <section className="card space-y-4 p-5">
            <div className="flex items-start justify-between gap-3">
                <div className="flex items-center gap-4">
                    <AccountPicture
                        username={selected.username}
                        name={selected.fullName}
                        size="lg"
                    />
                    <div className="min-w-0">
                        <h2 className="text-base font-semibold">
                            <Link
                                to={personPath(selected.username)}
                                className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
                            >
                                {selected.fullName}
                            </Link>
                        </h2>
                        <p className="text-sm text-ink-muted">{selected.jobTitle}</p>
                        <Badges isMe={isMe} isAdmin={isAdmin} />
                    </div>
                </div>
                {canEdit && !isMe && (
                    <Button icon="edit" onClick={onEdit}>
                        {t('refdata.records.edit')}
                    </Button>
                )}
            </div>

            <dl className="grid grid-cols-2 gap-4">
                <div className="min-w-0">
                    <dt className="text-[11px] tracking-wide text-ink-faint uppercase">
                        {t('membership.reporting.reportsTo')}
                    </dt>
                    <dd className="mt-1 text-sm">
                        {manager === undefined ? (
                            t('membership.reporting.noManager')
                        ) : (
                            <Link
                                to={personPath(manager.username)}
                                className="flex items-center gap-2"
                            >
                                <AccountPicture
                                    username={manager.username}
                                    name={manager.fullName}
                                    size="sm"
                                />
                                <span className="underline decoration-line-strong underline-offset-2 hover:decoration-accent">
                                    {manager.fullName}
                                </span>
                            </Link>
                        )}
                    </dd>
                </div>
                <Detail
                    label={t('membership.reporting.depth')}
                    value={
                        selected.depth < 0
                            ? t('membership.reporting.reachesNoRoot')
                            : String(selected.depth)
                    }
                />
            </dl>

            <section className="space-y-2 border-t border-line pt-4">
                <h3 className="text-sm font-semibold">
                    {t('membership.reporting.directReports')}{' '}
                    <span className="font-normal text-ink-faint">({reports.length})</span>
                </h3>
                {reports.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('membership.reporting.noReports')}</p>
                ) : (
                    <ul className="space-y-1">
                        {reports.map((report) => (
                            <li key={report.accountId}>
                                <Link
                                    to={personPath(report.username)}
                                    className="flex items-center gap-2 rounded px-1 py-0.5 text-sm hover:bg-surface-overlay"
                                >
                                    <AccountPicture
                                        username={report.username}
                                        name={report.fullName}
                                        size="sm"
                                    />
                                    <span className="font-medium">{report.fullName}</span>
                                    <span className="text-xs text-ink-muted">
                                        {report.jobTitle}
                                    </span>
                                </Link>
                            </li>
                        ))}
                    </ul>
                )}
            </section>
        </section>
    );
}

/** What marks a person out: that they are the signed-in person, or their tenant's administrator. */
function Badges({
    isMe,
    isAdmin,
}: {
    readonly isMe: boolean;
    readonly isAdmin: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (!isMe && !isAdmin) return null;
    return (
        <span className="mt-1 flex flex-wrap gap-1">
            {isMe && <Tag tone="accent">{t('membership.reporting.you')}</Tag>}
            {isAdmin && <Tag tone="warn">{roleLabel(t, TENANT_ADMIN)}</Tag>}
        </span>
    );
}

/** One person and the branch under them. */
function TreeNode({
    node,
    children,
    collapsed,
    selectedId,
    meId,
    admins,
    onSelect,
    onToggle,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly collapsed: ReadonlySet<string>;
    readonly selectedId: string;
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
    readonly onSelect: (id: string) => void;
    readonly onToggle: (id: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const kids = children.get(node.accountId) ?? [];
    const open = !collapsed.has(node.accountId);
    const isMe = node.accountId === meId;
    return (
        <li>
            <div
                className={`flex items-center gap-1 rounded px-1 py-0.5 ${
                    selectedId === node.accountId ? 'bg-accent/10' : ''
                } ${isMe ? 'border-l-2 border-accent' : 'border-l-2 border-transparent'}`}
            >
                {kids.length > 0 ? (
                    <button
                        type="button"
                        className="w-4 shrink-0 text-xs text-ink-faint"
                        aria-label={
                            open
                                ? t('membership.reporting.collapse')
                                : t('membership.reporting.expand')
                        }
                        onClick={() => onToggle(node.accountId)}
                    >
                        {open ? '▾' : '▸'}
                    </button>
                ) : (
                    <span className="w-4 shrink-0" />
                )}
                <button
                    type="button"
                    className="flex min-w-0 flex-1 items-center gap-2 rounded px-1 py-0.5 text-left hover:bg-surface-overlay"
                    onClick={() => onSelect(node.accountId)}
                >
                    <AccountPicture username={node.username} name={node.fullName} size="sm" />
                    <span className="min-w-0">
                        <span className={`text-sm ${isMe ? 'font-semibold' : 'font-medium'}`}>
                            {node.fullName}
                        </span>{' '}
                        <span className="text-xs text-ink-muted">{node.jobTitle}</span>
                        <span className="ml-2 text-[11px] text-ink-faint">
                            {t('membership.reporting.reports', { count: node.directReports })}
                        </span>
                    </span>
                    <Badges isMe={isMe} isAdmin={admins.has(node.accountId)} />
                </button>
            </div>
            {open && kids.length > 0 && (
                <ul className="ml-4 border-l border-line-subtle pl-2">
                    {kids.map((child) => (
                        <TreeNode
                            key={child.accountId}
                            node={child}
                            children={children}
                            collapsed={collapsed}
                            selectedId={selectedId}
                            meId={meId}
                            admins={admins}
                            onSelect={onSelect}
                            onToggle={onToggle}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}
