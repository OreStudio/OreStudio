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
import {
    fieldValue,
    type ReportingTreeNode,
    type ReportingTreeParty,
    type Timeline as Stream,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { ReportingLineDialog } from '../access/PersonForms.js';
import { useHolds } from '../access/holds.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useActorPictures } from '../access/PersonRef.js';
import { Crumbs } from '../refdata/shared.js';
import { Timeline } from '../timeline/Timeline.js';
import { Button, Detail, Field, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { TREE_WATCHES } from './treeWatches.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { Badges, NameLink, NodeAvatar, PartyLine, TENANT_ADMIN, nameOf } from './NodeParts.js';
import { OrgChart } from './OrgChart.js';
import {
    inParty,
    partyForest,
    partyLabel,
    shapeOf,
    type PartyBranch,
    type Shape,
} from './organisation.js';
import { PersonSearch } from './PersonSearch.js';
import { RecentLineChanges } from './RecentLineChanges.js';

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
    nameFor: (accountId: string | null) => string,
): Stream {
    const events = (source?.events ?? [])
        .filter((event) => event.entityType === ACCOUNT_ENTITY)
        .map((event) => {
            const raw = fieldValue(event.fields, 'Reports To Account ID');
            return {
                ...event,
                fields: [{ name: fieldName, value: nameFor(raw === '' ? null : raw) }],
            };
        });
    return { subject: 'person', id: source?.id ?? '', events, gaps: [] };
}

/**
 * Hierarchy: who reports to whom, as a tree, as an org chart, and as history.
 *
 * The read answers what the reader may see: the whole tenant for an
 * administrator, and for anybody else the people who work in the parties they
 * work in and everyone who reports to them. It opens on the signed-in person.
 * It is drawn by party, the parties nested by their own parent. Each party
 * holds the reporting tree of the people who work in it, so a person who works
 * in two parties appears in both, under their manager each time. A tenant with
 * no parties is drawn as one reporting tree. A party filter narrows it to one
 * party when the reader sees several.
 *
 * A person is opened only by a reader who may read accounts, and the history is
 * theirs too, because both are account reads. Changing a line is a dialog with
 * the reason it states, and the manager picker cannot offer anybody who reports
 * to the person, so the screen cannot ask for a ring.
 */
export function ReportingLinesPage({ me }: { readonly me: string }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const holds = useHolds();
    const mayReadAccounts = holds('iam::accounts:read');
    const actorPicture = useActorPictures();
    const mayChange = holds('iam::accounts:update');
    const mayReadRoles = holds('iam::roles:read');
    const [selectedId, setSelectedId] = useState('');
    const [collapsed, setCollapsed] = useState<ReadonlySet<string>>(new Set());
    const [editing, setEditing] = useState(false);
    const [party, setParty] = useState('');
    const { tab, bar } = useTabs({
        label: t('access.hub.hierarchy'),
        tabs: mayReadAccounts ? ['tree', 'chart', 'history'] : ['tree', 'chart'],
        titleOf: (part) => t(`membership.reporting.tabs.${part}`),
    });

    const tree = useQuery({ queryKey: ['reporting-tree'], queryFn: () => api.reportingTree() });
    const news = useEntityChanges(TREE_WATCHES, tree);
    const reasons = useQuery({
        queryKey: ['amend-reasons'],
        queryFn: api.amendReasons,
        enabled: mayChange,
    });

    const nodes = tree.data?.nodes ?? [];
    const parties = tree.data?.parties ?? [];
    const partyById = useMemo(
        () => new Map<string, ReportingTreeParty>(parties.map((entry) => [entry.partyId, entry])),
        [parties],
    );
    const visible = useMemo(() => inParty(nodes, party), [nodes, party]);
    const shape = useMemo(() => shapeOf(visible), [visible]);
    const forest = useMemo(() => partyForest(parties, visible, party), [parties, visible, party]);

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
     * Only somebody who may change a line needs it.
     */
    const account = useQuery({
        queryKey: ['account', selected?.username ?? ''],
        queryFn: () => api.account(selected?.username ?? ''),
        enabled: selected !== undefined && mayChange,
        retry: false,
        meta: { quiet: true },
    });
    const story = useQuery({
        queryKey: ['timeline', 'person', selected?.username ?? ''],
        queryFn: () => api.timeline('person', selected?.username ?? ''),
        enabled: selected !== undefined && mayReadAccounts && tab === 'history',
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

    const unrooted = nodes.filter((node) => node.depth < 0);
    const nameFor = (accountId: string | null): string => {
        if (accountId === null) return t('membership.reporting.noManager');
        const node = nodes.find((candidate) => candidate.accountId === accountId);
        return node === undefined ? accountId : nameOf(node);
    };
    const marks = {
        meId: mine?.accountId ?? '',
        admins,
        parties: partyById,
        openable: mayReadAccounts,
    };
    const list: TreeList = {
        collapsed,
        selectedId: selected?.accountId ?? '',
        marks,
        onSelect: setSelectedId,
        onToggle: (id) =>
            setCollapsed((previous) => {
                const next = new Set(previous);
                if (next.has(id)) {
                    next.delete(id);
                } else {
                    next.add(id);
                }
                return next;
            }),
    };
    const card = (
        <PersonCard
            selected={selected}
            nodes={nodes}
            marks={marks}
            canEdit={mayChange && account.data != null}
            onEdit={() => setEditing(true)}
        />
    );
    const filterBar = (
        <div className="flex flex-wrap items-center gap-3">
            <Select
                className="w-60"
                value={party}
                onChange={(event) => setParty(event.target.value)}
                aria-label={t('membership.reporting.party')}
            >
                <option value="">{t('membership.reporting.allParties')}</option>
                {[...parties]
                    .sort((a, b) => partyLabel(a).localeCompare(partyLabel(b)))
                    .map((entry) => (
                        <option key={entry.partyId} value={entry.partyId}>
                            {partyLabel(entry)}
                        </option>
                    ))}
            </Select>
        </div>
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
                    actions={
                        <RefreshButton
                            onClick={refresh}
                            pending={tree.isFetching}
                            stale={news.stale}
                            changedAt={news.changedAt}
                        />
                    }
                />
            </div>

            {bar}
            {tab !== 'history' && parties.length > 1 && filterBar}

            {tab === 'tree' && (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,1.3fr)_minmax(20rem,1fr)]">
                    <section className="card">
                        <div className="flex items-center justify-between gap-3 border-b border-line px-4 py-3">
                            <h2 className="text-sm font-semibold">
                                {t('membership.reporting.tree')}
                            </h2>
                            <div className="flex items-center gap-2">
                                <Tag tone="muted">
                                    {t('membership.reporting.people', { count: visible.length })}
                                </Tag>
                                <>
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
                                </>
                            </div>
                        </div>
                        <ul className="max-h-[36rem] overflow-auto p-3">
                            {parties.length === 0 ? (
                                <Roots shape={shape} list={list} />
                            ) : (
                                <>
                                    {forest.branches.map((branch) => (
                                        <PartyTree
                                            key={branch.party.partyId}
                                            branch={branch}
                                            list={list}
                                        />
                                    ))}
                                    {forest.unaffiliated.length > 0 && (
                                        <li>
                                            <p className="mt-2 text-xs font-semibold text-ink-muted">
                                                {t('membership.reporting.noParty')}
                                            </p>
                                            <ul className="ml-2">
                                                <Roots
                                                    shape={shapeOf(forest.unaffiliated)}
                                                    list={list}
                                                />
                                            </ul>
                                        </li>
                                    )}
                                </>
                            )}
                        </ul>
                    </section>
                    {card}
                </div>
            )}

            {tab === 'chart' && (
                <section className="card p-4">
                    <OrgChart
                        {...(parties.length === 0
                            ? { reporting: { roots: shape.roots, children: shape.children } }
                            : { party: forest })}
                        marks={marks}
                    />
                </section>
            )}

            {tab === 'history' && (
                <div className="grid gap-6 lg:grid-cols-[minmax(0,1fr)_minmax(0,1.4fr)]">
                    <RecentLineChanges
                        nodes={nodes}
                        nameFor={nameFor}
                        selectedId={selected?.accountId ?? ''}
                        onSelect={setSelectedId}
                    />
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
                                actorPath={actorPathFor(mayReadAccounts)}
                                actorPicture={actorPicture}
                                hideUnchanged
                                timeline={lineTimeline(
                                    story.data,
                                    t('membership.reporting.reportsTo'),
                                    nameFor,
                                )}
                            />
                        )}
                    </section>
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
    marks,
    canEdit,
    onEdit,
}: {
    readonly selected: ReportingTreeNode | undefined;
    readonly nodes: readonly ReportingTreeNode[];
    readonly marks: {
        readonly meId: string;
        readonly admins: ReadonlySet<string>;
        readonly parties: ReadonlyMap<string, ReportingTreeParty>;
        readonly openable: boolean;
    };
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
    const isMe = selected.accountId === marks.meId;
    const manager =
        selected.reportsToAccountId === null
            ? undefined
            : nodes.find((node) => node.accountId === selected.reportsToAccountId);
    const reports = nodes
        .filter((node) => node.reportsToAccountId === selected.accountId)
        .sort((a, b) => nameOf(a).localeCompare(nameOf(b)));
    return (
        <section className="card space-y-4 p-5">
            <div className="flex items-start justify-between gap-3">
                <div className="flex items-center gap-4">
                    <NodeAvatar node={selected} size="lg" />
                    <div className="min-w-0">
                        <h2 className="text-base font-semibold">
                            <NameLink
                                node={selected}
                                openable={marks.openable}
                                className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
                            />
                        </h2>
                        <p className="text-sm text-ink-muted">{selected.jobTitle}</p>
                        <PartyLine
                            node={selected}
                            parties={marks.parties}
                            className="text-xs text-ink-faint"
                        />
                        <Badges
                            node={selected}
                            isMe={isMe}
                            isAdmin={marks.admins.has(selected.accountId)}
                        />
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
                        {manager !== undefined ? (
                            <NameLink
                                node={manager}
                                openable={marks.openable}
                                className="flex items-center gap-2"
                            >
                                <NodeAvatar node={manager} size="sm" />
                                <span className="underline decoration-line-strong underline-offset-2 hover:decoration-accent">
                                    {nameOf(manager)}
                                </span>
                            </NameLink>
                        ) : selected.reportsOutsideScope ? (
                            t('membership.reporting.outsideManager')
                        ) : (
                            t('membership.reporting.noManager')
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
                                <NameLink
                                    node={report}
                                    openable={marks.openable}
                                    className="flex items-center gap-2 rounded px-1 py-0.5 text-sm hover:bg-surface-overlay"
                                >
                                    <NodeAvatar node={report} size="sm" />
                                    <span className="font-medium">{nameOf(report)}</span>
                                    <span className="text-xs text-ink-muted">
                                        {report.jobTitle}
                                    </span>
                                </NameLink>
                            </li>
                        ))}
                    </ul>
                )}
            </section>
        </section>
    );
}

interface RowMarks {
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
    readonly parties: ReadonlyMap<string, ReportingTreeParty>;
}

/** What a drawn reporting tree needs to open, close and select its people. */
interface TreeList {
    readonly collapsed: ReadonlySet<string>;
    readonly selectedId: string;
    readonly marks: RowMarks;
    readonly onSelect: (id: string) => void;
    readonly onToggle: (id: string) => void;
}

/** The people at the top of a reporting forest, each with the branch under them. */
function Roots({ shape, list }: { readonly shape: Shape; readonly list: TreeList }): ReactNode {
    return (
        <>
            {shape.roots.map((node) => (
                <TreeNode key={node.accountId} node={node} children={shape.children} list={list} />
            ))}
        </>
    );
}

/** One person and the branch under them. */
function TreeNode({
    node,
    children,
    list,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly list: TreeList;
}): ReactNode {
    const { t } = useTranslation();
    const { collapsed, selectedId, marks, onSelect, onToggle } = list;
    const kids = children.get(node.accountId) ?? [];
    const open = !collapsed.has(node.accountId);
    const isMe = node.accountId === marks.meId;
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
                <PersonRow node={node} marks={marks} onSelect={onSelect}>
                    <span className="ml-2 text-[11px] text-ink-faint">
                        {t('membership.reporting.reports', { count: node.directReports })}
                    </span>
                </PersonRow>
            </div>
            {open && kids.length > 0 && (
                <ul className="ml-4 border-l border-line-subtle pl-2">
                    {kids.map((child) => (
                        <TreeNode
                            key={child.accountId}
                            node={child}
                            children={children}
                            list={list}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}

/** A person as a row: picture, name, title and what marks them out; selecting is the click. */
function PersonRow({
    node,
    marks,
    onSelect,
    children,
}: {
    readonly node: ReportingTreeNode;
    readonly marks: RowMarks;
    readonly onSelect: (id: string) => void;
    readonly children?: ReactNode;
}): ReactNode {
    const isMe = node.accountId === marks.meId;
    return (
        <button
            type="button"
            className="flex min-w-0 flex-1 items-center gap-2 rounded px-1 py-0.5 text-left hover:bg-surface-overlay"
            onClick={() => onSelect(node.accountId)}
        >
            <NodeAvatar node={node} size="sm" />
            <span className="min-w-0">
                <span className={`text-sm ${isMe ? 'font-semibold' : 'font-medium'}`}>
                    {nameOf(node)}
                </span>{' '}
                <span className="text-xs text-ink-muted">{node.jobTitle}</span>
                {children}
                <PartyLine node={node} parties={marks.parties} />
            </span>
            <Badges node={node} isMe={isMe} isAdmin={marks.admins.has(node.accountId)} />
        </button>
    );
}

/** One party, the reporting tree of the people who work in it, and the parties below it. */
function PartyTree({
    branch,
    list,
}: {
    readonly branch: PartyBranch;
    readonly list: TreeList;
}): ReactNode {
    return (
        <li>
            <details open className="py-1">
                <summary className="cursor-pointer text-sm font-semibold">
                    {partyLabel(branch.party)}{' '}
                    <span className="font-normal text-ink-faint">({branch.members.length})</span>
                </summary>
                <ul className="ml-2">
                    <Roots shape={branch.shape} list={list} />
                </ul>
                {branch.below.length > 0 && (
                    <ul className="ml-4 border-l border-line-subtle pl-2">
                        {branch.below.map((below) => (
                            <PartyTree key={below.party.partyId} branch={below} list={list} />
                        ))}
                    </ul>
                )}
            </details>
        </li>
    );
}
