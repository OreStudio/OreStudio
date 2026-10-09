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
import { useMemo, useState, type ReactNode } from 'react';
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Button, Detail, Field, Input, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';

/**
 * Reporting lines: who reports to whom, drawn as a tree.
 *
 * The indented tree is the shape, and the panel beside it is the selected
 * person and the one field this screen changes. The manager picker cannot
 * offer anybody who reports to the selected person, so the screen cannot ask
 * for a ring even if the server would take one; the change states a reason,
 * and the write is the narrow one, so a field changed elsewhere is not
 * overwritten.
 *
 * The journey's remaining gaps are stated rather than hidden: the office each
 * person works in is not readable from here, a change is a proposal that a
 * senior or the tenant administrator approves and nothing holds that approval,
 * and the line's history is not shown.
 */
export function ReportingLinesPage(): ReactNode {
    const { t } = useTranslation();
    const queryClient = useQueryClient();
    const [selectedId, setSelectedId] = useState('');
    const [collapsed, setCollapsed] = useState<ReadonlySet<string>>(new Set());
    const [manager, setManager] = useState<string | undefined>(undefined);
    const [reason, setReason] = useState('');
    const [commentary, setCommentary] = useState('');
    const [failure, setFailure] = useState('');

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

    const selected = nodes.find((node) => node.accountId === selectedId);
    /*
     * The tree states the shape and the manager, and the account read states
     * the version the write must state back. Reading it on selection is what
     * keeps the two apart: the write is refused as a conflict when somebody
     * else has changed the row since this panel read it.
     */
    const account = useQuery({
        queryKey: ['account', selected?.username ?? ''],
        queryFn: () => api.account(selected?.username ?? ''),
        enabled: selected !== undefined,
        retry: false,
    });

    const save = useMutation({
        mutationFn: (input: { readonly accountId: string; readonly to: string }) =>
            api.setReportingLine(input.accountId, {
                reportsToAccountId: input.to,
                expectedVersion: account.data == null ? '' : String(account.data.version),
                reasonCode: reason,
                commentary,
            }),
        onSuccess: async () => {
            setFailure('');
            setManager(undefined);
            await queryClient.invalidateQueries({ queryKey: ['reporting-tree'] });
            await queryClient.invalidateQueries({ queryKey: ['account'] });
        },
        onError: (error: Error) => setFailure(error.message),
    });

    if (tree.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (tree.isError) {
        return <Notice tone="error">{tree.error.message}</Notice>;
    }

    const roots = nodes.filter((node) => node.reportsToAccountId === null);
    const unrooted = nodes.filter((node) => node.depth < 0);
    const chosenReason = reason === '' ? (reasons.data?.[0]?.code ?? '') : reason;

    const saveTo = (to: string) => {
        if (selected === undefined) {
            return;
        }
        save.mutate({ accountId: selected.accountId, to });
    };

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('membership.reporting.title')}
                description={t('membership.reporting.lead')}
            />

            {failure !== '' && <Notice tone="error">{failure}</Notice>}

            <div className="grid gap-6 lg:grid-cols-[minmax(0,1.3fr)_minmax(20rem,1fr)]">
                <section className="rounded-md border border-line bg-surface-raised">
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
                                onClick={() => setCollapsed(new Set(nodes.map((n) => n.accountId)))}
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
                                selectedId={selectedId}
                                onSelect={(id) => {
                                    setSelectedId(id);
                                    setManager(undefined);
                                    setFailure('');
                                }}
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

                <section className="space-y-4 rounded-md border border-line bg-surface-raised p-4">
                    {selected === undefined ? (
                        <p className="text-sm text-ink-muted">
                            {t('membership.reporting.pickSomeone')}
                        </p>
                    ) : (
                        <>
                            <div>
                                <h2 className="text-base font-semibold">{selected.fullName}</h2>
                                <p className="text-sm text-ink-muted">{selected.jobTitle}</p>
                            </div>
                            <dl className="grid grid-cols-2 gap-4">
                                <Detail
                                    label={t('membership.reporting.reportsTo')}
                                    value={
                                        selected.reportsToAccountId === null
                                            ? t('membership.reporting.noManager')
                                            : (nodes.find(
                                                  (node) =>
                                                      node.accountId ===
                                                      selected.reportsToAccountId,
                                              )?.fullName ??
                                              selected.reportsToAccountId)
                                    }
                                />
                                <Detail
                                    label={t('membership.reporting.directReports')}
                                    value={String(selected.directReports)}
                                />
                                <Detail
                                    label={t('membership.reporting.depth')}
                                    value={
                                        selected.depth < 0
                                            ? t('membership.reporting.reachesNoRoot')
                                            : String(selected.depth)
                                    }
                                />
                            </dl>

                            <Field label={t('membership.reporting.manager')}>
                                <Select
                                    key={selected.accountId}
                                    defaultValue={selected.reportsToAccountId ?? ''}
                                    onChange={(event) => setManager(event.target.value)}
                                >
                                    <option value="">{t('membership.reporting.noManager')}</option>
                                    {managerCandidates(nodes, selected).map((node) => (
                                        <option key={node.accountId} value={node.accountId}>
                                            {node.fullName} — {node.jobTitle}
                                        </option>
                                    ))}
                                </Select>
                            </Field>

                            <Field label={t('membership.reporting.reason')}>
                                <Select
                                    value={chosenReason}
                                    onChange={(event) => setReason(event.target.value)}
                                >
                                    {(reasons.data ?? []).map((entry) => (
                                        <option key={entry.code} value={entry.code}>
                                            {entry.description}
                                        </option>
                                    ))}
                                </Select>
                            </Field>

                            <Field label={t('membership.reporting.commentary')}>
                                <Input
                                    value={commentary}
                                    onChange={(event) => setCommentary(event.target.value)}
                                />
                            </Field>

                            <div className="flex flex-wrap items-center gap-2">
                                <Button
                                    variant="primary"
                                    size="sm"
                                    disabled={save.isPending || account.isPending}
                                    onClick={() =>
                                        saveTo(manager ?? selected.reportsToAccountId ?? '')
                                    }
                                >
                                    {t('membership.reporting.save')}
                                </Button>
                                <Button
                                    variant="ghost"
                                    size="sm"
                                    disabled={save.isPending}
                                    onClick={() => {
                                        setManager('');
                                        saveTo('');
                                    }}
                                >
                                    {t('membership.reporting.clear')}
                                </Button>
                            </div>
                            {account.isError && (
                                <p className="text-xs text-ink-faint">
                                    {t('membership.reporting.noVersion')}
                                </p>
                            )}
                        </>
                    )}
                </section>
            </div>

            {unrooted.length > 0 && (
                <Notice tone="warn">
                    {t('membership.reporting.unrooted', { count: unrooted.length })}
                </Notice>
            )}

            <section className="rounded-md border border-dashed border-line-strong p-4">
                <h2 className="text-sm font-semibold">{t('membership.reporting.gaps')}</h2>
                <ul className="mt-2 list-disc space-y-1 pl-5 text-sm text-ink-muted">
                    <li>{t('membership.reporting.gapOffice')}</li>
                    <li>{t('membership.reporting.gapApproval')}</li>
                    <li>{t('membership.reporting.gapHistory')}</li>
                </ul>
            </section>
        </div>
    );
}

/** The people who may be this person's manager: everyone but their own branch. */
export function managerCandidates(
    nodes: readonly ReportingTreeNode[],
    selected: ReportingTreeNode,
): readonly ReportingTreeNode[] {
    const forbidden = new Set<string>([selected.accountId]);
    const childrenOf = (id: string): readonly ReportingTreeNode[] =>
        nodes.filter((node) => node.reportsToAccountId === id);
    const walk = (id: string): void => {
        for (const child of childrenOf(id)) {
            if (!forbidden.has(child.accountId)) {
                forbidden.add(child.accountId);
                walk(child.accountId);
            }
        }
    };
    walk(selected.accountId);
    return nodes.filter((node) => !forbidden.has(node.accountId));
}

/** One person and the branch under them. */
function TreeNode({
    node,
    children,
    collapsed,
    selectedId,
    onSelect,
    onToggle,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly collapsed: ReadonlySet<string>;
    readonly selectedId: string;
    readonly onSelect: (id: string) => void;
    readonly onToggle: (id: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const kids = children.get(node.accountId) ?? [];
    const open = !collapsed.has(node.accountId);
    return (
        <li>
            <div
                className={`flex items-center gap-1 rounded px-1 py-0.5 ${
                    selectedId === node.accountId ? 'bg-accent/10' : ''
                }`}
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
                    className="min-w-0 flex-1 rounded px-1 text-left hover:bg-surface-overlay"
                    onClick={() => onSelect(node.accountId)}
                >
                    <span className="text-sm font-medium">{node.fullName}</span>{' '}
                    <span className="text-xs text-ink-muted">{node.jobTitle}</span>
                    <span className="ml-2 text-[11px] text-ink-faint">
                        {t('membership.reporting.reports', { count: node.directReports })}
                    </span>
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
                            onSelect={onSelect}
                            onToggle={onToggle}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}
