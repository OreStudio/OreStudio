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
import { useMutation, useQuery, useQueryClient, type QueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useHeld } from '../../access/holds.js';
import { PERMISSION } from '../../access/permissions.js';
import { api, type FeedGraph, type FeedGraphNode } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { logTime } from '../../operations/LogsPage.js';
import { ReasonFields, useReason } from '../../refdata/shared.js';
import { Card } from '../../ui/Card.js';
import { Button, Dialog, Notice, Tag } from '../../ui/Primitives.js';
import { PATHS } from '../../shell/paths.js';
import { feedPageOf } from './feedTree.js';
import { FOLDERS_KEY } from './useCollections.js';
import { HELD_KEY } from './useProviderFeeds.js';

const graphKey = (node: string): readonly unknown[] => ['feed-graph', node];

/** The graph around one node, read again every few seconds while the page is open. */
function useFeedGraph(node: string) {
    return useQuery({
        queryKey: graphKey(node),
        queryFn: () => api.feedGraph(node),
        refetchInterval: 5000,
        enabled: node !== '',
    });
}

/**
 * Where a node of the graph opens: a feed's own page, the Graph tab of the surface a vendor view
 * quotes, since a view has no page of its own and is stopped there, or a source's provider on
 * the Provider tab. The graph is what names a view's surface.
 */
export function nodePage(
    node: Pick<FeedGraphNode, 'kind' | 'name' | 'feed_config_id' | 'provider_code'>,
    graph?: FeedGraph,
): string | undefined {
    if (node.kind === 'source')
        return `${PATHS.synthetic}?tab=provider&provider=${encodeURIComponent(node.provider_code)}`;
    if (node.kind === 'view') {
        const surface = graph?.edges.find((e) => e.node === node.name && e.role === 'surface');
        const latent = graph?.nodes.find((n) => n.name === surface?.input);
        const page = latent === undefined ? undefined : nodePage(latent);
        return page === undefined ? undefined : `${page}?tab=graph`;
    }
    if (node.feed_config_id === '') return undefined;
    if (node.kind === 'latent')
        return feedPageOf({ kind: 'fx_vol_surface', id: node.feed_config_id });
    return feedPageOf({
        kind: node.name.includes('.ir.yield_curve.') ? 'ir_curve' : 'fx_spot',
        id: node.feed_config_id,
    });
}

/** A node's name, as a link to where it opens. */
function NodeName({
    node,
    name,
    graph,
}: {
    readonly node: FeedGraphNode | undefined;
    readonly name: string;
    readonly graph: FeedGraph;
}): ReactNode {
    const page = node === undefined ? undefined : nodePage(node, graph);
    if (page === undefined) return <span className="font-mono text-xs">{name}</span>;
    return (
        <Link className="font-mono text-xs text-accent hover:text-accent-bright" to={page}>
            {name}
        </Link>
    );
}

const TONE: Readonly<Record<string, 'up' | 'down' | 'warn' | 'muted'>> = {
    running: 'up',
    stopped: 'muted',
    starved: 'warn',
    failing: 'down',
};

/** A node's state, as a tag. */
export function NodeState({ state }: { readonly state: string }): ReactNode {
    const { t } = useTranslation();
    return <Tag tone={TONE[state] ?? 'muted'}>{t(`marketdata.feedGraph.state.${state}`)}</Tag>;
}

/**
 * Waits for every one of several node stops or starts, so a failure stops none of the others
 * from landing, and fails with how many failed and the first reason.
 */
async function settleAll(calls: readonly Promise<void>[]): Promise<void> {
    const settled = await Promise.allSettled(calls);
    const failed = settled.filter((one) => one.status === 'rejected');
    if (failed.length === 0) return;
    const first = failed[0] as PromiseRejectedResult;
    const reason = first.reason instanceof Error ? first.reason.message : String(first.reason);
    throw new Error(`${failed.length} of ${settled.length} failed: ${reason}`);
}

/** Reads again what a node stop or start changes: the graph, the messages and the feed lists. */
function refreshNodes(queries: QueryClient): void {
    for (const queryKey of [['feed-graph'], ['feed-messages'], FOLDERS_KEY, HELD_KEY])
        void queries.invalidateQueries({ queryKey });
}

/** Stop or start one node, with the person's reason for a stop. */
function NodeControl({ node }: { readonly node: FeedGraphNode }): ReactNode {
    const { t } = useTranslation();
    const mayRun = useHeld()(PERMISSION.feedsRun);
    const queries = useQueryClient();
    const [stopping, setStopping] = useState(false);
    const start = useMutation({
        mutationFn: () => api.startFeedNode(node.name),
        onSettled: () => refreshNodes(queries),
    });
    if (!mayRun) return null;
    return (
        <>
            {node.state === 'stopped' ? (
                <Button size="sm" disabled={start.isPending} onClick={() => start.mutate()}>
                    {t('marketdata.feedGraph.start')}
                </Button>
            ) : (
                <Button size="sm" onClick={() => setStopping(true)}>
                    {t('marketdata.feedGraph.stop')}
                </Button>
            )}
            {start.error !== null && <Notice tone="error">{start.error.message}</Notice>}
            {stopping && (
                <StopNodeDialog
                    title={t('marketdata.feedGraph.stopTitle', { node: node.name })}
                    names={[node.name]}
                    onClose={() => setStopping(false)}
                />
            )}
        </>
    );
}

/** The question a stop asks: what it does, and why the person stops the nodes. */
function StopNodeDialog({
    title,
    names,
    onClose,
}: {
    readonly title: string;
    readonly names: readonly string[];
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend', true);
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const stop = useMutation({
        mutationFn: () =>
            settleAll(
                names.map((name) =>
                    api.stopFeedNode(name, {
                        reason_code: reason.code,
                        commentary: commentary.trim(),
                    }),
                ),
            ),
        onSettled: () => refreshNodes(queries),
        onSuccess: () => onClose(),
    });
    return (
        <Dialog
            title={title}
            name="stop-feed-node"
            onClose={onClose}
            footer={
                <>
                    <Button onClick={onClose}>{t('refdata.records.cancel')}</Button>
                    <Button
                        variant="danger"
                        disabled={reason.code === '' || missing || stop.isPending}
                        onClick={() => stop.mutate()}
                    >
                        {t('marketdata.feedGraph.stop')}
                    </Button>
                </>
            }
        >
            <p className="text-sm">{t('marketdata.feedGraph.stopEffect')}</p>
            <ReasonFields
                reason={reason}
                commentary={commentary}
                onCommentary={setCommentary}
                missing={missing}
            />
            {stop.error !== null && <Notice tone="error">{stop.error.message}</Notice>}
        </Dialog>
    );
}

/**
 * Stops or starts every feed of a provider that has no source of its own, such as latent, as
 * nodes of the graph: the vendors' sources hold its feeds, so a hand run cannot start one, and a
 * node stop is stored, said to the nodes below it, and lifted by a start.
 */
export function ProviderNodeControl({
    provider,
    name,
}: {
    readonly provider: string;
    readonly name: string;
}): ReactNode {
    const { t } = useTranslation();
    const mayRun = useHeld()(PERMISSION.feedsRun);
    const queries = useQueryClient();
    const [stopping, setStopping] = useState(false);
    const graph = useFeedGraph('');
    const nodes = (graph.data?.nodes ?? []).filter(
        (n) => n.provider_code === provider && n.kind !== 'source' && n.kind !== 'view',
    );
    const stopped = nodes.filter((n) => n.state === 'stopped').map((n) => n.name);
    const others = nodes.filter((n) => n.state !== 'stopped').map((n) => n.name);
    const start = useMutation({
        mutationFn: () => settleAll(stopped.map((one) => api.startFeedNode(one))),
        onSettled: () => refreshNodes(queries),
    });
    if (!mayRun || nodes.length === 0) return null;
    return (
        <>
            <Button
                size="sm"
                disabled={stopped.length === 0 || start.isPending}
                onClick={() => start.mutate()}
            >
                {t('marketdata.synthetic.runs.start')}
            </Button>
            <Button size="sm" disabled={others.length === 0} onClick={() => setStopping(true)}>
                {t('marketdata.synthetic.runs.stop')}
            </Button>
            {start.error !== null && <Notice tone="error">{start.error.message}</Notice>}
            {stopping && (
                <StopNodeDialog
                    title={t('marketdata.feedGraph.stopTitle', { node: name })}
                    names={others}
                    onClose={() => setStopping(false)}
                />
            )}
        </>
    );
}

/** The nodes on one side of a node, each with its role, its state and its own control. */
function Neighbours({
    title,
    graph,
    rows,
}: {
    readonly title: string;
    readonly graph: FeedGraph;
    readonly rows: readonly { readonly name: string; readonly role: string }[];
}): ReactNode {
    const { t } = useTranslation();
    const byName = new Map(graph.nodes.map((n) => [n.name, n] as const));
    return (
        <Card title={title}>
            {rows.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.feedGraph.nothing')}</p>
            ) : (
                <table className="w-full text-sm">
                    <tbody>
                        {rows.map((row) => {
                            const node = byName.get(row.name);
                            return (
                                <tr
                                    key={`${row.role}|${row.name}`}
                                    className="border-t border-line"
                                >
                                    <td className="py-1 pr-2 text-ink-muted">
                                        {t(`marketdata.feedGraph.role.${row.role}`)}
                                    </td>
                                    <td className="py-1 pr-2">
                                        <NodeName node={node} name={row.name} graph={graph} />
                                    </td>
                                    <td className="py-1 pr-2">
                                        {node === undefined ? null : (
                                            <NodeState state={node.state} />
                                        )}
                                    </td>
                                    <td className="py-1 text-right">
                                        {/* A neighbour that has its own page is stopped there; a vendor view has none, so its surface's page stops it. */}
                                        {node?.kind === 'view' && <NodeControl node={node} />}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            )}
        </Card>
    );
}

/**
 * Where a feed sits in the synthetic feed graph: its state with its reason and, for a starved
 * node, the node that starved it; what it reads; and what reads it. A latent surface's readers
 * are its vendor views, each stopped and started on its own.
 */
export function FeedGraphTab({ name }: { readonly name: string }): ReactNode {
    const { t } = useTranslation();
    const graph = useFeedGraph(name);
    if (graph.error !== null) return <Notice tone="error">{graph.error.message}</Notice>;
    if (graph.data === undefined) return null;
    const me = graph.data.nodes.find((n) => n.name === name);
    if (me === undefined) return <Notice tone="info">{t('marketdata.feedGraph.unknown')}</Notice>;
    const cause = graph.data.nodes.find((n) => n.name === me.cause);
    const reads = graph.data.edges
        .filter((e) => e.node === name)
        .map((e) => ({ name: e.input, role: e.role }));
    const readBy = graph.data.edges
        .filter((e) => e.input === name)
        .map((e) => ({ name: e.node, role: e.role }));
    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">{t('marketdata.feedGraph.lead')}</p>
            <Card
                title={t(`marketdata.feedGraph.kind.${me.kind}`)}
                aside={<NodeControl node={me} />}
            >
                <div className="space-y-2 text-sm">
                    <div className="flex items-center gap-2">
                        <NodeState state={me.state} />
                        <span>{me.reason}</span>
                    </div>
                    {me.cause !== '' && (
                        <div>
                            {t('marketdata.feedGraph.causedBy')}{' '}
                            <NodeName node={cause} name={me.cause} graph={graph.data} />
                        </div>
                    )}
                </div>
            </Card>
            <Neighbours title={t('marketdata.feedGraph.reads')} graph={graph.data} rows={reads} />
            <Neighbours title={t('marketdata.feedGraph.readBy')} graph={graph.data} rows={readBy} />
        </div>
    );
}

const LEVEL_TONE: Readonly<Record<string, 'warn' | 'down'>> = {
    warn: 'warn',
    error: 'down',
    fatal: 'down',
};

const MESSAGES_SHOWN = 100;

/** A node's messages over the last day, from telemetry, by the node's name. */
export function FeedMessagesTab({ name }: { readonly name: string }): ReactNode {
    const { t } = useTranslation();
    const logs = useQuery({
        queryKey: ['feed-messages', name],
        queryFn: () =>
            api.logs({
                range: '24h',
                level: '',
                source: '',
                component: '',
                tag: name,
                message: '',
                offset: 0,
                limit: MESSAGES_SHOWN,
            }),
        refetchInterval: 5000,
        enabled: name !== '',
    });
    if (name === '') return <Notice tone="info">{t('marketdata.feedGraph.unknown')}</Notice>;
    if (logs.error !== null) return <Notice tone="error">{logs.error.message}</Notice>;
    const entries = logs.data?.entries ?? [];
    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">{t('marketdata.feedGraph.messagesLead')}</p>
            {(logs.data?.total ?? 0) > entries.length && (
                <p className="text-xs text-ink-muted">
                    {t('marketdata.feedGraph.latestOnly', { count: entries.length })}
                </p>
            )}
            {logs.data !== undefined && entries.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.feedGraph.noMessages')}</p>
            ) : (
                <table className="w-full text-sm">
                    <tbody>
                        {entries.map((entry) => (
                            <tr key={String(entry.id)} className="border-t border-line align-top">
                                <td className="py-1 pr-2 font-mono text-xs">
                                    {logTime(entry.timestamp) ?? ''}
                                </td>
                                <td className="py-1 pr-2">
                                    <Tag tone={LEVEL_TONE[entry.level] ?? 'muted'} small>
                                        {entry.level}
                                    </Tag>
                                </td>
                                <td className="py-1">{entry.message}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </div>
    );
}
