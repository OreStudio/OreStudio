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

/*
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * Watch the compute grid, from
 * doc/knowledge/journeys/operations/journey_watch_the_compute_grid.org.
 * The summary and the node rows are fixtures shaped by the
 * compute.v1.telemetry.get_grid_stats reply: the newest stored grid sample
 * and the latest sample of each node. The wrappers are fixtures shaped by
 * telemetry.v1.services.list; they belong here rather than on the services
 * screen, because a wrapper runs on a node.
 */

import { useState, type ReactNode } from 'react';
import { Button, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import {
    GapPanel,
    InstanceStateTag,
    InstanceVersion,
    OperationsBack,
    newestVersionOf,
    type ScreenGap,
} from './OperationsParts.js';
import {
    asGiB,
    asMiB,
    asMinutes,
    computeWrapperServiceName,
    gridStats,
    serviceInstances,
    type PrototypeNodeSummary,
    type PrototypeServiceInstance,
} from './fixtures.js';

const VARIANTS = [
    {
        id: 'sampled',
        name: 'Sampled',
        gist: 'Four nodes, one of them quiet for three hours, one with no host record.',
    },
    {
        id: 'nosample',
        name: 'No sample yet',
        gist: 'The deployment stores no grid summary; the node table stands alone.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'The read narrows a shared grid to one tenant',
        body: "The grid belongs to the installation: every tenant's work runs on the same hosts. The read filters by the tenant of the session anyway, so an administrator sees one tenant's slice of it. The read must serve the whole grid.",
    },
    {
        title: 'The failures are stored and then dropped',
        body: 'The ingest carries the failed task count and the longest task, and the node summary drops both. A node failing every task reads as a node doing nothing.',
    },
    {
        title: 'No history behind the sample',
        body: 'The read serves the newest stored sample and no series, so a trend cannot be drawn from these operations. The poller keeps writing samples that no read returns.',
    },
    {
        title: 'A node with no host record shows its identifier only',
        body: 'The host names come from compute.v1.hosts.list joined by host id; a node whose host row is missing keeps its row with no name.',
    },
    {
        title: 'A wrapper cannot be placed on its node',
        body: 'The wrapper heartbeat carries the service name, the instance id and the release, and no host. The node sample carries the host and no release. Nothing joins the two, so the wrappers are listed beside the nodes rather than on them. A host id on the heartbeat would join them, and each node row could then state the release it runs.',
    },
    {
        title: 'No permission gates the read',
        body: 'The handler authenticates the caller and checks nothing else.',
    },
];

export function GridPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'sampled');
    const [updatedAt, setUpdatedAt] = useState('14:31:12');
    const [log, setLog] = useState<readonly string[]>([]);

    const refresh = (): void => {
        setUpdatedAt('14:32:10');
        setLog((entries) => [...entries, 'refresh · re-read the summary and the nodes at 14:32:10']);
    };

    const wrappers = serviceInstances.filter(
        (instance) => instance.serviceName === computeWrapperServiceName,
    );
    const wrappersRunning = wrappers.filter((instance) => instance.state === 'running');

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Operations: compute grid"
                    description="The installation's host and work summary, one row per node, and the compute wrappers that report for those nodes."
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">Updated {updatedAt}</span>
                            <Button variant="secondary" onClick={refresh}>
                                Refresh
                            </Button>
                            <OperationsBack />
                        </div>
                    }
                />
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture shaped by the
                    compute.v1.telemetry.get_grid_stats and the
                    telemetry.v1.services.list replies. Nothing on this page reads the server.
                </Notice>

                {active.id === 'sampled' ? (
                    <SummaryPanel />
                ) : (
                    <section className="card space-y-2 p-6">
                        <h2 className="text-lg font-medium">Grid summary</h2>
                        <Notice tone="warn">
                            No sample yet. The deployment stores no grid summary, so the summary
                            cannot be drawn; the node table below stands alone.
                        </Notice>
                    </section>
                )}

                <NodesPanel />

                <WrappersPanel wrappers={wrappers} />

                <GapPanel gaps={GAPS} />
            </div>

            <VariantBar
                variants={VARIANTS}
                active={active}
                onChoose={choose}
                state={
                    <div className="space-y-2 text-xs">
                        <div className="grid gap-x-6 gap-y-1 text-ink-muted sm:grid-cols-2">
                            <span>
                                fixture: <span className="font-mono text-ink">{active.id}</span>
                            </span>
                            <span>
                                nodes: <span className="font-mono text-ink">{String(gridStats.nodes.length)}</span>
                            </span>
                            <span>
                                wrappers reporting:{' '}
                                <span className="font-mono text-ink">
                                    {wrappersRunning.length} of {wrappers.length}
                                </span>
                            </span>
                            <span>
                                updated: <span className="font-mono text-ink">{updatedAt}</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as system administrator, on the system tenant.
                        </p>
                        {log.length === 0 ? (
                            <p className="text-ink-faint">No action yet.</p>
                        ) : (
                            <ol className="space-y-0.5 font-mono text-ink-muted">
                                {log.map((entry, index) => (
                                    <li key={`${String(index)}-${entry}`}>
                                        {index + 1}. {entry}
                                    </li>
                                ))}
                            </ol>
                        )}
                    </div>
                }
            />
        </>
    );
}

function SummaryPanel(): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Grid summary</h2>
                <span className="text-xs text-ink-faint">sampled {gridStats.sampledAt}</span>
            </header>
            <div className="grid gap-x-8 gap-y-3 text-sm sm:grid-cols-3">
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Hosts</span>
                    <span className="flex flex-wrap items-center gap-2">
                        <span className="font-mono">{gridStats.totalHosts}</span>
                        <Tag tone="neutral">Online {gridStats.onlineHosts}</Tag>
                        <Tag tone="neutral">Idle {gridStats.idleHosts}</Tag>
                    </span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Work</span>
                    <span className="flex flex-wrap items-center gap-2">
                        <span className="font-mono">
                            {gridStats.totalWorkunits} workunits · {gridStats.totalBatches} batches
                        </span>
                        <Tag tone="accent">Active {gridStats.activeBatches}</Tag>
                    </span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Outcomes</span>
                    <span className="flex flex-wrap items-center gap-2">
                        <Tag tone="neutral">{gridStats.outcomesSuccess} success</Tag>
                        <Tag tone="warn">{gridStats.outcomesClientError} client error</Tag>
                        <Tag tone="warn">{gridStats.outcomesNoReply} no reply</Tag>
                    </span>
                </div>
            </div>
            <p className="text-xs text-ink-faint">
                The grid is the installation's: every tenant's work runs on the same hosts. The
                failure counts are the outcomes the server sends; the per-node failures are dropped
                before they arrive (see below).
            </p>
        </section>
    );
}

function NodesPanel(): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Nodes</h2>
                <span className="text-xs text-ink-faint">{gridStats.nodes.length} rows</span>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">Node</th>
                        <th className="py-1 font-normal">Tasks done</th>
                        <th className="py-1 font-normal">Since last</th>
                        <th className="py-1 font-normal">Mean time</th>
                        <th className="py-1 font-normal">Fetched</th>
                        <th className="py-1 font-normal">Uploaded</th>
                        <th className="py-1 font-normal">Since heartbeat</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {gridStats.nodes.map((node) => (
                        <NodeRow key={node.hostId} node={node} />
                    ))}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">
                Read-only. A node whose last column grows is the one to look at; the node keeps its
                row while it is quiet.
            </p>
            <div className="flex flex-wrap items-center gap-3">
                <Button
                    variant="secondary"
                    disabled
                    title="Opening a node has no journey yet: it waits for the compute journeys."
                >
                    Open the node
                </Button>
                <span className="text-xs text-ink-faint">
                    Waits for the compute journeys, which own the host and workunit screens.
                </span>
            </div>
        </section>
    );
}

function WrappersPanel({
    wrappers,
}: {
    readonly wrappers: readonly PrototypeServiceInstance[];
}): ReactNode {
    const running = wrappers.filter((instance) => instance.state === 'running');
    const stopped = wrappers.filter((instance) => instance.state === 'stopped');
    const missing = wrappers.filter((instance) => instance.state === 'missing');
    const newestVersion = newestVersionOf(running);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Compute wrappers</h2>
                <div className="flex items-center gap-2 text-xs text-ink-faint">
                    <span>
                        {running.length} of {wrappers.length} reported in the last five minutes
                    </span>
                    {stopped.length > 0 && <Tag tone="muted">{stopped.length} stopped</Tag>}
                    {missing.length > 0 && <Tag tone="warn">{missing.length} missing</Tag>}
                </div>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">Instance</th>
                        <th className="py-1 font-normal">Status</th>
                        <th className="py-1 font-normal">Version</th>
                        <th className="py-1 font-normal">Last heartbeat</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {wrappers.map((instance) => (
                        <tr key={instance.instanceId ?? 'missing'}>
                            <td className="py-2 font-mono" title={instance.instanceId}>
                                {instance.instanceId === undefined
                                    ? '—'
                                    : instance.instanceId.slice(0, 8)}
                            </td>
                            <td className="py-2">
                                <InstanceStateTag state={instance.state} />
                            </td>
                            <td className="py-2">
                                <InstanceVersion
                                    instance={instance}
                                    newestVersion={newestVersion}
                                />
                            </td>
                            <td className="py-2 font-mono">
                                {instance.lastHeartbeatSeconds === undefined
                                    ? '—'
                                    : `${asMinutes(instance.lastHeartbeatSeconds)} ago`}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">
                One wrapper runs on each node and takes the work that node runs. The roster is the
                registry's five replicas; the rows are telemetry.v1.services.list. The node rows
                above are the samples these wrappers publish, and nothing joins the two yet (see
                below).
            </p>
        </section>
    );
}

function NodeRow({ node }: { readonly node: PrototypeNodeSummary }): ReactNode {
    const quiet = node.secondsSinceHeartbeat > 300;
    return (
        <tr>
            <td className="py-2">
                {node.host === undefined ? (
                    <span className="flex items-center gap-2">
                        <span className="font-mono">{node.hostId}</span>
                        <Tag tone="muted">no host record</Tag>
                    </span>
                ) : (
                    <span className="font-mono">{node.host}</span>
                )}
            </td>
            <td className="py-2 font-mono">{node.tasksCompleted}</td>
            <td className="py-2 font-mono">{node.tasksSinceLast}</td>
            <td className="py-2 font-mono">
                {node.avgTaskDurationMs === undefined
                    ? '-'
                    : `${String(Math.round(node.avgTaskDurationMs / 1000))} s`}
            </td>
            <td className="py-2 font-mono">{asGiB(node.inputBytesFetched)}</td>
            <td className="py-2 font-mono">{asMiB(node.outputBytesUploaded)}</td>
            <td className="py-2">
                {quiet ? (
                    <Tag tone="warn">{asMinutes(node.secondsSinceHeartbeat)}</Tag>
                ) : (
                    <span className="font-mono">{asMinutes(node.secondsSinceHeartbeat)}</span>
                )}
            </td>
        </tr>
    );
}
