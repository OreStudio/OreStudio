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

/**
 * Watch the compute grid, from
 * doc/knowledge/journeys/operations/journey_watch_the_compute_grid.org.
 *
 * The node table is the whole installation's: the node read carries no tenant
 * predicate, so every session sees the whole fleet. The counters above it are
 * not: the poller computes them for one tenant, so the panel says whose they
 * are rather than presenting them as every tenant's work.
 *
 * One row per machine carries the runner that reports for it, because one
 * runner runs on each node: the state, version and instance are columns of the
 * node rather than a second table restating the node rows in another order. A
 * node whose runner never reported keeps its row and says missing rather than
 * leaving the table.
 */

import { useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';
import type { GridNodeRow, GridView, SeriesRange } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translator } from '../i18n/translate.js';
import { Button, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import {
    GapPanel,
    InstanceStateTag,
    InstanceVersion,
    OperationsBack,
    SERVICE_RUNNING_WINDOW_MINUTES,
    newestVersionOf,
    type ScreenGap,
} from './OperationsParts.js';
import { GridLoadTab, GridNodeLoad } from './GridSeries.js';
import { GridUsageTab } from './GridUsage.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { gridMachinePath, gridNodePath } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { RefreshNow } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    'EE26E58C-F389-4E82-BF96-EC5324F9F795',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
];

/** The tabs of the grid screen, in the order they are drawn. */
export const GRID_TABS = ['overview', 'usage', 'machines', 'nodes', 'heatmap'] as const;

/** Where the grid read's answer is cached, so a test can seed it. */
export const GRID_QUERY_KEY = ['operations-grid'] as const;

/** The instant a read answered, as the wall clock a person reads, in UTC. */
export function readTime(at: number): string {
    return `${new Date(at).toISOString().slice(11, 19)} UTC`;
}

/** A byte count in GiB, as the node table states what a node fetched. */
export function asGiB(bytes: number, t: Translator['t']): string {
    return t('operations.grid.units.gib', { value: (bytes / 1024 / 1024 / 1024).toFixed(2) });
}

/** A byte count in MiB, as the node table states what a node uploaded. */
export function asMiB(bytes: number, t: Translator['t']): string {
    return t('operations.grid.units.mib', { value: Math.round(bytes / 1024 / 1024) });
}

/** A task duration in whole seconds, as the prototype states it. */
export function asSeconds(milliseconds: number, t: Translator['t']): string {
    return t('operations.grid.units.seconds', { seconds: Math.round(milliseconds / 1000) });
}

/**
 * How long ago a node heartbeated, in the units a person reads.
 *
 * The unit words come from the catalogue rather than from here, so a French or
 * Portuguese reader does not read English abbreviations inside their own
 * sentence.
 */
export function formatAge(seconds: number, t: Translator['t']): string {
    if (seconds < 60) {
        return t('operations.grid.age.seconds', { seconds });
    }
    const minutes = Math.floor(seconds / 60);
    if (minutes < 60) {
        return t('operations.grid.age.minutesSeconds', {
            minutes,
            seconds: seconds % 60,
        });
    }
    return t('operations.grid.age.hoursMinutes', {
        hours: Math.floor(minutes / 60),
        minutes: minutes % 60,
    });
}

/**
 * The last eight characters of an instance id, as the row states them.
 *
 * The ids are UUIDv7, so they begin with a millisecond timestamp: agents
 * started in the same moment share their leading hex digits, which makes the
 * head say "when this process started" rather than which process it is. The
 * tail is the random part, so it is the part that tells two runners apart. The
 * full id stays in the cell's title.
 */
export function instanceTail(instanceId: string): string {
    return instanceId.slice(-8);
}

/** One node's name: the hostname, or the id beside a plain statement it is one. It opens the node's page. */
function NodeName({ node }: { readonly node: GridNodeRow }): ReactNode {
    const { t } = useTranslation();
    if (node.host === null) {
        return (
            <span className="flex flex-wrap items-center gap-2">
                <Link to={gridNodePath(node.host_id)} className="font-mono underline">
                    {node.host_id}
                </Link>
                <Tag tone="muted">{t('operations.grid.nodes.noHost')}</Tag>
            </span>
        );
    }
    return (
        <Link to={gridNodePath(node.host_id)} className="font-mono underline">
            {node.host}
        </Link>
    );
}

/** The runner's instance, shown by the tail with the full id in the title. */
function RunnerInstance({ node }: { readonly node: GridNodeRow }): ReactNode {
    if (node.instance_id === null) {
        return <span className="font-mono text-ink-faint">—</span>;
    }
    return (
        <span className="font-mono" title={node.instance_id}>
            {instanceTail(node.instance_id)}
        </span>
    );
}

/** One computer: the nodes that report it, and whether any of them is running. */
interface Machine {
    readonly name: string;
    readonly nodes: readonly GridNodeRow[];
    readonly running: number;
}

/**
 * The nodes grouped by the computer they say they run on, in name order.
 *
 * A computer is up when one of its nodes is running, which is the runner state
 * the nodes table already shows. A node that names no machine is grouped under
 * the empty name, which the panel labels.
 */
export function machinesOf(nodes: readonly GridNodeRow[]): readonly Machine[] {
    const byName = new Map<string, GridNodeRow[]>();
    for (const node of nodes) {
        byName.set(node.machine, [...(byName.get(node.machine) ?? []), node]);
    }
    return [...byName.entries()]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([name, group]) => ({
            name,
            nodes: group,
            running: group.filter((node) => node.state === 'running').length,
        }));
}

/** What a computer says about itself, read from its nodes. */
export interface MachineFacts {
    readonly cpuCount: number;
    readonly ramMb: number;
}

/**
 * The facts of a computer from the samples of its nodes.
 *
 * Every node on a computer has its own host row, and each holds the same
 * processors and memory once the node has reported them, so the largest figure
 * is the one a node actually gave.
 */
export function machineFacts(nodes: readonly GridNodeRow[]): MachineFacts {
    return {
        cpuCount: Math.max(0, ...nodes.map((node) => node.cpu_count)),
        ramMb: Math.max(0, ...nodes.map((node) => node.ram_mb)),
    };
}

/** A memory size in MiB as gibibytes, or a dash when the computer gave none. */
export function formatRam(ramMb: number): string {
    return ramMb <= 0 ? '—' : `${(ramMb / 1024).toFixed(1)} GiB`;
}

/** A computer's own page: its hardware, its nodes and what they have done. */
function MachineInfo({
    machine,
    nodes,
}: {
    readonly machine: string;
    readonly nodes: readonly GridNodeRow[];
}): ReactNode {
    const { t } = useTranslation();
    const facts = machineFacts(nodes);
    const running = nodes.filter((node) => node.state === 'running').length;
    const field = (label: string, value: ReactNode): ReactNode => (
        <div className="space-y-1">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="font-mono text-sm">{value}</dd>
        </div>
    );
    return (
        <section className="card space-y-4 p-6">
            <h2 className="text-lg font-medium">
                {machine === '' ? t('operations.grid.machines.unnamed') : machine}
            </h2>
            <dl className="grid gap-x-8 gap-y-4 sm:grid-cols-2 lg:grid-cols-4">
                {field(
                    t('operations.grid.machine.cpus'),
                    facts.cpuCount > 0 ? facts.cpuCount : '—',
                )}
                {field(t('operations.grid.machine.memory'), formatRam(facts.ramMb))}
                {field(
                    t('operations.grid.machine.nodes'),
                    t('operations.grid.machines.nodes', { running, total: nodes.length }),
                )}
                {field(
                    t('operations.grid.machine.done'),
                    nodes.reduce((sum, node) => sum + node.tasks_completed, 0),
                )}
                {field(
                    t('operations.grid.machine.failed'),
                    nodes.reduce((sum, node) => sum + node.tasks_failed, 0),
                )}
            </dl>
        </section>
    );
}

/** One row per computer, so a computer that has gone down shows at a glance. */
function MachinesPanel({ nodes }: { readonly nodes: readonly GridNodeRow[] }): ReactNode {
    const { t } = useTranslation();
    const machines = machinesOf(nodes);
    const up = machines.filter((machine) => machine.running > 0).length;
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.grid.machines.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('operations.grid.machines.summary', { up, total: machines.length })}
                </span>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.machine')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.status')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.cpus')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.memory')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.nodes')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.tasksCompleted')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.machines.columns.tasksFailed')}
                        </th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {machines.map((machine) => (
                        <tr key={machine.name}>
                            <td className="py-2 font-mono">
                                <Link to={gridMachinePath(machine.name)} className="underline">
                                    {machine.name === ''
                                        ? t('operations.grid.machines.unnamed')
                                        : machine.name}
                                </Link>
                            </td>
                            <td className="py-2">
                                {machine.running > 0 ? (
                                    <Tag tone="accent">{t('operations.grid.machines.up')}</Tag>
                                ) : (
                                    <Tag tone="warn">{t('operations.grid.machines.down')}</Tag>
                                )}
                            </td>
                            <td className="py-2 font-mono">
                                {machineFacts(machine.nodes).cpuCount || '—'}
                            </td>
                            <td className="py-2 font-mono">
                                {formatRam(machineFacts(machine.nodes).ramMb)}
                            </td>
                            <td className="py-2 font-mono">
                                {t('operations.grid.machines.nodes', {
                                    running: machine.running,
                                    total: machine.nodes.length,
                                })}
                            </td>
                            <td className="py-2 font-mono">
                                {machine.nodes.reduce((sum, node) => sum + node.tasks_completed, 0)}
                            </td>
                            <td className="py-2 font-mono">
                                {machine.nodes.reduce((sum, node) => sum + node.tasks_failed, 0)}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
            {machines.some((machine) => machine.name === '') && (
                <p className="text-xs text-ink-faint">{t('operations.grid.machines.hint')}</p>
            )}
        </section>
    );
}

function NodesPanel({ nodes: every }: { readonly nodes: readonly GridNodeRow[] }): ReactNode {
    const { t } = useTranslation();
    const [search] = useSearchParams();
    // A machine in the address narrows the list to its nodes. An empty name is the
    // nodes that report no machine, and no parameter is every node.
    const machine = search.get('machine');
    const nodes = machine === null ? every : every.filter((node) => node.machine === machine);
    const [offset, setOffset] = useState(0);
    const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
    const running = nodes.filter((node) => node.state === 'running');
    const lost = nodes.filter((node) => node.state === 'lost');
    const missing = nodes.filter((node) => node.state === 'missing');
    const newestVersion = newestVersionOf(running);
    const start = Math.min(
        offset,
        Math.max(0, Math.floor((nodes.length - 1) / pageSize) * pageSize),
    );
    const page = nodes.slice(start, start + pageSize);
    const { first, last } = pageBounds(start, page.length);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.grid.nodes.title')}</h2>
                <div className="flex flex-wrap items-center gap-2 text-xs text-ink-faint">
                    <span>{t('operations.grid.nodes.rows', { count: nodes.length })}</span>
                    <span>
                        {t('operations.grid.nodes.reported', {
                            running: running.length,
                            total: nodes.length,
                            minutes: SERVICE_RUNNING_WINDOW_MINUTES,
                        })}
                    </span>
                    {lost.length > 0 && (
                        <Tag tone="muted">
                            {t('operations.grid.nodes.lost', { count: lost.length })}
                        </Tag>
                    )}
                    {missing.length > 0 && (
                        <Tag tone="warn">
                            {t('operations.grid.nodes.missing', { count: missing.length })}
                        </Tag>
                    )}
                </div>
            </header>
            {machine !== null && (
                <div className="flex flex-wrap items-center gap-3 text-sm">
                    <Tag tone="accent">
                        {t('operations.grid.nodes.filter', {
                            machine:
                                machine === '' ? t('operations.grid.machines.unnamed') : machine,
                        })}
                    </Tag>
                    <Link to="/operations/grid?tab=nodes" className="underline">
                        {t('operations.grid.nodes.clearFilter')}
                    </Link>
                </div>
            )}
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.node')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.machine')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.status')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.version')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.instance')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksCompleted')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksFailed')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.tasksSinceLast')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.meanTime')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.slowest')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.fetched')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.uploaded')}
                        </th>
                        <th className="py-1 font-normal">
                            {t('operations.grid.nodes.columns.sinceHeartbeat')}
                        </th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {page.map((node) => {
                        const quiet = node.seconds_since_hb > SERVICE_RUNNING_WINDOW_MINUTES * 60;
                        return (
                            <tr key={node.host_id}>
                                <td className="py-2">
                                    <NodeName node={node} />
                                </td>
                                <td className="py-2 font-mono">
                                    {node.machine === '' ? (
                                        <span className="text-ink-faint">—</span>
                                    ) : (
                                        node.machine
                                    )}
                                </td>
                                <td className="py-2">
                                    <InstanceStateTag state={node.state} />
                                </td>
                                <td className="py-2">
                                    <InstanceVersion
                                        instance={node}
                                        newestVersion={newestVersion}
                                    />
                                </td>
                                <td className="py-2">
                                    <RunnerInstance node={node} />
                                </td>
                                <td className="py-2 font-mono">{node.tasks_completed}</td>
                                <td className="py-2 font-mono">
                                    {node.tasks_failed > 0 ? (
                                        <Tag tone="warn">{node.tasks_failed}</Tag>
                                    ) : (
                                        node.tasks_failed
                                    )}
                                </td>
                                <td className="py-2 font-mono">{node.tasks_since_last}</td>
                                <td className="py-2 font-mono">
                                    {asSeconds(node.avg_task_duration_ms, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asSeconds(node.max_task_duration_ms, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asGiB(node.input_bytes_fetched, t)}
                                </td>
                                <td className="py-2 font-mono">
                                    {asMiB(node.output_bytes_uploaded, t)}
                                </td>
                                <td className="py-2">
                                    {quiet ? (
                                        <Tag tone="warn">{formatAge(node.seconds_since_hb, t)}</Tag>
                                    ) : (
                                        <span className="font-mono">
                                            {formatAge(node.seconds_since_hb, t)}
                                        </span>
                                    )}
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>
            <Pager
                offset={start}
                shown={page.length}
                total={nodes.length}
                pageSize={pageSize}
                showing={t('operations.grid.nodes.showing', {
                    first,
                    last,
                    total: nodes.length,
                })}
                onMove={setOffset}
                onPageSize={(size) => {
                    setPageSize(size);
                    setOffset(0);
                }}
            />
            <p className="text-xs text-ink-faint">{t('operations.grid.nodes.hint')}</p>
            <div className="flex flex-wrap items-center gap-3">
                <Button variant="secondary" disabled title={t('operations.grid.nodes.openHint')}>
                    {t('operations.grid.nodes.open')}
                </Button>
                <span className="text-xs text-ink-faint">
                    {t('operations.grid.nodes.openHint')}
                </span>
            </div>
        </section>
    );
}

/** The Nodes tab: the node list, and above it the computer it is narrowed to. */
function NodesTab({ nodes }: { readonly nodes: readonly GridNodeRow[] }): ReactNode {
    const [search] = useSearchParams();
    const machine = search.get('machine');
    return (
        <>
            {machine !== null && (
                <MachineInfo
                    machine={machine}
                    nodes={nodes.filter((node) => node.machine === machine)}
                />
            )}
            <NodesPanel nodes={nodes} />
        </>
    );
}

export function GridPage(): ReactNode {
    const { t } = useTranslation();
    const grid = useQuery({ queryKey: GRID_QUERY_KEY, queryFn: api.grid });
    const [range, setRange] = useState<SeriesRange>('1h');
    const { tab, bar } = useTabs({
        label: t('operations.grid.tabs.label'),
        tabs: GRID_TABS,
        titleOf: (candidate) => t(`operations.grid.tabs.${candidate}`),
    });

    if (grid.isPending || grid.isError) {
        return (
            <ScreenFrame
                trail={<AreaTrail area="operations" screen={t('operations.screens.grid')} />}
                title={t('operations.grid.title')}
                description={t('operations.grid.description')}
            >
                {grid.isError ? (
                    <Notice tone="error">{grid.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }

    const view = grid.data;
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.grid.gap.oneTenant.title'),
            body: t('operations.grid.gap.oneTenant.body'),
            journey: t('operations.journeys.grid'),
        },
    ];

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.grid')} />
                <PageHeader
                    title={t('operations.grid.title')}
                    description={t('operations.grid.description')}
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">
                                {t('operations.grid.updated', { at: readTime(grid.dataUpdatedAt) })}
                            </span>
                            <RefreshNow
                                onClick={() => void grid.refetch()}
                                pending={grid.isFetching}
                            />
                            <OperationsBack />
                        </div>
                    }
                />
            </div>

            {bar}

            {tab === 'overview' && <GridLoadTab range={range} onRange={setRange} />}

            {tab === 'usage' && <GridUsageTab />}

            {tab === 'machines' && <MachinesPanel nodes={view.nodes} />}

            {tab === 'nodes' && <NodesTab nodes={view.nodes} />}

            {tab === 'heatmap' && <GridNodeLoad range={range} onRange={setRange} />}

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
