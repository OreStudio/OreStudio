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
 * PROTOTYPE. Kept on main as the design record; nothing outside the prototype
 * routes imports it.
 *
 * See the running services, from
 * doc/knowledge/journeys/operations/journey_see_the_running_services.org.
 * The screen answers the roster the registry expects with the samples the
 * instances send, so a service that stopped keeps its row and a version that
 * lags behind is visible. The states use the installation's own words and the
 * shell's tag tones. The compute wrappers belong to the grid screen, which
 * shows them against the nodes they run on.
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
    asMinutes,
    computeWrapperServiceName,
    serviceInstances,
    type PrototypeServiceInstance,
} from './operationsFixtures.js';

const VARIANTS = [
    {
        id: 'reporting',
        name: 'Reporting',
        gist: 'The roster the registry expects, met by the samples: one instance is stopped and one build is older.',
    },
    {
        id: 'nothing',
        name: 'Nothing reported',
        gist: 'No instance reported in five minutes; every row says so, and the screen cannot say why.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'The expected services are not a read',
        body: 'The registry that states which services exist and how many replicas each expects is a codegen model, projects/modeling/service_registry.org; no operation serves it. Without it the screen can only be drawn from the samples, and a service that stops leaves the list when five minutes pass.',
    },
    {
        title: 'The state comes from absence, not from a read',
        body: 'Running here means "reported in the last five minutes"; stopped and missing come from the installation\'s service manager, the one compass services status asks. Nothing serves that state to a screen, so a reader cannot tell a service somebody stopped from one that fell over.',
    },
    {
        title: 'The version is the release only',
        body: 'The heartbeat carries the release string, not the full build string, so two builds of one release read the same and the older-build label can only compare releases.',
    },
    {
        title: 'The reply is unordered',
        body: 'telemetry.v1.services.list orders nothing; this screen sorts by service name, then instance. A stable order from the read — service, then instance — would settle it once.',
    },
    {
        title: 'No uptime',
        body: 'Nothing says when an instance started, so an instance that just restarted reads like one that has run for weeks.',
    },
    {
        title: 'No permission gates the read',
        body: 'The handler authenticates the caller and checks nothing else; any signed-in person can read every instance.',
    },
];

export function ServicesPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'reporting');
    const [readAt, setReadAt] = useState('14:32:05');
    const [log, setLog] = useState<readonly string[]>([]);

    const refresh = (): void => {
        setReadAt('14:33:02');
        setLog((entries) => [...entries, 'refresh · re-read every instance at 14:33:02']);
    };

    const reported = serviceInstances.filter(
        (instance) => instance.serviceName !== computeWrapperServiceName,
    );

    const instances =
        active.id === 'reporting'
            ? reported
            : reported.map((instance): PrototypeServiceInstance => ({
                  serviceName: instance.serviceName,
                  instanceId: undefined,
                  state: 'missing',
                  version: undefined,
                  lastHeartbeatSeconds: undefined,
              }));

    const groups = groupByService(instances);
    const running = instances.filter((instance) => instance.state === 'running');
    const stopped = instances.filter((instance) => instance.state === 'stopped');
    const missing = instances.filter((instance) => instance.state === 'missing');
    const newestVersion = newestVersionOf(running);

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Operations: services"
                    description="Every service the registry expects, met by the instances that report. The compute wrappers are the grid screen's."
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">Updated {readAt}</span>
                            <Button variant="secondary" onClick={refresh}>
                                Refresh
                            </Button>
                            <OperationsBack />
                        </div>
                    }
                />
                <Notice tone="warn">
                    PROTOTYPE. The counts, states and versions below are fixtures: the roster comes
                    from the service registry and the state from the installation's service manager,
                    and no operation serves either yet. Nothing on this page reads the server.
                </Notice>

                {active.id === 'reporting' &&
                newestVersion !== undefined &&
                hasSkew(running, newestVersion) ? (
                    <Notice tone="warn">
                        Version skew: {skewSummary(running, newestVersion)}. After a rollout, an
                        instance that did not take the build is what this line is for.
                    </Notice>
                ) : null}

                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">Instances</h2>
                        <div className="flex items-center gap-2 text-xs text-ink-faint">
                            <span>
                                {running.length} of {instances.length} instances reported in the
                                last five minutes
                            </span>
                            {stopped.length > 0 && <Tag tone="muted">{stopped.length} stopped</Tag>}
                            {missing.length > 0 && <Tag tone="warn">{missing.length} missing</Tag>}
                        </div>
                    </header>

                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">Service</th>
                                <th className="py-1 font-normal">Instances</th>
                                <th className="py-1 font-normal">Instance</th>
                                <th className="py-1 font-normal">Status</th>
                                <th className="py-1 font-normal">Version</th>
                                <th className="py-1 font-normal">Last heartbeat</th>
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {groups.map((group) =>
                                group.instances.map((instance, index) => (
                                    <tr
                                        key={`${group.serviceName}-${instance.instanceId ?? 'missing'}`}
                                    >
                                        {index === 0 && (
                                            <td className="py-2" rowSpan={group.instances.length}>
                                                <span className="font-mono">
                                                    {group.serviceName}
                                                </span>
                                            </td>
                                        )}
                                        {index === 0 && (
                                            <td className="py-2" rowSpan={group.instances.length}>
                                                <InstanceCount
                                                    reported={
                                                        group.instances.filter(
                                                            (row) => row.state === 'running',
                                                        ).length
                                                    }
                                                    expected={group.instances.length}
                                                />
                                            </td>
                                        )}
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
                                )),
                            )}
                        </tbody>
                    </table>

                    <p className="text-xs text-ink-faint">
                        Read-only. One row per expected instance, whether it reports or not. The
                        instance id is a UUID the heartbeat publisher generates at startup; the
                        column shows its first eight characters.
                    </p>
                </section>

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
                                updated: <span className="font-mono text-ink">{readAt}</span>
                            </span>
                            <span>
                                instances reporting:{' '}
                                <span className="font-mono text-ink">
                                    {running.length} of {instances.length}
                                </span>
                            </span>
                            <span>
                                newest release:{' '}
                                <span className="font-mono text-ink">{newestVersion ?? '—'}</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as system administrator, on the system tenant. The roster is
                            the registry's, less the compute wrappers the grid screen owns; the
                            samples are telemetry.v1.services.list.
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

interface ServiceGroup {
    readonly serviceName: string;
    readonly instances: readonly PrototypeServiceInstance[];
}

function groupByService(instances: readonly PrototypeServiceInstance[]): readonly ServiceGroup[] {
    const made = new Map<string, PrototypeServiceInstance[]>();
    for (const instance of instances) {
        const held = made.get(instance.serviceName);
        if (held === undefined) {
            made.set(instance.serviceName, [instance]);
        } else {
            held.push(instance);
        }
    }
    return [...made.entries()]
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([serviceName, held]) => ({ serviceName, instances: held }));
}

function InstanceCount({
    reported,
    expected,
}: {
    readonly reported: number;
    readonly expected: number;
}): ReactNode {
    if (reported < expected) {
        return (
            <Tag tone="warn">
                {reported} of {expected}
            </Tag>
        );
    }
    return (
        <span className="font-mono text-ink-muted">
            {reported} of {expected}
        </span>
    );
}

function hasSkew(running: readonly PrototypeServiceInstance[], newestVersion: string): boolean {
    return running.some((instance) => instance.version !== newestVersion);
}

function skewSummary(running: readonly PrototypeServiceInstance[], newestVersion: string): string {
    const behind = [
        ...new Set(
            running
                .filter((instance) => instance.version !== newestVersion)
                .map((instance) => instance.serviceName),
        ),
    ];
    const versions = [
        ...new Set(
            running
                .filter((instance) => instance.version !== newestVersion)
                .map((instance) => instance.version ?? ''),
        ),
    ].sort();
    return `${behind.join(', ')} runs ${versions.join(', ')} while the rest run ${newestVersion}`;
}
