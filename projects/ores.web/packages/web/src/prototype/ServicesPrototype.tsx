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
 * See the running services, from
 * doc/knowledge/journeys/operations/journey_see_the_running_services.org.
 * The rows are fixtures shaped by the telemetry.v1.services.list reply: the
 * latest sample of every instance that reported in the last five minutes.
 */

import { useState, type ReactNode } from 'react';
import { Button, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { GapPanel, OperationsNav, type ScreenGap } from './OperationsParts.js';
import { asMinutes, serviceSamples } from './fixtures.js';

const VARIANTS = [
    {
        id: 'reporting',
        name: 'Reporting',
        gist: 'Six instances reported in the last five minutes, two of them two builds of ores.compute.service.',
    },
    {
        id: 'empty',
        name: 'Nothing reported',
        gist: 'The case the installation shows while it starts, or after the services stop.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'A stopped instance disappears from the list',
        body: 'The read keeps the latest sample of each instance among the rows of the last five minutes, so an instance that went quiet is simply absent — the screen cannot show its last heartbeat or state that it stopped.',
    },
    {
        title: 'The version is the release only',
        body: 'The heartbeat carries the release the service was built with, not the full build string, so two builds of one release look identical here.',
    },
    {
        title: 'The reply is in no particular order',
        body: 'telemetry.v1.services.list orders nothing, so the table is sorted on the screen; the server offers no order.',
    },
    {
        title: 'No permission gates the read',
        body: 'The handler authenticates the caller and checks nothing else; any signed-in person can read every instance.',
    },
];

export function ServicesPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'reporting');
    const [updatedAt, setUpdatedAt] = useState('14:32:05');
    const [log, setLog] = useState<readonly string[]>([]);

    const refresh = (): void => {
        setUpdatedAt('14:33:02');
        setLog((entries) => [...entries, 'refresh · re-read every instance at 14:33:02']);
    };

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <OperationsNav pathname="/prototype/services" />
                <PageHeader
                    title="Operations: services"
                    description="Every service instance that reported in the last five minutes."
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">Updated {updatedAt}</span>
                            <Button variant="secondary" onClick={refresh}>
                                Refresh
                            </Button>
                        </div>
                    }
                />
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture shaped by the
                    telemetry.v1.services.list reply. Nothing on this page reads the server.
                </Notice>

                {active.id === 'reporting' ? <ReportingPanel /> : <EmptyPanel />}

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
                                rows: <span className="font-mono text-ink">{active.id === 'reporting' ? String(serviceSamples.length) : '0'}</span>
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

function ReportingPanel(): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Reported instances</h2>
                <span className="text-xs text-ink-faint">
                    {serviceSamples.length} instances · two services run more than one
                </span>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">Service</th>
                        <th className="py-1 font-normal">Instance</th>
                        <th className="py-1 font-normal">Version</th>
                        <th className="py-1 font-normal">Last heartbeat</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {serviceSamples.map((row) => (
                        <tr key={`${row.serviceName}-${row.instanceId}`}>
                            <td className="py-2 font-mono">{row.serviceName}</td>
                            <td className="py-2 font-mono">{row.instanceId}</td>
                            <td className="py-2 font-mono">{row.version}</td>
                            <td className="py-2">
                                {asMinutes(row.lastHeartbeatSeconds)} ago
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">
                Read-only. One row per instance that reported in the last five minutes. Two
                instances of one service are two rows. The version is the release the instance
                sends with its heartbeat.
            </p>
        </section>
    );
}

function EmptyPanel(): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Reported instances</h2>
                <Tag tone="warn">Nothing reported in the last five minutes</Tag>
            </header>
            <p className="text-sm text-ink-muted">
                No service instance has reported. The installation may be starting, or the services
                may have stopped. The screen cannot tell which: an instance that went quiet leaves
                the list without a trace.
            </p>
        </section>
    );
}
