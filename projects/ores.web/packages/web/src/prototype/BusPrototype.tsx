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
 * Watch the message bus, from
 * doc/knowledge/journeys/operations/journey_watch_the_message_bus.org.
 * The rows are fixtures shaped by the NATS sample replies: server counters
 * that run since the server started, and one row per stream per sample.
 */

import { useState, type ReactNode } from 'react';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { asMiB, natsServerSamples, natsStreamSamples } from './fixtures.js';

const VARIANTS = [
    {
        id: 'sampled',
        name: 'Sampled',
        gist: 'The server sample every 30 seconds gives the vitals, the streams and one hour of counters for the trend.',
    },
    {
        id: 'empty',
        name: 'Empty range',
        gist: 'The range holds no samples; the screen points at the poller before anything else.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'Nothing lists the streams',
        body: 'A stream appears in the table only once a sample carries its name; a stream with no traffic is invisible.',
    },
    {
        title: 'A range longer than the limit truncates in silence',
        body: 'The read takes at most 1000 samples and the reply carries no total, so a busy range quietly loses its oldest samples.',
    },
    {
        title: 'The counters run since the server started',
        body: 'Messages and bytes are running totals; the change over the range is computed on the screen because no operation sends a rate.',
    },
    {
      title: 'A slow consumer cannot be named',
      body: 'The slow-consumer count is a number; no operation says which consumer fell behind.',
    },
    {
        title: 'No permission gates the read',
        body: 'The handler authenticates the caller and checks nothing else.',
    },
];

export function BusPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'sampled');
    const [range, setRange] = useState('Last hour');
    const [appliedRange, setAppliedRange] = useState('Last hour');
    const [log, setLog] = useState<readonly string[]>([]);

    const apply = (): void => {
        setAppliedRange(range);
        setLog((entries) => [...entries, `apply · re-read the range "${range}" at 14:32:11`]);
    };

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Operations: message bus"
                    description="The NATS server's vitals and one row per stream."
                    actions={
                        <div className="flex items-end gap-3">
                            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                                Range
                                <Select value={range} onChange={(e) => setRange(e.target.value)}>
                                    <option>Last 15 minutes</option>
                                    <option>Last hour</option>
                                    <option>Last 6 hours</option>
                                </Select>
                            </label>
                            <Button variant="secondary" onClick={apply}>
                                Apply
                            </Button>
                            <OperationsBack />
                        </div>
                    }
                />
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture shaped by the NATS server and stream
                    sample replies. Nothing on this page reads the server.
                </Notice>

                {active.id === 'sampled' ? (
                    <>
                        <VitalsPanel />
                        <StreamsPanel />
                        <TrendPanel />
                    </>
                ) : (
                    <>
                        <section className="card space-y-2 p-6">
                            <h2 className="text-lg font-medium">NATS server</h2>
                            <Notice tone="warn">
                                The range holds no samples. The telemetry service takes one sample
                                every 30 seconds; an empty range points at its poller first.
                            </Notice>
                        </section>
                        <StreamsPanel empty />
                    </>
                )}

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
                                range: <span className="font-mono text-ink">{appliedRange}</span>
                            </span>
                            <span>
                                samples asked for:{' '}
                                <span className="font-mono text-ink">
                                    {active.id === 'sampled' ? String(natsServerSamples.length) : '0'}
                                </span>
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

function VitalsPanel(): ReactNode {
    const newest = natsServerSamples[natsServerSamples.length - 1];
    const oldest = natsServerSamples[0];
    if (newest === undefined || oldest === undefined) {
        return null;
    }
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">NATS server</h2>
                <span className="text-xs text-ink-faint">newest sample {newest.sampledAt}</span>
            </header>
            <div className="grid gap-x-8 gap-y-3 text-sm sm:grid-cols-4">
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Connections</span>
                    <span className="font-mono">{newest.connections}</span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Memory</span>
                    <span className="font-mono">{asMiB(newest.memBytes)}</span>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Slow consumers</span>
                    <Tag tone={newest.slowConsumers > 0 ? 'warn' : 'neutral'}>
                        {newest.slowConsumers}
                    </Tag>
                </div>
                <div className="space-y-1">
                    <span className="block text-xs text-ink-faint">Totals since start</span>
                    <span className="font-mono">
                        {newest.inMsgs.toLocaleString('en-GB')} in ·{' '}
                        {newest.outMsgs.toLocaleString('en-GB')} out
                    </span>
                </div>
            </div>
            <p className="text-xs text-ink-faint">
                Over {oldest.sampledAt}–{newest.sampledAt}: +
                {(newest.inMsgs - oldest.inMsgs).toLocaleString('en-GB')} messages in, +
                {(newest.outMsgs - oldest.outMsgs).toLocaleString('en-GB')} out,{' '}
                {asMiB(newest.inBytes - oldest.inBytes)} in and{' '}
                {asMiB(newest.outBytes - oldest.outBytes)} out. The totals themselves are running
                totals since the NATS server started.
            </p>
        </section>
    );
}

function StreamsPanel({ empty = false }: { readonly empty?: boolean }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Streams</h2>
                <span className="text-xs text-ink-faint">
                    {empty ? 'no samples in the range' : `${String(natsStreamSamples.length)} streams`}
                </span>
            </header>
            {empty ? (
                <p className="text-sm text-ink-muted">
                    No stream sample in the range. The table draws a stream once a sample names it,
                    so nothing is listed here either.
                </p>
            ) : (
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">Stream</th>
                            <th className="py-1 font-normal">Messages stored</th>
                            <th className="py-1 font-normal">Bytes stored</th>
                            <th className="py-1 font-normal">Consumers</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {natsStreamSamples.map((row) => (
                            <tr key={row.streamName}>
                                <td className="py-2 font-mono">{row.streamName}</td>
                                <td className="py-2 font-mono">{row.messages.toLocaleString('en-GB')}</td>
                                <td className="py-2 font-mono">{asMiB(row.bytes)}</td>
                                <td className="py-2 font-mono">{row.consumerCount}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

function TrendPanel(): ReactNode {
    const points = sparkPoints(natsServerSamples.map((sample) => sample.inMsgs), 640, 80);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Trend</h2>
                <span className="text-xs text-ink-faint">messages in, over the range</span>
            </header>
            <svg viewBox="0 0 640 80" className="h-24 w-full text-accent" role="img">
                <title>Messages in over the range, from the sample series.</title>
                <polyline
                    points={points}
                    fill="none"
                    stroke="currentColor"
                    strokeWidth="2"
                />
            </svg>
            <p className="text-xs text-ink-faint">
                Drawn from the samples the range returns. The screen computes the movement itself:
                the counters are running totals, and no operation sends a rate.
            </p>
        </section>
    );
}

function sparkPoints(values: readonly number[], width: number, height: number): string {
    if (values.length === 0) {
        return '';
    }
    const lowest = Math.min(...values);
    const highest = Math.max(...values);
    const span = highest - lowest === 0 ? 1 : highest - lowest;
    return values
        .map((value, index) => {
            const x = values.length === 1 ? width / 2 : (index / (values.length - 1)) * width;
            const y = height - ((value - lowest) / span) * (height - 8) - 4;
            return `${x.toFixed(1)},${y.toFixed(1)}`;
        })
        .join(' ');
}
