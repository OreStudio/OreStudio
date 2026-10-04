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
 * Read the telemetry logs, from
 * doc/knowledge/journeys/operations/journey_read_the_telemetry_logs.org.
 * The rows are fixtures shaped by the telemetry.v1.logs.list reply; the
 * filters combine the way the query combines them, with AND.
 */

import { useState, type ReactNode } from 'react';
import { Button, Input, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { logEntries, type PrototypeLogEntry } from './fixtures.js';

const VARIANTS = [
    {
        id: 'matches',
        name: 'Matches',
        gist: 'The last hour of server lines, filtered by the bar above the table.',
    },
    {
        id: 'nothing',
        name: 'Nothing matches',
        gist: 'The filter in the range returns no entry; the screen says which filter to drop.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const TOTAL_COUNT = 2431;

const GAPS: readonly ScreenGap[] = [
    {
        title: 'The store holds no client lines',
        body: 'The entry model has a client source and the ingest stamps every stored entry source=server; nothing publishes a client line. Choosing the client source returns nothing, forever.',
    },
    {
        title: 'The filters combine with AND only',
        body: 'Every filter narrows the same set; there is no way to ask for one component or another, and nothing suggests the values — component and tag are typed blind.',
    },
    {
        title: 'The message filter reaches the database as text',
        body: 'The match runs as SQL text behind a hand-written escape; the journey records the defect on capture BBA0A093.',
    },
    {
        title: 'The statistics have no subject',
        body: 'The hourly, daily and per-session aggregates are stored and no subject reads them, so the screen cannot draw a count over time.',
    },
    {
        title: 'No permission gates the read',
        body: 'The handler authenticates the caller and checks nothing else.',
    },
];

export function LogsPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'matches');
    const [readAt, setReadAt] = useState('14:32:12');
    const [level, setLevel] = useState('Any level');
    const [source, setSource] = useState('Any source');
    const [componentDraft, setComponentDraft] = useState('');
    const [tagDraft, setTagDraft] = useState('');
    const [messageDraft, setMessageDraft] = useState('');
    const [applied, setApplied] = useState({ component: '', tag: '', message: '' });
    const [log, setLog] = useState<readonly string[]>([]);

    const search = (): void => {
        setApplied({ component: componentDraft, tag: tagDraft, message: messageDraft });
        setReadAt('14:33:08');
        setLog((entries) => [
            ...entries,
            `search · level "${level}", source "${source}", component "${componentDraft}", tag "${tagDraft}", message "${messageDraft}"`,
        ]);
    };

    const rows =
        active.id === 'nothing'
            ? []
            : logEntries.filter(
                  (entry) =>
                      matchesLevel(entry, level) &&
                      matchesSource(entry, source) &&
                      includes(entry.component, applied.component) &&
                      includes(entry.tag, applied.tag) &&
                      includes(entry.message, applied.message),
              );

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Operations: telemetry logs"
                    description="The lines behind a symptom, found by time, level, source, component, tag or session."
                    actions={
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">Read at {readAt}</span>
                            <OperationsBack />
                        </div>
                    }
                />

                <section className="card flex flex-wrap items-end gap-4 p-4">
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        Range
                        <Select value="Last hour" onChange={() => undefined}>
                            <option>Last hour</option>
                        </Select>
                    </label>
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        Level
                        <Select value={level} onChange={(e) => setLevel(e.target.value)}>
                            <option>Any level</option>
                            <option>ERROR</option>
                            <option>WARN</option>
                            <option>INFO</option>
                            <option>DEBUG</option>
                        </Select>
                    </label>
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        Source
                        <Select value={source} onChange={(e) => setSource(e.target.value)}>
                            <option>Any source</option>
                            <option>server</option>
                            <option>client</option>
                        </Select>
                    </label>
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        Component
                        <Input
                            value={componentDraft}
                            placeholder="ores.compute.poller"
                            onChange={(e) => setComponentDraft(e.target.value)}
                        />
                    </label>
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        Tag
                        <Input
                            value={tagDraft}
                            placeholder="compute.fetch"
                            onChange={(e) => setTagDraft(e.target.value)}
                        />
                    </label>
                    <label className="flex flex-1 flex-col gap-1 text-xs text-ink-muted">
                        Message
                        <Input
                            value={messageDraft}
                            placeholder="Search the message"
                            onChange={(e) => setMessageDraft(e.target.value)}
                        />
                    </label>
                    <Button variant="secondary" onClick={search}>
                        Search
                    </Button>
                </section>

                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture shaped by the telemetry.v1.logs.list
                    reply. Nothing on this page reads the server. The filters combine with AND, as
                    the query does.
                </Notice>

                {rows.length === 0 ? (
                    <EmptyPanel source={source} />
                ) : (
                    <EntriesPanel rows={rows} />
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
                                level: <span className="font-mono text-ink">{level}</span>
                            </span>
                            <span>
                                source: <span className="font-mono text-ink">{source}</span>
                            </span>
                            <span>
                                component: <span className="font-mono text-ink">{applied.component === '' ? '—' : applied.component}</span>
                            </span>
                            <span>
                                tag: <span className="font-mono text-ink">{applied.tag === '' ? '—' : applied.tag}</span>
                            </span>
                            <span>
                                message: <span className="font-mono text-ink">{applied.message === '' ? '—' : applied.message}</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as system administrator, tenant Acme Corporation. The read
                            takes limit and offset; the fixture holds one page.
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

function EntriesPanel({ rows }: { readonly rows: readonly PrototypeLogEntry[] }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Entries</h2>
                <span className="text-xs text-ink-faint">{rows.length} of {TOTAL_COUNT}</span>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">Time</th>
                        <th className="py-1 font-normal">Level</th>
                        <th className="py-1 font-normal">Source</th>
                        <th className="py-1 font-normal">Name</th>
                        <th className="py-1 font-normal">Component</th>
                        <th className="py-1 font-normal">Message</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {rows.map((entry) => (
                        <tr key={entry.id}>
                            <td className="py-2 font-mono">{entry.time}</td>
                            <td className="py-2">
                                <Tag tone={entry.level === 'ERROR' || entry.level === 'WARN' ? 'warn' : entry.level === 'DEBUG' ? 'muted' : 'neutral'}>
                                    {entry.level}
                                </Tag>
                            </td>
                            <td className="py-2 font-mono">{entry.source}</td>
                            <td className="py-2 font-mono">{entry.sourceName}</td>
                            <td className="py-2 font-mono">{entry.component}</td>
                            <td className="py-2">{entry.message}</td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <div className="flex flex-wrap items-center gap-3">
                <span className="text-xs text-ink-faint">
                    Showing 1–{rows.length} of {TOTAL_COUNT} entries
                </span>
                <Button size="sm" variant="secondary" disabled>
                    Previous
                </Button>
                <Button size="sm" variant="secondary" disabled>
                    Next
                </Button>
                <span className="text-xs text-ink-faint">
                    The reply carries limit and offset; the fixture holds one page.
                </span>
            </div>
        </section>
    );
}

function EmptyPanel({ source }: { readonly source: string }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Entries</h2>
                <Tag tone="warn">Nothing matches</Tag>
            </header>
            <p className="text-sm text-ink-muted">
                No entry matches the filter in this range. Widen the range or drop a filter.
            </p>
            <p className="text-xs text-ink-faint">
                {source === 'client'
                    ? 'No entry has the source client: the store holds server lines today.'
                    : 'Filters combine with AND; the store holds server lines today, so no entry has the source client.'}
            </p>
        </section>
    );
}

function matchesLevel(entry: PrototypeLogEntry, level: string): boolean {
    return level === 'Any level' || entry.level === level;
}

function matchesSource(entry: PrototypeLogEntry, source: string): boolean {
    return source === 'Any source' || entry.source === source;
}

function includes(value: string, filter: string): boolean {
    return filter === '' || value.includes(filter);
}
