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
 * Three answers to one question: how does a tenant administrator read who is
 * signed in, what they are doing, and who is failing to get in? The journey is
 * doc/knowledge/journeys/credentials/journey_audit_sign_ins.org.
 *
 * The screen is an event log: no versions, no diff, no revert, and Refresh is
 * its only action. The rows are fixtures, because the browser has no read path
 * for the sessions, the login records or the auth events, and nothing serves
 * the session samples on either transport.
 */

import { useState, type ReactNode } from 'react';
import { Button, Detail, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { account, sessions, type PrototypeSession } from './credentialsFixtures.js';

const VARIANTS = [
    {
        id: 'a',
        name: 'A · One log, sections',
        gist: 'One filter bar, then the active sessions, the activity and the failures as sections of one page.',
    },
    {
        id: 'b',
        name: 'B · Tabs over one filter bar',
        gist: 'The three readings share one filter bar and take turns in the body.',
    },
    {
        id: 'c',
        name: 'C · Session-first timeline',
        gist: 'One row per session, expanded to its activity, with the failures in a rail beside it.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

interface ProtectionRow {
    readonly account: string;
    readonly failedAttempts: number;
    readonly lastAddress: string;
    readonly locked: boolean;
}

const FAILURES: readonly ProtectionRow[] = [
    { account: 'jonas.lindqvist', failedAttempts: 7, lastAddress: '198.51.100.7', locked: true },
    { account: 'amara.okafor', failedAttempts: 2, lastAddress: '203.0.113.44', locked: false },
    { account: 'tomas.novak', failedAttempts: 1, lastAddress: '192.0.2.90', locked: false },
];

export function AuditSignInsPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'a');
    const [selectedTab, setSelectedTab] = useState<'sessions' | 'activity' | 'failures'>(
        'sessions',
    );
    const [refreshedAt, setRefreshedAt] = useState('2026-09-30 12:04 UTC');
    const [period, setPeriod] = useState('last 24 hours');
    const [event, setEvent] = useState('any event');
    const [openSession, setOpenSession] = useState<string>(sessions[0]?.id ?? '');
    const [log, setLog] = useState<readonly string[]>([]);

    const record = (entry: string): void => setLog((entries) => [...entries, entry]);
    const refresh = (): void => {
        const stamp = `2026-09-30 12:${String(4 + log.length).padStart(2, '0')} UTC`;
        setRefreshedAt(stamp);
        record(`refresh · re-read every subject at ${stamp}`);
    };

    const header = (
        <PageHeader
            title="Audit: sign-ins"
            description="Who is signed in, what they are doing, and who is failing to get in."
            actions={
                <div className="flex items-center gap-3">
                    <span className="text-xs text-ink-faint">Read at {refreshedAt}</span>
                    <Button variant="secondary" onClick={refresh}>
                        Refresh
                    </Button>
                </div>
            }
        />
    );

    const filters = (
        <section className="card flex flex-wrap items-end gap-4 p-4">
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Account
                <Select disabled value="all" onChange={() => undefined}>
                    <option value="all">
                        Every account (the server cannot filter by account yet)
                    </option>
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Period
                <Select value={period} onChange={(e) => setPeriod(e.target.value)}>
                    <option>last hour</option>
                    <option>last 24 hours</option>
                    <option>last 7 days</option>
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Event
                <Select value={event} onChange={(e) => setEvent(e.target.value)}>
                    <option>any event</option>
                    <option>login</option>
                    <option>login failed</option>
                    <option>logout</option>
                    <option>token refresh</option>
                </Select>
            </label>
            <p className="flex-1 text-xs text-ink-faint">
                No version, diff or revert control appears on this screen: it is an event log, not a
                versioned entity.
            </p>
        </section>
    );

    const sessionsPanel = <SessionsPanel rows={sessions} />;
    const activityPanel = (
        <ActivityPanel row={sessions.find((row) => row.id === openSession) ?? sessions[0]} />
    );
    const failuresPanel = <FailuresPanel />;
    const gapsPanel = <GapsPanel />;

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                {header}
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture. The browser has no read path for the
                    sessions, the login records or the auth events, and nothing serves the session
                    samples, so no panel here reads the server.
                </Notice>

                {active.id === 'a' && (
                    <div className="space-y-6">
                        {filters}
                        {sessionsPanel}
                        {activityPanel}
                        {failuresPanel}
                        {gapsPanel}
                    </div>
                )}

                {active.id === 'b' && (
                    <div className="space-y-6">
                        {filters}
                        <div className="flex gap-1 border-b border-line">
                            {(['sessions', 'activity', 'failures'] as const).map((tab) => (
                                <button
                                    key={tab}
                                    type="button"
                                    className={cx(
                                        '-mb-px border-b-2 px-4 py-2 text-sm capitalize',
                                        selectedTab === tab
                                            ? 'border-accent text-ink'
                                            : 'border-transparent text-ink-muted',
                                    )}
                                    onClick={() => setSelectedTab(tab)}
                                >
                                    {tab}
                                </button>
                            ))}
                        </div>
                        {selectedTab === 'sessions' && sessionsPanel}
                        {selectedTab === 'activity' && activityPanel}
                        {selectedTab === 'failures' && failuresPanel}
                        {gapsPanel}
                    </div>
                )}

                {active.id === 'c' && (
                    <div className="space-y-6">
                        {filters}
                        <div className="grid gap-6 lg:grid-cols-[minmax(0,1fr)_minmax(0,320px)]">
                            <SessionTimeline
                                rows={sessions}
                                open={openSession}
                                onOpen={setOpenSession}
                            />
                            <div className="space-y-6">
                                {failuresPanel}
                                {gapsPanel}
                            </div>
                        </div>
                    </div>
                )}
            </div>

            <VariantBar
                variants={VARIANTS}
                active={active}
                onChoose={choose}
                state={
                    <div className="space-y-2 text-xs">
                        <div className="grid gap-x-6 gap-y-1 text-ink-muted sm:grid-cols-2">
                            <span>
                                variant: <span className="font-mono text-ink">{active.id}</span>
                            </span>
                            <span>
                                tab: <span className="font-mono text-ink">{selectedTab}</span>
                            </span>
                            <span>
                                period: <span className="font-mono text-ink">{period}</span>
                            </span>
                            <span>
                                event: <span className="font-mono text-ink">{event}</span>
                            </span>
                            <span>
                                read at: <span className="font-mono text-ink">{refreshedAt}</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as <span className="font-mono">{account.username}</span>,
                            tenant administrator, tenant Acme Corporation.
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

function SessionsPanel({ rows }: { readonly rows: readonly PrototypeSession[] }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Active sessions</h2>
                <span className="text-xs text-ink-faint">{rows.length} open</span>
            </header>
            <ul className="divide-y divide-line-subtle">
                {rows.map((row) => (
                    <li key={row.id} className="flex flex-wrap items-center gap-x-4 gap-y-1 py-3">
                        <span className="w-28 shrink-0 font-mono text-sm">{row.client}</span>
                        <span className="min-w-0 flex-1">
                            <span className="block font-mono text-sm">{row.address}</span>
                            <span className="block text-xs text-ink-muted">
                                {row.country} · started {row.startedAt} · {row.duration}
                            </span>
                        </span>
                        <span className="hidden text-right text-xs text-ink-faint sm:block">
                            {row.bytesIn} in / {row.bytesOut} out
                        </span>
                        <Button
                            size="sm"
                            variant="secondary"
                            disabled
                            title="Ending another account's session has no subject yet: iam.v1.sessions.end does not exist."
                        >
                            End session
                        </Button>
                    </li>
                ))}
            </ul>
            <p className="text-xs text-ink-faint">
                End session is drawn unavailable: the repository writes an end time for the caller's
                own logout only, and iam.v1.sessions.delete needs the iam::* wildcard.
            </p>
        </section>
    );
}

function ActivityPanel({ row }: { readonly row: PrototypeSession | undefined }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Session activity</h2>
                <p className="text-sm text-ink-muted">
                    How the byte totals moved while the session was open.
                </p>
            </header>
            {row === undefined ? (
                <Notice tone="warn">No session is selected.</Notice>
            ) : (
                <>
                    <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                        <Detail label="Session" value={row.client} mono />
                        <Detail label="Address" value={row.address} mono />
                        <Detail label="Started" value={row.startedAt} />
                    </div>
                    <Notice tone="warn">
                        Not available. Nothing serves the samples: iam.v1.sessions.samples replies
                        with success and no rows, and no route serves them in the browser either.
                        The totals above are the session row's own counters.
                    </Notice>
                </>
            )}
        </section>
    );
}

function FailuresPanel(): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Failed attempts</h2>
                <p className="text-sm text-ink-muted">
                    A locked account explains a support call; a burst explains an incident.
                </p>
            </header>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 font-normal">Account</th>
                        <th className="py-1 font-normal">Failed</th>
                        <th className="py-1 font-normal">Last address</th>
                        <th className="py-1 font-normal">State</th>
                    </tr>
                </thead>
                <tbody className="divide-y divide-line-subtle">
                    {FAILURES.map((row) => (
                        <tr key={row.account}>
                            <td className="py-2 font-mono">{row.account}</td>
                            <td className="py-2 font-mono">{row.failedAttempts}</td>
                            <td className="py-2 font-mono">{row.lastAddress}</td>
                            <td className="py-2">
                                <Tag tone={row.locked ? 'warn' : 'muted'}>
                                    {row.locked ? 'Locked' : 'Not locked'}
                                </Tag>
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">
                iam.v1.login_info.list exists and no route serves it, so this panel cannot read the
                table today.
            </p>
        </section>
    );
}

function SessionTimeline({
    rows,
    open,
    onOpen,
}: {
    readonly rows: readonly PrototypeSession[];
    readonly open: string;
    readonly onOpen: (id: string) => void;
}): ReactNode {
    return (
        <section className="card divide-y divide-line-subtle p-6">
            <h2 className="pb-3 text-lg font-medium">Sessions and their activity</h2>
            {rows.map((row) => {
                const expanded = row.id === open;
                return (
                    <div key={row.id} className="py-3">
                        <button
                            type="button"
                            className="flex w-full flex-wrap items-center gap-x-4 gap-y-1 text-left"
                            onClick={() => onOpen(row.id)}
                        >
                            <span className="w-24 shrink-0 font-mono text-sm">{row.client}</span>
                            <span className="min-w-0 flex-1">
                                <span className="block font-mono text-sm">{row.address}</span>
                                <span className="block text-xs text-ink-muted">
                                    {row.country} · {row.startedAt} · {row.duration}
                                </span>
                            </span>
                            <span className="text-xs text-ink-faint">
                                {expanded ? 'Hide' : 'Activity'}
                            </span>
                        </button>
                        {expanded && (
                            <div className="mt-3 rounded-[var(--radius-card)] border border-line-subtle p-3 text-xs text-ink-muted">
                                {row.bytesIn} in and {row.bytesOut} out over {row.duration}. The
                                samples that moved those totals have no read path: nothing serves
                                iam.v1.sessions.samples.
                            </div>
                        )}
                    </div>
                );
            })}
        </section>
    );
}

function GapsPanel(): ReactNode {
    return (
        <section className="card space-y-3 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Not available in this build</h2>
                <p className="text-sm text-ink-muted">
                    The readings this journey asks for and the server does not serve.
                </p>
            </header>
            <ul className="space-y-2 text-sm">
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        The authentication events —{' '}
                        <span className="font-mono">ores_iam_auth_events_tbl</span> holds them;
                        candidate <span className="font-mono">iam.v1.auth_events.list</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        The session statistics — three continuous aggregates exist; candidate{' '}
                        <span className="font-mono">iam.v1.sessions.statistics</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Ending another account's session — candidate{' '}
                        <span className="font-mono">iam.v1.sessions.end</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Every read on this screen — no route serves the sessions, the samples or the
                        login records
                    </span>
                </li>
            </ul>
        </section>
    );
}
