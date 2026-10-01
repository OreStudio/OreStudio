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

import { useCallback, useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { api } from '../api/client.js';
import { Button, Detail, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import type { LoginInfo, Session } from '@ores/wire-protocol/browser';

/**
 * Audit: sign-ins, the tenant administrator's screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_audit_sign_ins.org`, and the
 * screen is the accepted prototype's variant B: the active sessions, the
 * session activity and the failed attempts take turns in the body under one
 * filter bar, with *Refresh* as the only action.
 *
 * The screen is an event log, so it carries no version, no diff and no revert.
 * Every reading the server cannot serve says so: the account filter is drawn
 * unavailable because the read takes no account filter, the session activity
 * has no samples to draw, and the authentication events have no subject at all.
 */

type Tab = 'sessions' | 'activity' | 'failures';

const TABS: readonly Tab[] = ['sessions', 'activity', 'failures'];

/**
 * The periods the filter offers.
 *
 * The server takes no time bound on any of these reads, so the filter is
 * applied in the browser to the page that was read, and the screen says so
 * rather than implying the server narrowed it.
 */
const PERIODS = [
    { id: 'hour', label: 'last hour', seconds: 3600 },
    { id: 'day', label: 'last 24 hours', seconds: 86400 },
    { id: 'week', label: 'last 7 days', seconds: 604800 },
    { id: 'all', label: 'everything read', seconds: 0 },
] as const;

type PeriodId = (typeof PERIODS)[number]['id'];

/** How many rows the reads ask for, which is what bounds every panel. */
const PAGE_SIZE = 100;

interface Loaded {
    readonly active: readonly Session[];
    readonly sessionCount: number;
    readonly loginInfo: readonly LoginInfo[];
    readonly loginInfoCount: number;
    readonly readAt: string;
}

type State =
    | { readonly kind: 'loading' }
    | { readonly kind: 'ready'; readonly loaded: Loaded }
    | { readonly kind: 'failed'; readonly reason: string };

export function AuditPage(): ReactNode {
    const [state, setState] = useState<State>({ kind: 'loading' });

    const load = useCallback(async (): Promise<void> => {
        try {
            const [page, active, loginInfo] = await Promise.all([
                api.sessions(),
                api.activeSessions(),
                api.loginInfoPage(),
            ]);
            setState({
                kind: 'ready',
                loaded: {
                    active,
                    sessionCount: page.totalCount,
                    loginInfo: loginInfo.loginInfo,
                    loginInfoCount: loginInfo.totalCount,
                    readAt: new Date().toISOString().replace('T', ' ').slice(0, 19) + ' UTC',
                },
            });
        } catch (error) {
            setState({
                kind: 'failed',
                reason: error instanceof Error ? error.message : 'The read failed.',
            });
        }
    }, []);

    useEffect(() => {
        void load();
    }, [load]);

    return (
        <div className="mx-auto max-w-[1200px] space-y-6">
            <PageHeader
                title="Audit: sign-ins"
                description="Who is signed in, what they are doing, and who is failing to get in."
                actions={
                    state.kind === 'ready' ? (
                        <div className="flex items-center gap-3">
                            <span className="text-xs text-ink-faint">
                                Read at {state.loaded.readAt}
                            </span>
                            <Button variant="secondary" onClick={() => void load()}>
                                Refresh
                            </Button>
                        </div>
                    ) : undefined
                }
            />
            {state.kind === 'loading' && (
                <Notice tone="info">Reading the tenant&rsquo;s sessions…</Notice>
            )}
            {state.kind === 'failed' && <Notice tone="error">{state.reason}</Notice>}
            {state.kind === 'ready' && <AuditView loaded={state.loaded} />}
        </div>
    );
}

/**
 * Variant B: one filter bar, and the three readings taking turns under it.
 *
 * The reading in view is a query parameter, so a review can link to the one it
 * is about: `/audit?tab=failures` opens the failed attempts directly.
 */
export function AuditView({ loaded }: { readonly loaded: Loaded }): ReactNode {
    const [params, setParams] = useSearchParams();
    const tab = asTab(params.get('tab'));
    const [period, setPeriod] = useState<PeriodId>('all');
    const [selected, setSelected] = useState<string>('');

    const show = (next: Tab): void => {
        const query = new URLSearchParams(params);
        query.set('tab', next);
        setParams(query);
    };

    const sessions = useMemo(
        () => loaded.active.filter((row) => withinPeriod(row.startTime, period)),
        [loaded.active, period],
    );
    const chosen = sessions.find((row) => row.id === selected) ?? sessions[0];

    return (
        <div className="space-y-6">
            <FilterBar period={period} onPeriod={setPeriod} />
            <div className="flex gap-1 border-b border-line">
                {TABS.map((name) => (
                    <button
                        key={name}
                        type="button"
                        className={cx(
                            '-mb-px border-b-2 px-4 py-2 text-sm capitalize',
                            tab === name
                                ? 'border-accent text-ink'
                                : 'border-transparent text-ink-muted',
                        )}
                        onClick={() => show(name)}
                    >
                        {name}
                    </button>
                ))}
            </div>
            {tab === 'sessions' && (
                <SessionsPanel
                    rows={sessions}
                    openCount={loaded.active.length}
                    totalCount={loaded.sessionCount}
                />
            )}
            {tab === 'activity' && (
                <ActivityPanel rows={sessions} chosen={chosen} onChoose={setSelected} />
            )}
            {tab === 'failures' && (
                <FailuresPanel rows={loaded.loginInfo} totalCount={loaded.loginInfoCount} />
            )}
            <GapsPanel />
        </div>
    );
}

/**
 * The filter bar.
 *
 * Two of the three controls the journey names cannot work, and the bar says why
 * rather than offering a control that silently does nothing. The period is
 * applied in the browser, over the page that was read.
 */
function FilterBar({
    period,
    onPeriod,
}: {
    readonly period: PeriodId;
    readonly onPeriod: (next: PeriodId) => void;
}): ReactNode {
    return (
        <section className="card flex flex-wrap items-end gap-4 p-4">
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Account
                <Select disabled value="all" onChange={() => undefined}>
                    <option value="all">Every account (the read takes no account filter)</option>
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Period
                <Select
                    value={period}
                    onChange={(event) => onPeriod(event.target.value as PeriodId)}
                >
                    {PERIODS.map((option) => (
                        <option key={option.id} value={option.id}>
                            {option.label}
                        </option>
                    ))}
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                Event
                <Select disabled value="any" onChange={() => undefined}>
                    <option value="any">Any event (no subject carries them)</option>
                </Select>
            </label>
            <p className="flex-1 text-xs text-ink-faint">
                The period is applied in this browser to the page that was read; the server takes no
                time bound. No version, diff or revert control appears here: this screen is an event
                log, not a versioned entity.
            </p>
        </section>
    );
}

/**
 * The sessions with no end time.
 *
 * The panel compares what the active read answered with what the session page
 * holds, because the two would differ the day something writes an end time and
 * do not today.
 */
function SessionsPanel({
    rows,
    openCount,
    totalCount,
}: {
    readonly rows: readonly Session[];
    readonly openCount: number;
    readonly totalCount: number;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Active sessions</h2>
                <span className="text-xs text-ink-faint">
                    {String(rows.length)} shown of {String(openCount)} open
                </span>
            </header>
            <Notice tone="warn">
                Nothing ends a session. No sign-out writes an end time, so every session this
                deployment has created has none, all {String(openCount)} read as open, and an old
                one cannot be told from a live one. Ending another account&rsquo;s session has no
                operation either, which is why no row offers it.
            </Notice>
            {rows.length === 0 ? (
                <p className="text-sm text-ink-muted">No session in this period has no end time.</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {rows.map((row) => (
                        <li
                            key={row.id}
                            className="flex flex-wrap items-center gap-x-4 gap-y-1 py-3"
                        >
                            <span className="w-32 shrink-0 font-mono text-sm">
                                {row.clientIdentifier === ''
                                    ? 'unknown client'
                                    : row.clientIdentifier}
                            </span>
                            <span className="min-w-0 flex-1">
                                <span className="block font-mono text-sm">
                                    {row.clientIp === '' ? 'no address' : row.clientIp}
                                </span>
                                <span className="block text-xs text-ink-muted">
                                    {row.countryCode === '' ? 'unknown country' : row.countryCode} ·
                                    started {row.startTime} · account{' '}
                                    <span className="font-mono">{row.accountId}</span>
                                </span>
                            </span>
                            <span className="hidden text-right text-xs text-ink-faint sm:block">
                                {String(row.bytesSent)} sent / {String(row.bytesReceived)} received
                            </span>
                        </li>
                    ))}
                </ul>
            )}
            <div className="flex flex-wrap items-center justify-between gap-2">
                <p className="text-xs text-ink-faint">
                    The session page holds {String(totalCount)} rows and the read returns one page
                    of {String(PAGE_SIZE)}, so a large tenant is short here.
                </p>
            </div>
        </section>
    );
}

/**
 * One session's activity.
 *
 * The journey asks for the byte counters as a time series of samples. Nothing
 * serves them on either transport, so the panel shows the session row's own
 * running totals and states that the series behind them has no read path. No
 * empty chart is drawn, because an empty chart claims the tenant moved nothing.
 */
function ActivityPanel({
    rows,
    chosen,
    onChoose,
}: {
    readonly rows: readonly Session[];
    readonly chosen: Session | undefined;
    readonly onChoose: (id: string) => void;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Session activity</h2>
                <p className="text-sm text-ink-muted">
                    How the byte totals moved while the session was open.
                </p>
            </header>
            {chosen === undefined ? (
                <Notice tone="warn">No session is in the period that was read.</Notice>
            ) : (
                <>
                    <label className="flex max-w-md flex-col gap-1 text-xs text-ink-muted">
                        Session
                        <Select
                            value={chosen.id}
                            onChange={(event) => onChoose(event.target.value)}
                        >
                            {rows.map((row) => (
                                <option key={row.id} value={row.id}>
                                    {row.clientIdentifier === ''
                                        ? 'unknown client'
                                        : row.clientIdentifier}
                                    {' · '}
                                    {row.clientIp === '' ? 'no address' : row.clientIp}
                                    {' · '}
                                    {row.startTime}
                                </option>
                            ))}
                        </Select>
                    </label>
                    <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                        <Detail label="Started" value={chosen.startTime} />
                        <Detail label="Bytes sent" value={String(chosen.bytesSent)} mono />
                        <Detail label="Bytes received" value={String(chosen.bytesReceived)} mono />
                    </div>
                    <Notice tone="warn">
                        Not available. Nothing serves the samples: the subject behind them is a stub
                        that answers with no rows, and nothing writes a sample row, so there is no
                        series to draw. The totals above are the session row&rsquo;s own counters.
                    </Notice>
                </>
            )}
        </section>
    );
}

/**
 * The failed attempts, and the lock state beside them.
 *
 * A locked account explains a support call and a run of attempts explains an
 * incident, so the count and the state travel together.
 */
function FailuresPanel({
    rows,
    totalCount,
}: {
    readonly rows: readonly LoginInfo[];
    readonly totalCount: number;
}): ReactNode {
    const ordered = useMemo(
        () => [...rows].sort((left, right) => right.failedLogins - left.failedLogins),
        [rows],
    );

    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Failed attempts</h2>
                <span className="text-xs text-ink-faint">
                    {String(rows.length)} shown of {String(totalCount)} records
                </span>
            </header>
            {ordered.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    The tenant holds no login record, which means nobody has tried to sign in.
                </p>
            ) : (
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">Account</th>
                            <th className="py-1 font-normal">Failed</th>
                            <th className="py-1 font-normal">Last address</th>
                            <th className="py-1 font-normal">Last sign-in</th>
                            <th className="py-1 font-normal">State</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {ordered.map((row) => (
                            <tr key={row.accountId}>
                                <td className="py-2 font-mono">{row.accountId}</td>
                                <td className="py-2 font-mono">{String(row.failedLogins)}</td>
                                <td className="py-2 font-mono">
                                    {row.lastAttemptIp === '' ? 'unknown' : row.lastAttemptIp}
                                </td>
                                <td className="py-2">
                                    {row.lastLogin === '' ? 'never' : row.lastLogin}
                                </td>
                                <td className="py-2">
                                    <Tag tone={row.locked ? 'warn' : 'muted'}>
                                        {row.locked ? 'Locked' : 'Not locked'}
                                    </Tag>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
            <p className="text-xs text-ink-faint">
                The period filter does not apply here: a login record carries one last sign-in and
                no attempt times, so no time bound can narrow it. Unlocking an account is{' '}
                <Link className="underline" to="/rescue">
                    Rescue access
                </Link>
                &rsquo;s job.
            </p>
        </section>
    );
}

/** What this journey asks for and the server does not serve. */
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
                        The authentication events — the table exists and no subject reads it
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>The session statistics — the continuous aggregates have no subject</span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        End another account&rsquo;s session — the repository writes an end time, and
                        only the caller&rsquo;s own sign-out calls it
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Filter by account — the session read pages by offset and limit and takes no
                        account filter
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        The per-session sample series — nothing writes a sample row, so the subject
                        behind it answers with none
                    </span>
                </li>
            </ul>
        </section>
    );
}

/** The tab a query parameter names, or the sessions reading when it names none. */
function asTab(value: string | null): Tab {
    return TABS.find((name) => name === value) ?? 'sessions';
}

/** Whether a wire timestamp falls inside the chosen period. */
function withinPeriod(timestamp: string, period: PeriodId): boolean {
    const span = PERIODS.find((option) => option.id === period)?.seconds ?? 0;
    if (span === 0) {
        return true;
    }
    const at = Date.parse(timestamp.replace(' ', 'T'));
    if (Number.isNaN(at)) {
        return true;
    }
    return Date.now() - at <= span * 1000;
}
