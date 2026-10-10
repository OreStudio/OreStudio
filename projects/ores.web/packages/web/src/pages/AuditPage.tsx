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

import { useState, type ReactNode } from 'react';
import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import { isWireTimestamp } from '@ores/wire-protocol/browser';
import type {
    AuthEvent,
    LoginInfo,
    Session,
    SessionStatisticsRow,
} from '@ores/wire-protocol/browser';
import { api, type AuditPeriod } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ApiFailure } from '../api/transport.js';
import { Crumbs } from '../refdata/shared.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { isZeroTimestamp } from '../ui/Time.js';

/**
 * Audit: sign-ins, the tenant administrator's screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_audit_sign_ins.org`, and the
 * screen is the accepted prototype's variant B: the readings take turns in the
 * body under one filter bar, with *Refresh* as the only page action and *End
 * session* on an open session's row.
 *
 * The screen is an event log, so it carries no version, no diff and no revert.
 * Every reading the server cannot serve says so in its own panel: the session
 * activity has no samples to draw, and nothing writes one. A refused read is
 * stated in its panel too, because one reading may be withheld while the rest
 * of the screen stands.
 */

type AuditTab = 'sessions' | 'activity' | 'statistics' | 'failures' | 'events';

/** The readings of the screen, in the order the tabs offer them. */
export const AUDIT_TABS: readonly AuditTab[] = [
    'sessions',
    'activity',
    'statistics',
    'failures',
    'events',
];

/** The period presets the filter offers, mapped to the BFF's window. */
export const AUDIT_PERIODS: readonly AuditPeriod[] = ['hour', 'day', 'week', 'all'];

/**
 * The event types the filter offers, in the store's own words.
 *
 * The read's event filter is an equality, so the values offered must be the
 * words the log stores; the prototype's human spellings would match nothing.
 */
export const AUDIT_EVENT_TYPES = [
    'login_success',
    'login_failure',
    'logout',
    'token_refresh',
] as const;

/** The filter the screen opens on: the last day, with every event. */
export interface AuditFilter {
    readonly period: AuditPeriod;
    readonly eventType: string;
}

export const DEFAULT_AUDIT_FILTER: AuditFilter = { period: 'day', eventType: '' };

/** How many rows the reads ask for, which is what bounds every table. */
export const AUDIT_PAGE_SIZE = 100;

/** Where each reading is cached, by the filter it read under. */
export const AUDIT_SESSIONS_QUERY_KEY = 'audit-active-sessions' as const;
export const AUDIT_FAILURES_QUERY_KEY = 'audit-login-records' as const;
export const AUDIT_EVENTS_QUERY_KEY = 'audit-auth-events' as const;
export const AUDIT_STATISTICS_QUERY_KEY = 'audit-session-statistics' as const;
export const AUDIT_ACCOUNTS_QUERY_KEY = 'audit-account-names' as const;

/**
 * The seconds one period names, for the sessions read.
 *
 * The active-sessions read takes no time bound, so the period is applied in
 * this browser to the rows it answered. The event and statistics reads take the
 * period as a preset, and the BFF turns it into a window on the deployment's
 * clock.
 */
const PERIOD_SECONDS: Readonly<Record<AuditPeriod, number>> = {
    hour: 3600,
    day: 86400,
    week: 604800,
    all: 0,
};

/** Whether a wire start time falls inside the chosen period. */
export function withinPeriod(startTime: string, period: AuditPeriod): boolean {
    const span = PERIOD_SECONDS[period];
    if (span === 0) {
        return true;
    }
    if (!isWireTimestamp(startTime)) {
        return true;
    }
    const at = Date.parse(startTime.replace(' ', 'T'));
    if (Number.isNaN(at)) {
        return true;
    }
    return Date.now() - at <= span * 1000;
}

/** The moment a reading was taken, as a wall clock a person reads, in UTC. */
export function formatReadAt(at: number): string | undefined {
    if (at === 0) {
        return undefined;
    }
    return `${new Date(at).toISOString().slice(11, 19)} UTC`;
}

/**
 * Whether a wire address names no address.
 *
 * An account that has never been tried carries the wire's zero address,
 * `0.0.0.0`, which is a placeholder rather than somewhere a person signed in.
 * An empty value names no address either.
 */
export function isZeroAddress(value: string): boolean {
    return value === '' || value === '0.0.0.0';
}

/**
 * The login records in the order the panel reads them.
 *
 * A login record is per-account state rather than an event, so the read
 * answers one for every account. The rows that have something to say come
 * first: the most failed, then the accounts that have signed in at least once,
 * then the accounts that have never been used.
 */
export function orderLoginRecords(rows: readonly LoginInfo[]): readonly LoginInfo[] {
    return [...rows].sort((left, right) => {
        if (left.failedLogins !== right.failedLogins) {
            return right.failedLogins - left.failedLogins;
        }
        return Number(isZeroTimestamp(left.lastLogin)) - Number(isZeroTimestamp(right.lastLogin));
    });
}

/**
 * An account as the audit names it.
 *
 * The username is the first line because a person reads a name, not a key; the
 * identifier sits under it for the support call that quotes it. An account the
 * read did not answer keeps its identifier alone.
 */
function AccountCell({
    accountId,
    username,
    nameOf,
}: {
    readonly accountId: string;
    readonly username?: string;
    readonly nameOf: ReadonlyMap<string, string>;
}): ReactNode {
    if (accountId === '') {
        return <span>{username === undefined || username === '' ? '—' : username}</span>;
    }
    const name =
        username !== undefined && username !== '' ? username : (nameOf.get(accountId) ?? '');
    return (
        <div className="space-y-0.5">
            {name !== '' && <div>{name}</div>}
            <div className="font-mono text-xs text-ink-faint">{accountId}</div>
        </div>
    );
}

/** The tone one event type is painted with, for the types the log stores. */
export function eventTone(eventType: string): 'neutral' | 'warn' | 'muted' | 'up' {
    if (eventType === 'login_success') {
        return 'up';
    }
    if (eventType === 'login_failure' || eventType === 'signup_failure') {
        return 'warn';
    }
    if (eventType === 'logout') {
        return 'muted';
    }
    return 'neutral';
}

/**
 * The reason a panel cannot show its reading.
 *
 * A refusal is stated as the refusal it is, because signing in again changes
 * nothing: the session is real and the permission is not held.
 */
function failureReason(error: Error, refused: string): string {
    if (error instanceof ApiFailure && error.status === 403) {
        return refused;
    }
    return error.message;
}

/** The filter bar: the prototype's controls, in the prototype's order. */
function FilterBar({
    filter,
    onChange,
}: {
    readonly filter: AuditFilter;
    readonly onChange: (next: AuditFilter) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card flex flex-wrap items-end gap-4 p-4">
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('auditSignIns.account.label')}
                <Select disabled value="all" onChange={() => undefined}>
                    <option value="all">{t('auditSignIns.account.every')}</option>
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('auditSignIns.period.label')}
                <Select
                    value={filter.period}
                    onChange={(event) =>
                        onChange({ ...filter, period: event.target.value as AuditPeriod })
                    }
                >
                    {AUDIT_PERIODS.map((option) => (
                        <option key={option} value={option}>
                            {t(`auditSignIns.period.${option}`)}
                        </option>
                    ))}
                </Select>
            </label>
            <label className="flex flex-col gap-1 text-xs text-ink-muted">
                {t('auditSignIns.event.label')}
                <Select
                    value={filter.eventType}
                    onChange={(event) => onChange({ ...filter, eventType: event.target.value })}
                >
                    <option value="">{t('auditSignIns.event.any')}</option>
                    {AUDIT_EVENT_TYPES.map((option) => (
                        <option key={option} value={option}>
                            {t(`auditSignIns.event.${option}`)}
                        </option>
                    ))}
                </Select>
            </label>
            <p className="flex-1 text-xs text-ink-faint">{t('auditSignIns.filterNote')}</p>
        </section>
    );
}

/**
 * The sessions with no end time.
 *
 * The period narrows this panel in the browser, because the active-sessions
 * read takes no time bound. Each open row offers the one write the screen has:
 * ending that session, which re-reads the panel.
 */
function SessionsPanel({
    sessions,
    accountNames,
    pending,
    error,
    endingId,
    endedId,
    endError,
    onEnd,
}: {
    readonly sessions: readonly Session[];
    readonly accountNames: ReadonlyMap<string, string>;
    readonly pending: boolean;
    readonly error: Error | null;
    readonly endingId: string;
    readonly endedId: string;
    readonly endError: string;
    readonly onEnd: (session: Session) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('auditSignIns.sessions.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('auditSignIns.sessions.count', { count: sessions.length })}
                </span>
            </header>
            {pending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : error !== null ? (
                <Notice tone="error">
                    {failureReason(error, t('auditSignIns.sessions.notAllowed'))}
                </Notice>
            ) : (
                <>
                    {endedId !== '' && (
                        <Notice tone="success">{t('auditSignIns.sessions.ended')}</Notice>
                    )}
                    {endError !== '' && <Notice tone="error">{endError}</Notice>}
                    {sessions.length === 0 ? (
                        <p className="text-sm text-ink-muted">{t('auditSignIns.sessions.empty')}</p>
                    ) : (
                        <>
                            <table className="w-full text-left text-sm">
                                <thead className="text-left text-xs text-ink-faint">
                                    <tr>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.started')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.client')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.address')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.country')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.account')}
                                        </th>
                                        <th className="py-1 font-normal">
                                            {t('auditSignIns.column.traffic')}
                                        </th>
                                        <th className="py-1 font-normal" />
                                    </tr>
                                </thead>
                                <tbody className="divide-y divide-line-subtle">
                                    {sessions.map((row) => (
                                        <tr key={row.id}>
                                            <td className="whitespace-nowrap py-2 font-mono text-xs">
                                                {row.startTime}
                                            </td>
                                            <td className="py-2 font-mono text-xs">
                                                {row.clientIdentifier === ''
                                                    ? '—'
                                                    : row.clientIdentifier}
                                            </td>
                                            <td className="py-2 font-mono text-xs">
                                                {row.clientIp === '' ? '—' : row.clientIp}
                                            </td>
                                            <td className="py-2 text-ink-muted">
                                                {row.countryCode === '' ? '—' : row.countryCode}
                                            </td>
                                            <td className="py-2">
                                                <AccountCell
                                                    accountId={row.accountId}
                                                    nameOf={accountNames}
                                                />
                                            </td>
                                            <td className="whitespace-nowrap py-2 text-right text-xs text-ink-faint">
                                                {String(row.bytesSent)} /{' '}
                                                {String(row.bytesReceived)}
                                            </td>
                                            <td className="py-2 text-right">
                                                <Button
                                                    size="sm"
                                                    variant="secondary"
                                                    disabled={endingId !== ''}
                                                    onClick={() => onEnd(row)}
                                                >
                                                    {endingId === row.id
                                                        ? t('auditSignIns.sessions.ending')
                                                        : t('auditSignIns.sessions.end')}
                                                </Button>
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                            <p className="text-xs text-ink-faint">
                                {t('auditSignIns.sessions.note')}
                            </p>
                        </>
                    )}
                </>
            )}
        </section>
    );
}

/**
 * One session's activity.
 *
 * The journey asks for the byte counters as a time series of samples. Nothing
 * writes a sample row, so the read behind the series answers with none; the
 * panel shows the session row's own running totals and states that the series
 * is not available rather than drawing an empty chart, which would claim the
 * session moved nothing.
 */
function ActivityPanel({
    sessions,
    chosen,
    onChoose,
}: {
    readonly sessions: readonly Session[];
    readonly chosen: Session | undefined;
    readonly onChoose: (id: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('auditSignIns.activity.title')}</h2>
                <p className="text-sm text-ink-muted">{t('auditSignIns.activity.lead')}</p>
            </header>
            {chosen === undefined ? (
                <Notice tone="warn">{t('auditSignIns.activity.noSession')}</Notice>
            ) : (
                <>
                    <label className="flex max-w-md flex-col gap-1 text-xs text-ink-muted">
                        {t('auditSignIns.activity.session')}
                        <Select
                            value={chosen.id}
                            onChange={(event) => onChoose(event.target.value)}
                        >
                            {sessions.map((row) => (
                                <option key={row.id} value={row.id}>
                                    {row.clientIdentifier === '' ? '—' : row.clientIdentifier}
                                    {' · '}
                                    {row.clientIp === '' ? '—' : row.clientIp}
                                    {' · '}
                                    {row.startTime}
                                </option>
                            ))}
                        </Select>
                    </label>
                    <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                        <div>
                            <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                                {t('auditSignIns.column.started')}
                            </dt>
                            <dd className="mt-0.5 font-mono text-xs">{chosen.startTime}</dd>
                        </div>
                        <div>
                            <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                                {t('auditSignIns.activity.bytesSent')}
                            </dt>
                            <dd className="mt-0.5 font-mono text-xs">{chosen.bytesSent}</dd>
                        </div>
                        <div>
                            <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                                {t('auditSignIns.activity.bytesReceived')}
                            </dt>
                            <dd className="mt-0.5 font-mono text-xs">{chosen.bytesReceived}</dd>
                        </div>
                    </dl>
                    <Notice tone="warn">{t('auditSignIns.activity.notAvailable')}</Notice>
                </>
            )}
        </section>
    );
}

/** The session statistics: one row per day and account. */
function StatisticsPanel({
    query,
    accountNames,
}: {
    readonly query: UseQueryResult<readonly SessionStatisticsRow[]>;
    readonly accountNames: ReadonlyMap<string, string>;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('auditSignIns.statistics.title')}</h2>
                <p className="text-sm text-ink-muted">{t('auditSignIns.statistics.lead')}</p>
            </header>
            {query.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : query.isError ? (
                <Notice tone="error">
                    {failureReason(query.error, t('auditSignIns.statistics.notAllowed'))}
                </Notice>
            ) : query.data.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('auditSignIns.statistics.empty')}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">{t('auditSignIns.column.day')}</th>
                            <th className="py-1 font-normal">{t('auditSignIns.column.account')}</th>
                            <th className="py-1 text-right font-normal">
                                {t('auditSignIns.statistics.sessions')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('auditSignIns.statistics.average')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('auditSignIns.column.traffic')}
                            </th>
                            <th className="py-1 text-right font-normal">
                                {t('auditSignIns.statistics.countries')}
                            </th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {query.data.map((row) => (
                            <tr key={`${row.day}-${row.accountId}`}>
                                <td className="py-2 font-mono text-xs">{row.day}</td>
                                <td className="py-2">
                                    <AccountCell accountId={row.accountId} nameOf={accountNames} />
                                </td>
                                <td className="py-2 text-right tabular-nums">{row.sessionCount}</td>
                                <td className="py-2 text-right tabular-nums">
                                    {`${String(Math.round(row.avgDurationSeconds))} s`}
                                </td>
                                <td className="whitespace-nowrap py-2 text-right font-mono text-xs text-ink-faint">
                                    {String(row.totalBytesSent)} / {String(row.totalBytesReceived)}
                                </td>
                                <td className="py-2 text-right tabular-nums">
                                    {row.uniqueCountries}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
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
    query,
    accountNames,
}: {
    readonly query: UseQueryResult<{
        readonly loginInfo: readonly LoginInfo[];
        readonly totalCount: number;
    }>;
    readonly accountNames: ReadonlyMap<string, string>;
}): ReactNode {
    const { t } = useTranslation();
    const ordered = orderLoginRecords(query.data?.loginInfo ?? []);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('auditSignIns.failures.title')}</h2>
                {query.data !== undefined && (
                    <span className="text-xs text-ink-faint">
                        {t('auditSignIns.failures.count', {
                            shown: ordered.length,
                            total: query.data.totalCount,
                        })}
                    </span>
                )}
            </header>
            {query.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : query.isError ? (
                <Notice tone="error">
                    {failureReason(query.error, t('auditSignIns.failures.notAllowed'))}
                </Notice>
            ) : ordered.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('auditSignIns.failures.empty')}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">{t('auditSignIns.column.account')}</th>
                            <th className="py-1 font-normal">
                                {t('auditSignIns.failures.failed')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('auditSignIns.failures.lastAddress')}
                            </th>
                            <th className="py-1 font-normal">
                                {t('auditSignIns.failures.lastSignIn')}
                            </th>
                            <th className="py-1 font-normal">{t('auditSignIns.column.state')}</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {ordered.map((row) => (
                            <tr key={row.accountId}>
                                <td className="py-2">
                                    <AccountCell accountId={row.accountId} nameOf={accountNames} />
                                </td>
                                <td className="py-2 tabular-nums">{row.failedLogins}</td>
                                <td className="py-2">
                                    {isZeroAddress(row.lastAttemptIp) ? (
                                        <span className="text-xs text-ink-faint">
                                            {t('auditSignIns.failures.never')}
                                        </span>
                                    ) : (
                                        <span className="font-mono text-xs">
                                            {row.lastAttemptIp}
                                        </span>
                                    )}
                                </td>
                                <td className="py-2">
                                    {isZeroTimestamp(row.lastLogin) ? (
                                        <span className="text-xs text-ink-faint">
                                            {t('auditSignIns.failures.neverSignedIn')}
                                        </span>
                                    ) : (
                                        row.lastLogin
                                    )}
                                </td>
                                <td className="py-2">
                                    <Tag tone={row.locked ? 'warn' : 'muted'}>
                                        {row.locked
                                            ? t('auditSignIns.failures.locked')
                                            : t('auditSignIns.failures.notLocked')}
                                    </Tag>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

/** The authentication event log, newest first. */
function EventsPanel({
    query,
    accountNames,
}: {
    readonly query: UseQueryResult<readonly AuthEvent[]>;
    readonly accountNames: ReadonlyMap<string, string>;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('auditSignIns.events.title')}</h2>
                <p className="text-sm text-ink-muted">{t('auditSignIns.events.lead')}</p>
            </header>
            {query.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : query.isError ? (
                <Notice tone="error">
                    {failureReason(query.error, t('auditSignIns.events.notAllowed'))}
                </Notice>
            ) : query.data.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('auditSignIns.events.empty')}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">{t('auditSignIns.events.time')}</th>
                            <th className="py-1 font-normal">{t('auditSignIns.events.event')}</th>
                            <th className="py-1 font-normal">{t('auditSignIns.column.account')}</th>
                            <th className="py-1 font-normal">{t('auditSignIns.events.session')}</th>
                            <th className="py-1 font-normal">{t('auditSignIns.events.detail')}</th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {query.data.map((row) => (
                            <tr key={row.id}>
                                <td className="whitespace-nowrap py-2 font-mono text-xs">
                                    {row.eventTime}
                                </td>
                                <td className="py-2">
                                    <Tag tone={eventTone(row.eventType)}>{row.eventType}</Tag>
                                </td>
                                <td className="py-2">
                                    <AccountCell
                                        accountId={row.accountId}
                                        username={row.username}
                                        nameOf={accountNames}
                                    />
                                </td>
                                <td className="py-2 font-mono text-xs">
                                    {row.sessionId === '' ? '—' : row.sessionId}
                                </td>
                                <td className="py-2 text-ink-muted">
                                    {row.errorDetail === '' ? '—' : row.errorDetail}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

export function AuditPage(): ReactNode {
    const { t } = useTranslation();
    const [filter, setFilter] = useState<AuditFilter>(DEFAULT_AUDIT_FILTER);
    const [chosen, setChosen] = useState('');
    const [endingId, setEndingId] = useState('');
    const [endedId, setEndedId] = useState('');
    const [endError, setEndError] = useState('');

    const sessions = useQuery({
        queryKey: [AUDIT_SESSIONS_QUERY_KEY],
        queryFn: () => api.activeSessions(),
        retry: false,
    });
    const failures = useQuery({
        queryKey: [AUDIT_FAILURES_QUERY_KEY],
        queryFn: () => api.loginInfoPage(),
        retry: false,
    });
    const events = useQuery({
        queryKey: [AUDIT_EVENTS_QUERY_KEY, filter.period, filter.eventType],
        queryFn: () =>
            api.authEvents({
                period: filter.period,
                eventType: filter.eventType,
                offset: 0,
                limit: AUDIT_PAGE_SIZE,
            }),
        retry: false,
    });
    const statistics = useQuery({
        queryKey: [AUDIT_STATISTICS_QUERY_KEY, filter.period],
        queryFn: () =>
            api.sessionStatistics({ period: filter.period, offset: 0, limit: AUDIT_PAGE_SIZE }),
        retry: false,
    });
    const accounts = useQuery({
        queryKey: [AUDIT_ACCOUNTS_QUERY_KEY],
        queryFn: () => api.accounts(),
        retry: false,
    });

    /*
     * The readings speak in account identifiers. The account list is read once
     * and turned into the names every panel shows; a refused read leaves the
     * map empty and each panel falls back to the identifier alone.
     */
    const accountNames = new Map(
        (accounts.data?.accounts ?? []).map((row) => [row.id, row.username] as const),
    );

    const { tab, bar } = useTabs({
        label: t('auditSignIns.tabs'),
        tabs: AUDIT_TABS,
        titleOf: (name) => t(`auditSignIns.tab.${name}`),
    });

    /*
     * The new readings are re-read here rather than by invalidating the cache:
     * a filter that moved the events and the statistics to their own keys
     * leaves the reading the person is looking at on the key they asked for.
     */
    const refresh = (): void => {
        void Promise.all([
            sessions.refetch(),
            failures.refetch(),
            events.refetch(),
            statistics.refetch(),
        ]);
    };

    const end = async (session: Session): Promise<void> => {
        setEndingId(session.id);
        setEndedId('');
        setEndError('');
        try {
            await api.endSession(session.id);
            setEndedId(session.id);
            await sessions.refetch();
        } catch (error) {
            setEndError(
                error instanceof Error ? error.message : t('auditSignIns.sessions.endFailed'),
            );
        }
        setEndingId('');
    };

    const readAt = formatReadAt(
        Math.max(
            sessions.dataUpdatedAt,
            failures.dataUpdatedAt,
            events.dataUpdatedAt,
            statistics.dataUpdatedAt,
        ),
    );
    const openSessions = (sessions.data ?? []).filter((row) =>
        withinPeriod(row.startTime, filter.period),
    );
    const chosenSession = openSessions.find((row) => row.id === chosen) ?? openSessions[0];

    return (
        <div className="space-y-6">
            <Crumbs
                parts={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('auditSignIns.title') },
                ]}
            />
            <PageHeader
                title={t('auditSignIns.title')}
                description={t('auditSignIns.description')}
                actions={
                    <div className="flex items-center gap-3">
                        {readAt !== undefined && (
                            <span className="text-xs text-ink-faint">
                                {t('auditSignIns.readAt', { at: readAt })}
                            </span>
                        )}
                        <Button variant="secondary" onClick={refresh}>
                            {t('auditSignIns.refresh')}
                        </Button>
                    </div>
                }
            />
            <FilterBar filter={filter} onChange={setFilter} />
            {bar}
            {tab === 'sessions' && (
                <SessionsPanel
                    sessions={openSessions}
                    accountNames={accountNames}
                    pending={sessions.isPending}
                    error={sessions.error}
                    endingId={endingId}
                    endedId={endedId}
                    endError={endError}
                    onEnd={(session) => {
                        void end(session);
                    }}
                />
            )}
            {tab === 'activity' && (
                <ActivityPanel
                    sessions={openSessions}
                    chosen={chosenSession}
                    onChoose={setChosen}
                />
            )}
            {tab === 'statistics' && (
                <StatisticsPanel query={statistics} accountNames={accountNames} />
            )}
            {tab === 'failures' && <FailuresPanel query={failures} accountNames={accountNames} />}
            {tab === 'events' && <EventsPanel query={events} accountNames={accountNames} />}
        </div>
    );
}
