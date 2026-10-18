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

import { describe, expect, it } from 'vitest';
import type { ComponentProps } from 'react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type {
    Account,
    AuthEvent,
    LoginInfo,
    Session,
    SessionStatisticsRow,
} from '@ores/wire-protocol/browser';
import { uuid } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { personPath, rescuePath } from '../shell/paths.js';
import {
    AUDIT_ACCOUNTS_QUERY_KEY,
    AUDIT_EVENTS_QUERY_KEY,
    AUDIT_FAILURES_QUERY_KEY,
    AUDIT_SESSIONS_QUERY_KEY,
    AUDIT_STATISTICS_QUERY_KEY,
    AuditPage,
    FailuresPanel,
    failureTone,
    authEventsAsStream,
    eventKind,
    formatReadAt,
    isZeroAddress,
    orderLoginRecords,
    withoutNeverSignedIn,
    withinPeriod,
} from './AuditPage.js';

/**
 * Audit sign-ins, as a reader sees it.
 *
 * The screen is rendered with each reading already in the cache, so what is
 * checked is the screen: the reading each tab shows, and the states the
 * journey turns on. The account filter is unavailable with its reason, the
 * session activity says the data is not available, and an open session offers
 * the one write the screen has.
 */

const TENANT_ID = uuid('ffffffff-ffff-ffff-ffff-ffffffffffff');
const ACCOUNT_ID = uuid('11111111-1111-1111-1111-111111111111');
const SESSION_ID = uuid('33333333-3333-3333-3333-333333333333');
const USERNAME = 'amara.okafor';

function account(overrides: Partial<Account> = {}): Account {
    return {
        version: 1,
        id: ACCOUNT_ID,
        tenantId: TENANT_ID,
        username: USERNAME,
        fullName: 'Amara Okafor',
        email: 'amara.okafor@example.com',
        accountType: 'user',
        jobTitle: '',
        reportsToAccountId: null,
        defaultPartyId: null,
        imageId: null,
        modifiedBy: 'system',
        changeReasonCode: '',
        changeCommentary: '',
        performedBy: 'system',
        recordedAt: '2026-10-01 22:14:00Z',
        ...overrides,
    };
}

function session(overrides: Partial<Session> = {}): Session {
    return {
        tenantId: TENANT_ID,
        id: SESSION_ID,
        accountId: ACCOUNT_ID,
        startTime: new Date().toISOString().replace('T', ' ').slice(0, 19) + 'Z',
        endTime: null,
        clientIp: '203.0.113.44',
        clientIdentifier: 'ores.web',
        clientVersionMajor: 0,
        clientVersionMinor: 0,
        bytesSent: 4096,
        bytesReceived: 8192,
        countryCode: 'GB',
        protocol: 'https',
        ...overrides,
    };
}

function loginInfo(overrides: Partial<LoginInfo> = {}): LoginInfo {
    return {
        tenantId: TENANT_ID,
        accountId: ACCOUNT_ID,
        lastIp: '203.0.113.44',
        lastAttemptIp: '198.51.100.7',
        failedLogins: 7,
        locked: true,
        lastLogin: '2026-09-29 21:03:00Z',
        online: true,
        passwordResetRequired: false,
        ...overrides,
    };
}

function authEvent(overrides: Partial<AuthEvent> = {}): AuthEvent {
    return {
        id: uuid('44444444-4444-4444-4444-444444444444'),
        eventTime: '2026-10-01 22:14:00Z',
        accountId: ACCOUNT_ID,
        eventType: 'login_failure',
        username: USERNAME,
        sessionId: '',
        partyId: '',
        errorDetail: 'bad password',
        ...overrides,
    };
}

function statisticsRow(overrides: Partial<SessionStatisticsRow> = {}): SessionStatisticsRow {
    return {
        day: '2026-10-01',
        accountId: ACCOUNT_ID,
        sessionCount: 4,
        avgDurationSeconds: 1800,
        totalBytesSent: 18840000,
        totalBytesReceived: 2400000,
        avgBytesSent: 4710000,
        avgBytesReceived: 600000,
        uniqueCountries: 2,
        ...overrides,
    };
}

function render(
    path = '/audit',
    loginRecords: readonly LoginInfo[] = [loginInfo()],
    people: readonly Account[] = [account()],
    events: readonly AuthEvent[] = [authEvent()],
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData([AUDIT_SESSIONS_QUERY_KEY], [session()]);
    client.setQueryData([AUDIT_FAILURES_QUERY_KEY], {
        loginInfo: loginRecords,
        totalCount: 25,
    });
    client.setQueryData([AUDIT_EVENTS_QUERY_KEY, 'day', ''], events);
    client.setQueryData([AUDIT_STATISTICS_QUERY_KEY, 'day'], [statisticsRow()]);
    client.setQueryData([AUDIT_ACCOUNTS_QUERY_KEY], {
        accounts: people,
        totalCount: people.length,
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <AuditPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function renderFailures(rows: readonly LoginInfo[], hideNeverSignedInAtStart: boolean): string {
    const client = new QueryClient();
    client.setQueryData([AUDIT_ACCOUNTS_QUERY_KEY], { accounts: [account()], totalCount: 1 });
    const query = {
        isPending: false,
        isError: false,
        data: { loginInfo: rows, totalCount: rows.length },
    };
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <FailuresPanel
                        query={query as ComponentProps<typeof FailuresPanel>['query']}
                        accountNames={
                            new Map([[ACCOUNT_ID, { username: USERNAME, imageId: null }]])
                        }
                        hideNeverSignedInAtStart={hideNeverSignedInAtStart}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the audit sign-ins screen', () => {
    it('opens on the active sessions, and offers to end one', () => {
        const html = render();

        expect(html).toContain('Sign-ins');
        expect(html).toContain('Active sessions');
        expect(html).toContain('ores.web');
        expect(html).toContain('203.0.113.44');
        expect(html).toContain('GB');
        expect(html).toContain('End session');
    });

    it('draws the account filter unavailable, with its reason', () => {
        const html = render();

        expect(html).toContain('Every account');
        expect(html).toContain('takes no account filter');
        expect(html).toContain('not a versioned entity');
    });

    it('states the session activity as not available rather than drawing a series', () => {
        const html = render('/audit?tab=activity');

        expect(html).toContain('Session activity');
        expect(html).toContain('The data is not available');
        expect(html).toContain('4096');
        expect(html).toContain('8192');
    });

    it('reads the session statistics, one row per day and account', () => {
        const html = render('/audit?tab=statistics');

        expect(html).toContain('Session statistics');
        expect(html).toContain('2026-10-01');
        expect(html).toContain('18840000');
        expect(html).toContain('1800 s');
    });

    it('reads the failed attempts with the lock state beside the count', () => {
        const html = render('/audit?tab=failures');

        expect(html).toContain('Failed attempts');
        expect(html).toContain('198.51.100.7');
        expect(html).toContain('Locked');
        expect(html).toContain('7');
    });

    it('names the account above its identifier in every panel', () => {
        expect(render('/audit?tab=sessions')).toContain(USERNAME);
        expect(render('/audit?tab=statistics')).toContain(USERNAME);
        expect(render('/audit?tab=failures')).toContain(USERNAME);
        expect(render('/audit?tab=sessions')).toContain(ACCOUNT_ID);
        expect(render('/audit?tab=statistics')).toContain(ACCOUNT_ID);
        expect(render('/audit?tab=failures')).toContain(ACCOUNT_ID);
        expect(render('/audit?tab=events')).toContain(ACCOUNT_ID);
    });

    it('keeps a login record for an account that has never been tried, and calls it never', () => {
        const never = loginInfo({
            failedLogins: 0,
            locked: false,
            online: false,
            lastLogin: null,
            lastAttemptIp: '0.0.0.0',
        });
        const html = renderFailures([never], false);

        expect(html).toContain(ACCOUNT_ID);
        expect(html).toContain('>Never signed in<');
        expect(html).toContain('>Never<');
        expect(html).not.toContain('1970-01-01');
        expect(html).not.toContain('0.0.0.0');
    });

    it('hides the accounts that never signed in until the button is released', () => {
        const never = loginInfo({ lastLogin: null });

        expect(renderFailures([never], true)).not.toContain(ACCOUNT_ID);
        expect(renderFailures([never], true)).toContain('Every record is hidden');
        expect(renderFailures([never], true)).toContain('aria-pressed="true"');
        expect(renderFailures([never], false)).toContain(ACCOUNT_ID);
    });

    it('draws a count above two in the warning colour and above five in red', () => {
        expect(failureTone(2)).toBe('plain');
        expect(failureTone(3)).toBe('warn');
        expect(failureTone(5)).toBe('warn');
        expect(failureTone(6)).toBe('down');
        expect(renderFailures([loginInfo({ failedLogins: 4 })], true)).toContain('text-warn');
        expect(renderFailures([loginInfo({ failedLogins: 7 })], true)).toContain('text-down');
    });
    it('links the account name to the page of the person', () => {
        const html = render('/audit?tab=failures');

        expect(html).toContain(`href="${personPath(USERNAME)}"`);
    });

    it('links a locked account to its rescue page, and an unlocked one to nothing', () => {
        const locked = renderFailures([loginInfo({ locked: true })], false);
        const open = renderFailures([loginInfo({ locked: false })], false);

        expect(locked).toContain(`href="${rescuePath(USERNAME)}"`);
        expect(locked).toMatch(
            /<a [^>]*class="[^"]*border[^"]*"[^>]*href="\/organisation\/rescue\?username=/,
        );
        expect(locked).toContain('aria-label="Rescue access"');
        expect(locked).toContain('title="Rescue access"');
        expect(open).not.toContain('/organisation/rescue');
    });

    it('draws the photo of the account beside its name on every tab', () => {
        const photo = '5b8e0a52-7c1d-4f43-9a6b-0d2f6e3a9c11';
        const people = [account({ imageId: photo })];

        for (const tab of ['sessions', 'statistics', 'failures', 'events']) {
            expect(render(`/audit?tab=${tab}`, [loginInfo()], people)).toContain(
                `/api/images/${photo}`,
            );
        }
    });

    it('draws the initials where the account has no photo', () => {
        const html = render('/audit?tab=failures');

        expect(html).not.toContain('<img');
        expect(html).toContain('>AO<');
    });

    it('offers the signed-in filter as a button that is pressed to start with', () => {
        const html = render('/audit?tab=failures');

        expect(html).toContain('>Signed in only<');
        expect(html).toContain('aria-pressed="true"');
    });

    it('reads the authentication event log, newest first', () => {
        const html = render('/audit?tab=events');

        expect(html).toContain('Authentication events');
        expect(html).toContain('Sign-in refused');
        expect(html).toContain('login_failure');
        expect(html).toContain(USERNAME);
        expect(html).toContain('bad password');
        expect(html).toContain('data-timeline');
    });

    it('opens the reading the query parameter names, and no other', () => {
        expect(render('/audit?tab=statistics')).toContain('Session statistics');
        expect(render('/audit?tab=events')).toContain('login_failure');
        // The tab bar names every reading, so the reading left behind is
        // absent rather than its name.
        expect(render('/audit?tab=events')).not.toContain('Last address');
    });
});

describe('the audit helpers', () => {
    it('keeps a session inside the chosen period, and everything under all', () => {
        const now = new Date().toISOString().replace('T', ' ').slice(0, 19) + 'Z';

        expect(withinPeriod(now, 'hour')).toBe(true);
        expect(withinPeriod('2000-01-01 00:00:00Z', 'hour')).toBe(false);
        expect(withinPeriod('2000-01-01 00:00:00Z', 'all')).toBe(true);
    });

    it('states a read time in UTC, and nothing before any read', () => {
        expect(formatReadAt(0)).toBeUndefined();
        expect(formatReadAt(Date.UTC(2026, 9, 1, 22, 14, 0))).toBe('22:14:00 UTC');
    });

    it('states a zero or empty address as no address at all', () => {
        expect(isZeroAddress('0.0.0.0')).toBe(true);
        expect(isZeroAddress('')).toBe(true);
        expect(isZeroAddress('198.51.100.7')).toBe(false);
    });

    it('drops the accounts whose record states no last sign-in', () => {
        const signedIn = loginInfo({ lastLogin: '2026-09-29 21:03:00Z' });
        const never = loginInfo({ lastLogin: null });

        expect(withoutNeverSignedIn([signedIn, never])).toEqual([signedIn]);
        expect(withoutNeverSignedIn([never])).toEqual([]);
    });

    it('orders the most failed first, then the accounts that have signed in', () => {
        const accountA = loginInfo({
            accountId: ACCOUNT_ID,
            failedLogins: 2,
            lastLogin: null,
        });
        const accountB = loginInfo({
            accountId: uuid('22222222-2222-2222-2222-222222222222'),
            failedLogins: 5,
            lastLogin: null,
        });
        const accountC = loginInfo({
            accountId: uuid('33333333-3333-3333-3333-333333333333'),
            failedLogins: 2,
            lastLogin: '2026-09-29 21:03:00Z',
        });

        expect(
            orderLoginRecords([accountA, accountB, accountC]).map((row) => row.accountId),
        ).toEqual([accountB.accountId, accountC.accountId, accountA.accountId]);
    });

    it('draws each event type the log stores as a kind of timeline entry', () => {
        expect(eventKind('login_success')).toBe('signed_in');
        expect(eventKind('login_failure')).toBe('sign_in_failed');
        expect(eventKind('signup_failure')).toBe('sign_in_failed');
        expect(eventKind('logout')).toBe('signed_out');
        expect(eventKind('token_refresh')).toBe('refreshed');
        expect(eventKind('account_locked')).toBe('locked');
        expect(eventKind('account_unlocked')).toBe('unlocked');
        expect(eventKind('select_party_only')).toBe('noticed');
    });

    it('links and pictures the person of an event that has no session', () => {
        const photo = '5b8e0a52-7c1d-4f43-9a6b-0d2f6e3a9c11';
        const html = render(
            '/audit?tab=events',
            [loginInfo()],
            [account({ imageId: photo })],
            [authEvent({ accountId: '', sessionId: '', username: USERNAME })],
        );

        expect(html).toContain(`href="${personPath(USERNAME)}"`);
        expect(html).toContain(`/api/images/${photo}`);
    });

    it('names the person of an event that has no account or session by its username', () => {
        const stream = authEventsAsStream(
            [authEvent({ accountId: '', sessionId: '', username: 'diane.kay' })],
            new Map(),
        );

        expect(stream.events[0]?.actor).toBe('diane.kay');
    });
});
