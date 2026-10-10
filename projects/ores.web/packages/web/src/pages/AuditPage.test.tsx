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
import {
    AUDIT_ACCOUNTS_QUERY_KEY,
    AUDIT_EVENTS_QUERY_KEY,
    AUDIT_FAILURES_QUERY_KEY,
    AUDIT_SESSIONS_QUERY_KEY,
    AUDIT_STATISTICS_QUERY_KEY,
    AuditPage,
    eventTone,
    formatReadAt,
    isZeroAddress,
    orderLoginRecords,
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
        endTime: '',
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
        username: 'jonas.lindqvist',
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

function render(path = '/audit', loginRecords: readonly LoginInfo[] = [loginInfo()]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData([AUDIT_SESSIONS_QUERY_KEY], [session()]);
    client.setQueryData([AUDIT_FAILURES_QUERY_KEY], {
        loginInfo: loginRecords,
        totalCount: 25,
    });
    client.setQueryData([AUDIT_EVENTS_QUERY_KEY, 'day', ''], [authEvent()]);
    client.setQueryData([AUDIT_STATISTICS_QUERY_KEY, 'day'], [statisticsRow()]);
    client.setQueryData([AUDIT_ACCOUNTS_QUERY_KEY], {
        accounts: [account()],
        totalCount: 1,
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

describe('the audit sign-ins screen', () => {
    it('opens on the active sessions, and offers to end one', () => {
        const html = render();

        expect(html).toContain('Audit: sign-ins');
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
            lastLogin: '1970-01-01 00:00:00Z',
            lastAttemptIp: '0.0.0.0',
        });
        const html = render('/audit?tab=failures', [never]);

        expect(html).toContain(ACCOUNT_ID);
        expect(html).toContain('>Never signed in<');
        expect(html).toContain('>Never<');
        expect(html).not.toContain('1970-01-01');
        expect(html).not.toContain('0.0.0.0');
    });

    it('reads the authentication event log, newest first', () => {
        const html = render('/audit?tab=events');

        expect(html).toContain('Authentication events');
        expect(html).toContain('login_failure');
        expect(html).toContain('jonas.lindqvist');
        expect(html).toContain('bad password');
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

    it('orders the most failed first, then the accounts that have signed in', () => {
        const accountA = loginInfo({ accountId: ACCOUNT_ID, failedLogins: 2, lastLogin: '' });
        const accountB = loginInfo({
            accountId: uuid('22222222-2222-2222-2222-222222222222'),
            failedLogins: 5,
            lastLogin: '1970-01-01 00:00:00Z',
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

    it('paints the event types the log stores', () => {
        expect(eventTone('login_success')).toBe('up');
        expect(eventTone('login_failure')).toBe('warn');
        expect(eventTone('logout')).toBe('muted');
        expect(eventTone('token_refresh')).toBe('neutral');
    });
});
