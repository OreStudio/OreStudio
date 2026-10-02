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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { LoginInfo, Session } from '@ores/wire-protocol/browser';
import { uuid } from '@ores/wire-protocol/browser';
import { AuditView } from './AuditPage.js';

/**
 * Audit sign-ins, as a reader sees it.
 *
 * The screen is rendered from fixtures rather than from a read, because what is
 * being checked is the screen: the reading each tab shows, and the three panels
 * the journey's acceptance turns on. The account filter is unavailable with its
 * reason, the session activity says its series has no read path, and the panels
 * state that nothing writes an end time. A screen that drew an empty chart or an
 * enabled filter would pass a review and fail the journey.
 */

const TENANT_ID = uuid('ffffffff-ffff-ffff-ffff-ffffffffffff');
const ACCOUNT_ID = uuid('11111111-1111-1111-1111-111111111111');

function session(overrides: Partial<Session> = {}): Session {
    return {
        tenantId: TENANT_ID,
        id: uuid('33333333-3333-3333-3333-333333333333'),
        accountId: ACCOUNT_ID,
        startTime: '2026-10-01 20:00:00Z',
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

const loaded = {
    active: [session()],
    sessionCount: 17,
    loginInfo: [loginInfo()],
    loginInfoCount: 25,
    readAt: '2026-10-01 23:00:00 UTC',
};

function render(tab: string): string {
    return renderToStaticMarkup(
        <MemoryRouter initialEntries={[`/audit?tab=${tab}`]}>
            <AuditView loaded={loaded} />
        </MemoryRouter>,
    );
}

describe('AuditView', () => {
    it('opens on the sessions reading and says nothing ends a session', () => {
        const markup = render('sessions');

        expect(markup).toContain('Active sessions');
        expect(markup).toContain('ores.web');
        expect(markup).toContain('203.0.113.44');
        expect(markup).toContain('Nothing ends a session');
        expect(markup).toContain('17');
    });

    it('draws the account and event filters unavailable, with their reasons', () => {
        const markup = render('sessions');

        expect(markup).toContain('the read takes no account filter');
        expect(markup).toContain('no subject carries them');
        expect(markup).toContain('not a versioned entity');
    });

    it('shows the session totals and says the sample series has no read path', () => {
        const markup = render('activity');

        expect(markup).toContain('Session activity');
        expect(markup).toContain('4096');
        expect(markup).toContain('8192');
        expect(markup).toContain('Nothing serves the samples');
    });

    it('reads the failed attempts with the lock state beside the count', () => {
        const markup = render('failures');

        expect(markup).toContain('Failed attempts');
        expect(markup).toContain('198.51.100.7');
        expect(markup).toContain('Locked');
        expect(markup).toContain('25');
    });

    it('lists the readings the server does not serve', () => {
        const markup = render('failures');

        expect(markup).toContain('The authentication events');
        expect(markup).toContain('The session statistics');
        expect(markup).toContain('End another account\u2019s session');
        expect(markup).toContain('Filter by account');
    });

    it('opens the reading the query parameter names', () => {
        expect(render('activity')).toContain('Session activity');
        expect(render('failures')).not.toContain('Active sessions');
    });
});
