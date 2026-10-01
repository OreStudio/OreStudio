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
import type { Account, LoginInfo } from '@ores/wire-protocol/browser';
import { uuid } from '@ores/wire-protocol/browser';
import { RescueView } from './RescuePage.js';

/**
 * Rescue access, as a reader sees it.
 *
 * The screen is rendered from a fixture rather than from a read, because what
 * is being checked is the screen: the state it names, the tag beside the count,
 * and the two things the journey's acceptance turns on. The recovery link is
 * drawn unavailable with its reason, and the lock panel says what a lock does
 * not do. A screen that hid either would pass a review and fail the journey.
 */

const TENANT_ID = uuid('ffffffff-ffff-ffff-ffff-ffffffffffff');

function account(overrides: Partial<Account> = {}): Account {
    return {
        version: 1,
        id: uuid('11111111-1111-1111-1111-111111111111'),
        tenantId: TENANT_ID,
        username: 'jdoe',
        fullName: 'Jane Doe',
        email: 'jane.doe@example.com',
        accountType: 'user',
        jobTitle: 'Analyst',
        reportsToAccountId: null,
        defaultPartyId: null,
        modifiedBy: 'admin',
        changeReasonCode: 'new',
        changeCommentary: '',
        performedBy: 'admin',
        recordedAt: '2026-09-30 12:00:00Z',
        ...overrides,
    };
}

function loginInfo(overrides: Partial<LoginInfo> = {}): LoginInfo {
    return {
        tenantId: TENANT_ID,
        accountId: uuid('11111111-1111-1111-1111-111111111111'),
        lastIp: '203.0.113.44',
        lastAttemptIp: '203.0.113.44',
        failedLogins: 7,
        locked: true,
        lastLogin: '2026-09-29 21:03:00Z',
        online: true,
        passwordResetRequired: false,
        ...overrides,
    };
}

function render(row: Account, state: LoginInfo | null): string {
    return renderToStaticMarkup(
        <MemoryRouter>
            <RescueView account={row} loginInfo={state} onReload={async () => undefined} />
        </MemoryRouter>,
    );
}

describe('RescueView', () => {
    it('names the account, its state and the failed attempts behind it', () => {
        const markup = render(account(), loginInfo());

        expect(markup).toContain('Jane Doe');
        expect(markup).toContain('jdoe');
        expect(markup).toContain('jane.doe@example.com');
        expect(markup).toContain('Locked');
        expect(markup).toContain('7 failed attempts');
        expect(markup).toContain('203.0.113.44');
    });

    it('says a lock leaves open sessions open', () => {
        const markup = render(account(), loginInfo());

        expect(markup).toContain('A lock leaves open sessions open');
    });

    it('offers the recovery link and says it cannot be sent', () => {
        const markup = render(account(), loginInfo());

        expect(markup).toContain('Send the recovery link');
        expect(markup).toContain('disabled');
        expect(markup).toContain('no subject requests a reset');
    });

    it('offers no password the administrator could type', () => {
        const markup = render(account(), loginInfo());

        expect(markup).not.toContain('type="password"');
        expect(markup).toContain('the screen does not offer it');
    });

    it('states that an account with no login record has never signed in', () => {
        const markup = render(account(), null);

        expect(markup).toContain('has never signed in');
        expect(markup).toContain('Not locked');
    });

    it('lists the operations the server does not have', () => {
        const markup = render(account(), loginInfo());

        expect(markup).toContain('Activate or deactivate an account');
        expect(markup).toContain('Self-service recovery');
    });
});
