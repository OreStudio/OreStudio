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
 * Fixture rows for the credentials prototypes. The browser has no read path
 * for any of this data today: the BFF serves the caller's own session and the
 * password change, and nothing else in this file. Every panel that renders
 * these rows says so on the screen.
 */

import type { PasswordPolicy } from '@ores/wire-protocol/browser';

export interface PrototypeAccount {
    readonly username: string;
    readonly fullName: string;
    readonly email: string;
    readonly accountType: string;
}

export interface PrototypeLoginState {
    readonly lastSignInAt: string;
    readonly lastSignInFrom: string;
    readonly failedAttempts: number;
    readonly locked: boolean;
    readonly online: boolean;
    readonly passwordResetRequired: boolean;
}

export interface PrototypeSession {
    readonly id: string;
    readonly client: string;
    readonly address: string;
    readonly country: string;
    readonly startedAt: string;
    readonly duration: string;
    readonly bytesIn: string;
    readonly bytesOut: string;
    readonly thisDevice: boolean;
}

export interface PrototypeAuditRow {
    readonly account: string;
    readonly event: string;
    readonly address: string;
    readonly country: string;
    readonly at: string;
}

export const account: PrototypeAccount = {
    username: 'amara.okafor',
    fullName: 'Amara Okafor',
    email: 'amara.okafor@acme.example',
    accountType: 'user',
};

export const loginState: PrototypeLoginState = {
    lastSignInAt: '2026-09-30 08:12 UTC',
    lastSignInFrom: '203.0.113.44 · United Kingdom',
    failedAttempts: 2,
    locked: false,
    online: true,
    passwordResetRequired: false,
};

export const sessions: readonly PrototypeSession[] = [
    {
        id: '5F1B0A2C-9C34-4C7E-9A11-8E4B7D2F6A01',
        client: 'ores.web',
        address: '203.0.113.44',
        country: 'United Kingdom',
        startedAt: '2026-09-30 08:12 UTC',
        duration: '3h 41m',
        bytesIn: '18.4 MB',
        bytesOut: '2.1 MB',
        thisDevice: true,
    },
    {
        id: 'A7C4E1D8-2B69-4F03-8D52-1C9A6E3B7F42',
        client: 'ores.shell',
        address: '198.51.100.7',
        country: 'Germany',
        startedAt: '2026-09-29 21:03 UTC',
        duration: '14h 50m',
        bytesIn: '1.2 MB',
        bytesOut: '340 KB',
        thisDevice: false,
    },
    {
        id: 'D2E8B5A9-4C17-4A6B-B3F8-7E5D0C2A9B63',
        client: 'ores.web',
        address: '192.0.2.19',
        country: 'Netherlands',
        startedAt: '2026-09-28 06:40 UTC',
        duration: '2d 5h',
        bytesIn: '44.9 MB',
        bytesOut: '6.8 MB',
        thisDevice: false,
    },
];

export const auditRows: readonly PrototypeAuditRow[] = [
    {
        account: 'amara.okafor',
        event: 'login',
        address: '203.0.113.44',
        country: 'United Kingdom',
        at: '2026-09-30 08:12 UTC',
    },
    {
        account: 'amara.okafor',
        event: 'login failed',
        address: '203.0.113.44',
        country: 'United Kingdom',
        at: '2026-09-30 08:11 UTC',
    },
    {
        account: 'amara.okafor',
        event: 'login failed',
        address: '203.0.113.44',
        country: 'United Kingdom',
        at: '2026-09-30 08:11 UTC',
    },
    {
        account: 'jonas.lindqvist',
        event: 'login',
        address: '198.51.100.7',
        country: 'Germany',
        at: '2026-09-29 21:03 UTC',
    },
    {
        account: 'priya.raman',
        event: 'logout',
        address: '192.0.2.19',
        country: 'Netherlands',
        at: '2026-09-29 17:55 UTC',
    },
    {
        account: 'priya.raman',
        event: 'token refresh',
        address: '192.0.2.19',
        country: 'Netherlands',
        at: '2026-09-29 17:40 UTC',
    },
];

/** The server's rules. The same shape GET /api/password-policy returns. */
export const passwordPolicy: PasswordPolicy = {
    success: true,
    message: '',
    minLength: 12,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    specialChars: '!@#$%^&*-_=+',
};

/** The account a tenant administrator is rescuing, and its security state. */
export const rescuedAccount: PrototypeAccount = {
    username: 'jonas.lindqvist',
    fullName: 'Jonas Lindqvist',
    email: 'jonas.lindqvist@acme.example',
    accountType: 'user',
};

export const rescuedLoginState: PrototypeLoginState = {
    lastSignInAt: '2026-09-29 21:03 UTC',
    lastSignInFrom: '198.51.100.7 · Germany',
    failedAttempts: 7,
    locked: true,
    online: true,
    passwordResetRequired: false,
};
