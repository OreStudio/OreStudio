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
import { MemoryRouter, Route, Routes } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import { TenantAccountPage } from './TenantAccountPage.js';

/**
 * An account opened from a tenant's People tab: who it is, what kind of
 * account, its sign-in state and its sessions.
 */

const service = {
    version: 1,
    id: '22222222-2222-2222-2222-222222222222',
    tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    username: 'ores_iam_service',
    fullName: '',
    email: '',
    accountType: 'service',
    jobTitle: '',
    reportsToAccountId: null,
    defaultPartyId: null,
    imageId: null,
    modifiedBy: 'system',
    changeReasonCode: 'system.initial_load',
    changeCommentary: '',
    performedBy: 'system',
    recordedAt: '2026-10-04 09:00:00Z',
};

function render(signIns: unknown): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['tenant-account-sign-ins', 'system', 'ores_iam_service', 0], signIns);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/tenants/system/people/ores_iam_service']}>
                    <Routes>
                        <Route
                            path="/tenants/:code/people/:username"
                            element={<TenantAccountPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('an account of a tenant', () => {
    it('says it is a service, how it signs in and the sessions it has had, newest first', () => {
        const html = render({
            account: service,
            loginInfo: {
                tenantId: service.tenantId,
                accountId: service.id,
                lastIp: '127.0.0.1',
                lastAttemptIp: '127.0.0.1',
                failedLogins: 0,
                locked: false,
                lastLogin: '2026-10-04 17:01:30Z',
                online: true,
                passwordResetRequired: false,
            },
            sessions: [
                {
                    tenantId: service.tenantId,
                    id: '77777777-0000-0000-0000-000000000002',
                    accountId: service.id,
                    startTime: '2026-10-04 17:01:30Z',
                    endTime: '',
                    clientIp: '127.0.0.1',
                    clientIdentifier: 'ores.service.binary',
                    clientVersionMajor: 0,
                    clientVersionMinor: 25,
                    bytesSent: 0,
                    bytesReceived: 0,
                    countryCode: '',
                    protocol: 'nats',
                },
            ],
            totalCount: 1,
        });

        expect(html).toContain('ores_iam_service');
        expect(html).toContain('>Service<');
        expect(html).toContain('2026-10-04 17:01:30Z');
        expect(html).toContain('ores.service.binary');
        expect(html).toContain('Not recorded');
        expect(html).toContain('No session records its end yet');
    });

    it('says an account that never signed in never did', () => {
        const html = render({ account: service, loginInfo: null, sessions: [], totalCount: 0 });

        expect(html).toContain('Never');
        expect(html).toContain('No sessions are recorded for this account.');
    });
});
