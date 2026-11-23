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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { AgentGrantsAdminPage } from './AgentGrantsAdminPage.js';

const PERSON = '11111111-1111-1111-1111-111111111111';
const ROLE = '22222222-2222-2222-2222-222222222222';
const GRANT = '33333333-3333-3333-3333-333333333333';
const KEY = '44444444-4444-4444-4444-444444444444';
const NEVER = '1970-01-01 00:00:00Z';

function render(permissionCodes: readonly string[]): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false, staleTime: Infinity } },
    });
    client.setQueryData(['my-access'], { roles: [{ roleId: 'r', permissionCodes }] });
    client.setQueryData(['agent-grants-all'], {
        success: true,
        message: '',
        grants: [
            {
                id: GRANT,
                grantor_account_id: PERSON,
                agent_name: 'research-bot',
                role_id: ROLE,
                not_after: '2999-01-01 00:00:00Z',
                revoked_at: NEVER,
            },
        ],
        keys: [
            {
                id: KEY,
                grant_id: GRANT,
                revoked_at: NEVER,
                recorded_at: '2026-11-22 10:00:00Z',
                secret_hash: '',
            },
        ],
    });
    client.setQueryData(['agent-grants-accounts'], {
        accounts: [{ id: PERSON, username: 'alice' }],
        totalCount: 1,
    });
    client.setQueryData(['agent-grants-roles'], [{ id: ROLE, name: 'AgentReader' }]);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <AgentGrantsAdminPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('Agent grants for administrators', () => {
    it('shows each grant with its person, agent, role and key, and links to its sessions', () => {
        const html = render(['iam::agent_grants:read']);

        expect(html).toContain('alice');
        expect(html).toContain('research-bot');
        expect(html).toContain('AgentReader');
        expect(html).toContain(KEY);
        expect(html).toContain(`/operations/sign-ins?grant=${GRANT}`);
    });

    it('offers revocation only to a holder of the revoke permission', () => {
        expect(render(['iam::agent_grants:read'])).not.toContain('Revoke grant');
        const html = render(['iam::agent_grants:read', 'iam::agent_grants:revoke']);
        expect(html).toContain('Revoke grant');
        expect(html).toContain('Revoke key');
    });
});
