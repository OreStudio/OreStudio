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
    AccountAccess,
    AccountContactInformation,
    SessionView,
} from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { ProfilePage } from './ProfilePage.js';

/**
 * The profile screen, as a reader sees it.
 *
 * Every read the screen makes is seeded in the query cache, so the page
 * renders without a server. What is checked is the screen's own contract: a
 * member's own panels, one per tab, with their read-only fields; no other
 * person, even for a holder of a write permission; and the statement a
 * refused account read draws instead of fields.
 */

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = '44444444-4444-4444-4444-444444444444';
const MANAGER_ID = '33333333-3333-3333-3333-333333333333';
const IMAGE_ID = '55555555-5555-5555-5555-555555555555';
const RECORD_ID = '22222222-2222-2222-2222-222222222222';

const session: SessionView = {
    username: 'ada',
    email: 'ada@example.com',
    accountId: ACCOUNT_ID,
    tenantId: TENANT_ID,
    tenantName: 'Acme Corporation',
    mode: 'application',
    version: 'v0.0.25 (test)',
    party: {
        id: '66666666-6666-6666-6666-666666666666',
        name: 'System Party',
        partyCategory: 'System',
        businessCenterCode: '',
    },
    availableParties: [],
    accessLifetimeSeconds: 1800,
    passwordResetRequired: false,
};

const account: Account = {
    version: 7,
    id: ACCOUNT_ID,
    tenantId: TENANT_ID,
    username: 'ada',
    fullName: 'Ada Lovelace',
    email: 'ada@example.com',
    accountType: 'user',
    jobTitle: 'Head of Desk',
    reportsToAccountId: MANAGER_ID,
    defaultPartyId: null,
    imageId: IMAGE_ID,
    modifiedBy: 'ada',
    changeReasonCode: 'common.non_material_update',
    changeCommentary: '',
    performedBy: 'ada',
    recordedAt: '2026-10-05 09:30:00Z',
};

const contact: AccountContactInformation = {
    version: 4,
    id: RECORD_ID,
    accountId: ACCOUNT_ID,
    streetLine1: '1 Panton Street',
    streetLine2: '',
    city: 'London',
    state: '',
    countryCode: 'GB',
    postalCode: 'SW1Y 4DL',
    phone: '+44 20 0000 0000',
    email: 'ada@colleagues.example.com',
    webPage: '',
    modifiedBy: 'ada',
    changeReasonCode: 'common.rectification',
    changeCommentary: '',
    performedBy: 'ada',
    recordedAt: '2026-10-05 09:30:00Z',
};

const reasons = [
    {
        code: 'common.non_material_update',
        description: 'Non-material update',
        requiresCommentary: false,
    },
    {
        code: 'common.rectification',
        description: 'Rectification',
        requiresCommentary: false,
    },
];

const manager: Account = {
    ...account,
    id: MANAGER_ID,
    username: 'grace',
    fullName: 'Grace Hopper',
    email: 'grace@example.com',
    jobTitle: 'Chief of Staff',
    reportsToAccountId: null,
    imageId: null,
};

function access(permissionCodes: readonly string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '77777777-7777-7777-7777-777777777777',
                name: 'Viewer',
                description: '',
                permissionCodes: [...permissionCodes],
                givenBy: 'system',
                givenAt: '2026-10-05 09:30:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    };
}

function render(seed: (client: QueryClient) => void, path = '/profile'): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['contact-information', 'me'], contact);
    client.setQueryData(['amend-reasons'], reasons);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <ProfilePage session={session} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('ProfilePage', () => {
    const member = (client: QueryClient) => {
        client.setQueryData(['my-access'], access([]));
        client.setQueryData(['account', 'ada'], account);
    };

    it("draws the member's identity on the first tab, with the others offered", () => {
        const html = render(member);

        expect(html).toContain('My profile');
        expect(html).toContain('>Details<');
        expect(html).toContain('>Contact<');
        expect(html).toContain('>Access<');
        expect(html).toContain('Photo and identity');
        expect(html).toContain('value="Ada Lovelace"');
        expect(html).toContain('value="Head of Desk"');
        expect(html).toContain('src="/api/images/55555555-5555-5555-5555-555555555555"');
        expect(html).toContain('Record version 7.');
        expect(html).toContain('Non-material update');
        /*
         * The tenant list that names the manager is the administrator's read,
         * so a member reads the recorded identifier and the panel says why.
         */
        expect(html).toContain(`Reports to ${MANAGER_ID}.`);
        expect(html).toContain('the recorded identifier is shown');
        expect(html).toContain('Propose a change');
        expect(html).not.toContain('value="1 Panton Street"');
    });

    it('draws the contact record on its tab, telling the two addresses apart', () => {
        const html = render(member, '/profile?tab=contact');

        expect(html).toContain('ada@example.com');
        expect(html).toContain('value="ada@colleagues.example.com"');
        expect(html).toContain('not the sign-in address (ada@example.com)');
        expect(html).toContain('value="1 Panton Street"');
        expect(html).not.toContain('value="Ada Lovelace"');
    });

    it('draws the access held, and the doors to sign-in and access, on its tab', () => {
        const html = render(member, '/profile?tab=access');

        expect(html).toContain('Sign-in and access');
        expect(html).toContain('href="/security"');
        expect(html).toContain('href="/access"');
    });

    it('shows a holder of a write permission only their own profile, with the manager named', () => {
        const html = render((client) => {
            client.setQueryData(['my-access'], access(['iam::accounts:update']));
            client.setQueryData(['account', 'ada'], account);
            client.setQueryData(['accounts'], { accounts: [account, manager], totalCount: 2 });
        });

        expect(html).not.toContain('Find the person');
        expect(html).not.toContain('Grace Hopper</');
        expect(html).toContain('Editable');
        expect(html).not.toContain('iam::');
        expect(html).toContain('Reports to Grace Hopper.');
    });

    it("marks the member's own panels editable, naming no permission", () => {
        const html = render((client) => {
            client.setQueryData(['my-access'], access([]));
            client.setQueryData(['account', 'ada'], account);
        }, '/profile');
        expect(html).toContain('Editable');
        expect(html).not.toContain('iam::');
    });

    it('states a refused account read rather than drawing fields it cannot ground', async () => {
        const html = await renderAfterRefusal('/profile');

        expect(html).toContain('The server did not let this screen read the account');
        expect(html).toContain('offers no save');
        expect(html).not.toContain('value="Ada Lovelace"');
    });

    it('keeps the contact tab working when the account read is refused', async () => {
        const html = await renderAfterRefusal('/profile?tab=contact');

        expect(html).toContain('Contact details');
        expect(html).toContain('value="1 Panton Street"');
    });
});

/**
 * The member's own render, with the account read answered by a refusal.
 *
 * The mount refetch is off, because the point of the render is the state a
 * refused read leaves behind: a retry on mount would answer a fresh pending
 * state and there is no server here to settle it.
 */
async function renderAfterRefusal(path: string): Promise<string> {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false, refetchOnMount: false } },
    });
    client.setQueryData(['my-access'], access([]));
    client.setQueryData(['contact-information', 'me'], contact);
    client.setQueryData(['amend-reasons'], reasons);
    await client
        .prefetchQuery({
            queryKey: ['account', 'ada'],
            queryFn: () => Promise.reject(new Error('You do not have access to this.')),
        })
        .catch(() => undefined);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <ProfilePage session={session} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}
