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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import type { Account, MyParties } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { WhereIWorkPage } from './WhereIWorkPage.js';

/**
 * The Where I work screen, rendered from a seeded query cache.
 *
 * What is checked is what the screen promises: each party the person works in
 * is named, the party the session acts for and the stored default are stated
 * apart, the two actions a person takes are offered where they belong, and a
 * party the server cannot name is stated rather than drawn nameless.
 */

const US = '33333333-3333-3333-3333-333333333333';
const UK = '44444444-4444-4444-4444-444444444444';

const self = {
    version: 1,
    id: '11111111-1111-1111-1111-111111111111',
    tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    username: 'ashley.moore',
    fullName: 'Ashley Moore',
    email: 'ashley.moore@acme.example',
    accountType: 'user',
    jobTitle: 'Junior Analyst',
    reportsToAccountId: null,
    defaultPartyId: US,
    imageId: null,
    modifiedBy: 'system',
    changeReasonCode: 'system.new_record',
    changeCommentary: '',
    performedBy: 'system',
    recordedAt: '2026-10-04 09:00:00Z',
} as unknown as Account;

function party(partyId: string, name: string, shortCode: string): MyParties['parties'][number] {
    return {
        partyId,
        name,
        shortCode,
        partyCategory: 'Operational',
        businessCenterCode: 'USNY',
    };
}

function render(parties: MyParties, props: Partial<Parameters<typeof WhereIWorkPage>[0]> = {}) {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-parties'], parties);
    client.setQueryData(['account', 'ashley.moore'], self);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <WhereIWorkPage
                    tenantName="Acme Corporation"
                    username="ashley.moore"
                    email="ashley.moore@acme.example"
                    actingPartyId={US}
                    onSwitchParty={async () => undefined}
                    {...props}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('Where I work', () => {
    it('names the parties, and states the acting party and the default apart', () => {
        const html = render({
            defaultPartyId: US,
            parties: [
                party(US, 'ACME Corporation US Inc', 'ACCOUS'),
                party(UK, 'ACME Corporation UK plc', 'ACCOUK'),
            ],
        });

        expect(html).toContain('ACME Corporation US Inc');
        expect(html).toContain('ACME Corporation UK plc');
        expect(html).toContain('ACCOUS');
        expect(html).toContain('Ashley Moore');
        expect(html).toContain('Junior Analyst');
        expect(html).toContain('src="/api/accounts/ashley.moore/picture"');

        // The party the session acts for offers no switch, and the stored
        // default is what the clear control belongs to.
        expect(html).toContain('This is the party your session is acting for.');
        expect(html).toContain('Clear default');
        expect(html).toContain('Switch to this party');
        expect(html).toContain('Acting for');
        expect(html).toContain('Default');

        // Switching and setting the default are different actions on the same
        // card, so the party that is neither acting nor default offers both.
        expect(html).toContain('Set as default');
    });

    it('says there is nothing to switch when the account works in one party', () => {
        const html = render({
            defaultPartyId: US,
            parties: [party(US, 'ACME Corporation US Inc', 'ACCOUS')],
        });

        expect(html).toContain('works in one party');
        expect(html).not.toContain('Switch to this party');
    });

    it('states a party the server cannot name rather than drawing it nameless', () => {
        const html = render({
            defaultPartyId: '',
            parties: [party(US, '', ''), party(UK, 'ACME Corporation UK plc', 'ACCOUK')],
        });

        expect(html).toContain('A party this build cannot name');
        expect(html).toContain('ACME Corporation UK plc');
        // No default is stored, so both parties offer one.
        expect(html).not.toContain('Clear default');
    });

    it('falls back to the username when the account read is refused', () => {
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        client.setQueryData(['my-parties'], {
            defaultPartyId: US,
            parties: [party(US, 'ACME Corporation US Inc', 'ACCOUS')],
        } as MyParties);
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <WhereIWorkPage
                        tenantName="Acme Corporation"
                        username="ashley.moore"
                        email="ashley.moore@acme.example"
                        actingPartyId={US}
                        onSwitchParty={async () => undefined}
                    />
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('ashley.moore');
        expect(html).not.toContain('Ashley Moore');
    });
});
