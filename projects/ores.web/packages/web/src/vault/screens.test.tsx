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
 */

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { ReactNode } from 'react';
import { describe, expect, it } from 'vitest';
import type { VaultConsumer, VaultSecret } from '../api/vault.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ConsumersPage } from './ConsumersPage.js';
import { SecretPage } from './SecretPage.js';
import { SecretsPage } from './SecretsPage.js';

/**
 * The vault's screens, called the way a person reaches them: the reads in the
 * query cache and the permissions in the person's own access, with the screen
 * drawing what it is given.
 *
 * The value is the one thing no screen holds. The list and the record are asked
 * to say what the vault holds and to leave the ask to the record's own control,
 * which is why the value appears in none of the markup these tests read.
 */

const SECRETS: readonly VaultSecret[] = [
    {
        id: '11111111-1111-4111-8111-111111111111',
        name: 'db.marketdata_service.password',
        owner: 'marketdata_service',
        key_version: 2,
        description: 'The market data service connects with it',
        modified_by: 'vault provision',
        recorded_at: '2026-11-28 09:00:00Z',
    },
    {
        id: '22222222-2222-4222-8222-222222222222',
        name: 'ores.marketdata.vendor.api_key',
        owner: 'marketdata_service',
        key_version: 1,
        description: '',
        modified_by: 'marco',
        recorded_at: '2026-11-27 14:00:00Z',
    },
];

const CONSUMERS: readonly VaultConsumer[] = [
    {
        id: '33333333-3333-4333-8333-333333333333',
        name: 'marketdata_service',
        actor: 'marketdata_service@system.ores',
        public_key: '8f2c0d4e6a1b39f0a1b2c3d4e5f60718',
        is_enabled: true,
        description: 'Writes the tick series',
        recorded_at: '2026-11-20 08:00:00Z',
    },
    {
        id: '44444444-4444-4444-8444-444444444444',
        name: 'reporting_service',
        actor: 'reporting_service@system.ores',
        public_key: 'c41b02aa11bb22cc33dd44ee55ff6607',
        is_enabled: false,
        description: '',
        recorded_at: '2026-11-20 08:00:00Z',
    },
];

const EVERYTHING = ['*'];

function render(
    children: ReactNode,
    path = '/secrets',
    codes: readonly string[] = EVERYTHING,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [{ permissionCodes: codes }] });
    client.setQueryData(['vault-secrets', 0], { secrets: SECRETS, total: SECRETS.length });
    client.setQueryData(['vault-consumers', 0], { consumers: CONSUMERS, total: CONSUMERS.length });
    client.setQueryData(['vault-secret', 'db.marketdata_service.password'], {
        name: 'db.marketdata_service.password',
        owner: 'marketdata_service',
        key_version: 2,
        written_at: '2026-11-28 09:00:00Z',
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>{children}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the secrets list', () => {
    it('names the screen, says what it holds and lists each secret', () => {
        const html = render(<SecretsPage />);

        expect(html).toContain('Secrets');
        expect(html).toContain('The values this installation keeps for its own use.');
        expect(html).toContain('db.marketdata_service.password');
        expect(html).toContain('marketdata_service');
        expect(html).toContain('key 2');
        expect(html).toContain('ores.marketdata.vendor.api_key');
    });

    it('says that it shows what the vault holds and never a value', () => {
        const html = render(<SecretsPage />);

        expect(html).toContain('Every value is sealed.');
        expect(html).toContain('a value leaves the vault only through an ask it records');
    });

    it('draws Add for a person who holds the write, and not for one who does not', () => {
        const writer = render(<SecretsPage />, '/secrets', EVERYTHING);
        const reader = render(<SecretsPage />, '/secrets', ['vault::secrets:read']);

        expect(writer).toContain('Add');
        // Not drawn disabled: an action somebody cannot take is not drawn at all.
        expect(reader).not.toContain('>Add<');
        expect(reader).toContain('Consumers');
    });

    it('draws a vault with no secret as a sentence and not as a failure', () => {
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        client.setQueryData(['my-access'], { roles: [{ permissionCodes: EVERYTHING }] });
        client.setQueryData(['vault-secrets', 0], { secrets: [], total: 0 });
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter initialEntries={['/secrets']}>
                        <SecretsPage />
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('The installation holds no secret yet.');
    });
});

describe('a secret record', () => {
    const record = (codes: readonly string[] = EVERYTHING): string =>
        render(
            <Routes>
                <Route path="/secrets/:name" element={<SecretPage />} />
            </Routes>,
            '/secrets/db.marketdata_service.password',
            codes,
        );

    it('names the secret and states the key, the owner and when it was written', () => {
        const html = record();

        expect(html).toContain('db.marketdata_service.password');
        expect(html).toContain('marketdata_service');
        expect(html).toContain('Key version');
        expect(html).toContain('Written');
    });

    it('masks the value and offers the one control that asks for it', () => {
        const html = record();

        expect(html).toContain('••••••••••••');
        expect(html).toContain('Reveal');
        expect(html).toContain('Masked until somebody asks for it');
    });

    it('draws the ask only for a person who may make it', () => {
        const html = record(['vault::secrets:read']);

        expect(html).not.toContain('>Reveal<');
        expect(html).toContain('You do not hold vault::secrets:resolve');
    });

    it('draws Change and Delete for a person who holds each, and neither otherwise', () => {
        const reader = record(['vault::secrets:read', 'vault::secrets:resolve']);
        const writer = record(['vault::secrets:read', 'vault::secrets:write']);

        expect(reader).not.toContain('>Change<');
        expect(reader).not.toContain('>Delete<');
        expect(writer).toContain('Change');
        expect(writer).not.toContain('vault::secrets:delete');
    });
});

describe('the consumers list', () => {
    it('names each consumer, its subject, its key and whether the vault will use it', () => {
        const html = render(<ConsumersPage />);

        expect(html).toContain('Consumers');
        expect(html).toContain('marketdata_service');
        expect(html).toContain('marketdata_service@system.ores');
        expect(html).toContain('8f2c0d4e6a1b…');
        expect(html).toContain('enabled');
        expect(html).toContain('disabled');
    });

    it('says that a consumer registers itself, because no screen registers one', () => {
        const html = render(<ConsumersPage />);

        expect(html).toContain('No screen registers one for somebody else.');
    });
});
