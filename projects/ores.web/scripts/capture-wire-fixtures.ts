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

/**
 * Captures the wire payloads the web's golden test parses.
 *
 * Run against a live fleet:
 *
 *   compass services start
 *   npx tsx scripts/capture-wire-fixtures.ts
 *
 * It writes the raw msgpack reply for `iam.v1.accounts.list` and
 * `iam.v1.auth_events.list` into `packages/wire-protocol/src/fixtures/`, as
 * decoded JSON. The fixture is the fact the web parser is tested against: a
 * change to the server's timestamp serializer changes the captured bytes, and
 * the web's test then fails until it is recaptured and the parser is shown to
 * agree. A value this script invented would prove only that the parser agrees
 * with itself.
 */

import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
import { OresClient } from '../packages/wire-protocol/src/client.js';
import { NatsTransport } from '../packages/wire-protocol/src/transport.js';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..', '..');
process.loadEnvFile(resolve(REPO_ROOT, '.env'));

const KEYS_DIR = resolve(REPO_ROOT, 'build', 'keys', 'nats');
const FIXTURES = resolve(
    dirname(fileURLToPath(import.meta.url)),
    '..',
    'packages',
    'wire-protocol',
    'src',
    'fixtures',
);
const NATS_URL = process.env['ORES_NATS_URL'] ?? 'nats://localhost:4222';
const SUBJECT_PREFIX = process.env['ORES_NATS_SUBJECT_PREFIX'] ?? '';

function makeClient(): { client: OresClient; transport: NatsTransport } {
    const transport = new NatsTransport({
        server: NATS_URL,
        subjectPrefix: SUBJECT_PREFIX,
        tls: {
            ca: readFileSync(resolve(KEYS_DIR, 'ca.crt'), 'utf8'),
            cert: readFileSync(resolve(KEYS_DIR, 'ores.shell.crt'), 'utf8'),
            key: readFileSync(resolve(KEYS_DIR, 'ores.shell.key'), 'utf8'),
        },
        name: 'ores.web.capture',
    });
    return { client: new OresClient({ transport, format: 'msgpack' }), transport };
}

async function signIn(client: OresClient, principal: string, password: string): Promise<void> {
    const outcome = await client.login({ principal, password });
    if (outcome.kind === 'rejected') {
        throw new Error(`login rejected for ${principal}: ${outcome.message}`);
    }
    if (outcome.kind === 'party-selection-required') {
        const chosen = outcome.availableParties[0];
        if (chosen === undefined) throw new Error(`${principal} was offered no party`);
        await client.selectParty({ partyId: chosen.id, expected: outcome });
    }
}

function writeFixture(name: string, payload: unknown): void {
    mkdirSync(FIXTURES, { recursive: true });
    writeFileSync(resolve(FIXTURES, name), `${JSON.stringify(payload, null, 4)}\n`, 'utf8');
    console.log(`captured ${name}`);
}

const accounts = makeClient();
await accounts.transport.connect();
await signIn(
    accounts.client,
    process.env['ORES_PRINCIPAL'] ?? 'adrian.vance@acme_corporation',
    process.env['ORES_PASSWORD'] ?? 'Secure-Password-123',
);
writeFixture(
    'live-accounts-page.json',
    await accounts.client.callAuthenticated(
        'iam.v1.accounts.list',
        {
            offset: 0,
            limit: 3,
            order: { field: '', descending: false },
            filter: null,
            as_of: null,
        },
        z.any(),
    ),
);
await accounts.client.close();

const events = makeClient();
await events.transport.connect();
await signIn(
    events.client,
    process.env['ORES_AUDIT_PRINCIPAL'] ??
        process.env['ORES_SHELL_LOGIN_USERNAME'] ??
        'tenant_admin@acme_corporation',
    process.env['ORES_AUDIT_PASSWORD'] ??
        process.env['ORES_SHELL_LOGIN_PASSWORD'] ??
        'Secure-Password-123',
);
writeFixture(
    'live-auth-events.json',
    await events.client.callAuthenticated(
        'iam.v1.auth_events.list',
        {
            account_id: '',
            event_type: '',
            from_time: '',
            to_time: '',
            limit: 3,
            offset: 0,
        },
        z.any(),
    ),
);
await events.client.close();
