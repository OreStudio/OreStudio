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
 * End-to-end verification of the TypeScript protocol layer against the running
 * C++ services.
 *
 * Every step asserts against the real bus. The first assertion is deliberately a
 * rejected login, because a server that decoded the msgpack body answers with a
 * message while a server that did not decode it never replies at all. So a
 * well-formed rejection already proves the subject, the encoding, and the
 * response schema.
 *
 * Prerequisites:
 *   scripts/dev-stack.sh start           (NATS, IAM and refdata in another shell)
 *   scripts/seed-test-account.sh         (creates the login used below)
 *
 * Run:
 *   npx tsx scripts/verify-login.ts
 *
 * Exit code 0 means every assertion held.
 */

import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  NotAuthenticatedError,
  OresClient,
  NatsTransport,
  isUuid,
  isWireTimestamp,
  type ActiveSession,
  type LoginOutcome,
  type PartySelectionRequired,
} from '../packages/wire-protocol/src/index.js';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..', '..');
const ENV_FILE = resolve(REPO_ROOT, '.env');
const KEYS_DIR = resolve(REPO_ROOT, 'build', 'keys', 'nats');

// The checkout's environment file is the authority. A variable already in the
// process environment wins, as it does for the BFF started with --env-file.
try {
  process.loadEnvFile(ENV_FILE);
} catch {
  // A checkout without one falls back to the defaults below.
}

const NATS_URL =
  setting('ORES_NATS_URL') ?? `nats://localhost:${setting('ORES_NATS_PORT') ?? '4222'}`;
const SUBJECT_PREFIX = setting('ORES_NATS_SUBJECT_PREFIX');
const TLS_CA = setting('ORES_NATS_TLS_CA') ?? resolve(KEYS_DIR, 'ca.crt');
const TLS_CERT = setting('ORES_NATS_TLS_CERT') ?? resolve(KEYS_DIR, 'ores.shell.crt');
const TLS_KEY = setting('ORES_NATS_TLS_KEY') ?? resolve(KEYS_DIR, 'ores.shell.key');
const PRINCIPAL = process.env['ORES_PRINCIPAL'] ?? 'ores_web_probe';
const PASSWORD = process.env['ORES_PASSWORD'] ?? 'Secure-Password-123';

let failures = 0;

function check(label: string, condition: boolean, detail = ''): void {
  const status = condition ? 'PASS' : 'FAIL';
  if (!condition) {
    failures += 1;
  }
  console.log(`  [${status}] ${label}${detail.length > 0 ? ` (${detail})` : ''}`);
}

/** Reads a variable, treating whitespace as absent. */
function setting(name: string): string | undefined {
  const value = process.env[name];
  return value !== undefined && value.trim().length > 0 ? value : undefined;
}

function pem(path: string): string {
  return readFileSync(path, 'utf8');
}

interface VerificationClient {
  readonly client: OresClient;
  readonly transport: NatsTransport;
}

function makeClient(): VerificationClient {
  if (SUBJECT_PREFIX === undefined || SUBJECT_PREFIX.length === 0) {
    throw new Error(`no ORES_NATS_SUBJECT_PREFIX in ${ENV_FILE}`);
  }
  const transport = new NatsTransport({
    server: NATS_URL,
    subjectPrefix: SUBJECT_PREFIX,
    tls: {
      ca: pem(TLS_CA),
      cert: pem(TLS_CERT),
      key: pem(TLS_KEY),
    },
    name: 'ores.web.verify',
  });
  return {
    client: new OresClient({ transport, format: 'msgpack' }),
    transport,
  };
}

/** Asserts a rejected login reaches the client as a structured failure. */
async function verifyRejectedLogin(client: OresClient): Promise<void> {
  console.log('\nrejected login (proves the wire format round-trips):');
  const outcome = await client.login({
    principal: 'no-such-account',
    password: 'definitely-wrong',
  });
  check('server answered in our schema', outcome.kind === 'rejected');
  if (outcome.kind === 'rejected') {
    check('server sent a human-readable reason', outcome.message.length > 0, outcome.message);
  }
}

/** Asserts the authenticated surface works, selecting a party when offered. */
async function verifyAuthenticatedSurface(
  client: OresClient,
  outcome: LoginOutcome,
): Promise<ActiveSession | null> {
  if (outcome.kind === 'rejected') {
    check('login accepted', false, outcome.message);
    return null;
  }

  let session: ActiveSession;
  if (outcome.kind === 'party-selection-required') {
    session = await selectParty(client, outcome);
  } else {
    session = outcome;
  }

  check('session has a party', session.party.id.length > 0, session.party.name);
  check('token lifetime is positive', session.accessLifetimeSeconds > 0);

  await verifyAccounts(client);
  await verifyUnauthenticatedRefusal();
  return session;
}

async function selectParty(
  client: OresClient,
  outcome: PartySelectionRequired,
): Promise<ActiveSession> {
  console.log(`\nparty selection: ${outcome.availableParties.length} parties offered`);
  check('more than one party was offered', outcome.availableParties.length > 1);

  // Prefer the account's stored default, which is the branch a real client
  // takes when the user has opted into quick login.
  const chosen =
    outcome.availableParties.find((party) => party.id === outcome.defaultPartyId) ??
    outcome.availableParties[0];
  if (chosen === undefined) {
    throw new Error('server offered no parties to select');
  }

  console.log(`  selecting ${chosen.name} (${chosen.partyCategory})`);
  const session = await client.selectParty({ partyId: chosen.id, expected: outcome });
  check('select-party issued a new token', session.token.length > 0);
  check(
    'the selected party matches the request',
    session.party.id === chosen.id,
    session.party.name,
  );
  return session;
}

async function verifyAccounts(client: OresClient): Promise<void> {
  console.log('\naccounts.list:');
  const page = await client.listAccounts({ offset: 0, limit: 5 });
  check('a page came back', Array.isArray(page.accounts));
  check('total count is positive', page.totalCount > 0, String(page.totalCount));
  check('the page carries every row the server reported', page.accounts.length === page.totalCount);
  // The C++ list handler decodes offset and limit and then calls
  // list_accounts() with no arguments, so the request is accepted but not
  // applied. Assert the behaviour we can rely on rather than the behaviour we
  // wish for, so a future server-side fix shows up as a changed expectation.
  console.log(
    `  note: server returned ${page.accounts.length} rows for limit=5; pagination is not applied server-side`,
  );

  const first = page.accounts[0];
  if (first !== undefined) {
    console.log(`  first account: ${first.username} <${first.email}> type=${first.accountType}`);
    check('account id parses as a UUID', isUuid(first.id));
    check('recordedAt parses as a wire timestamp', isWireTimestamp(first.recordedAt));
    check(
      'credential fields are absent',
      !Object.keys(first).some(
        (key) => key.toLowerCase().includes('password') || key.includes('totp'),
      ),
    );
  } else {
    check('at least one account in the page', false);
  }
}

/** Asserts the client refuses an authenticated call before login. */
async function verifyUnauthenticatedRefusal(): Promise<void> {
  console.log('\nunauthenticated guard:');
  const { client: fresh } = makeClient();
  try {
    await fresh.listAccounts({ limit: 1 });
    check('listAccounts without a session is refused', false);
  } catch (error) {
    check('listAccounts without a session is refused', error instanceof NotAuthenticatedError);
  } finally {
    await fresh.close();
  }
}

async function main(): Promise<number> {
  const { client, transport } = makeClient();
  await transport.connect();
  console.log('connected: mTLS handshake complete');
  console.log(`subject prefix: ${transport.absoluteSubject('<relative>')}`);

  await verifyRejectedLogin(client);

  console.log(`\nlogin as ${PRINCIPAL}:`);
  const outcome = await client.login({ principal: PRINCIPAL, password: PASSWORD });
  const session = await verifyAuthenticatedSurface(client, outcome);

  if (session !== null) {
    const baseUrl = await client.discoverHttpBaseUrl();
    console.log('\ncompanion http server:');
    console.log(`  base url: ${baseUrl ?? '(not discovered; the service may be down)'}`);
  }

  await client.close();
  console.log(`\n${failures === 0 ? 'ALL CHECKS PASSED' : `${failures} CHECK(S) FAILED`}`);
  return failures === 0 ? 0 : 1;
}

main().then(
  (code) => process.exit(code),
  (error: unknown) => {
    console.error('\nverification aborted:', error);
    process.exit(1);
  },
);
