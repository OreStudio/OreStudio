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
import { OresClient } from './client.js';
import { NotAuthenticatedError } from './errors.js';
import { WireCodec } from './codec.js';
import type { Reply, RequestHeaders, Transport } from './transport.js';

interface RecordedCall {
  readonly subject: string;
  readonly body: Uint8Array;
  readonly headers: RequestHeaders;
}

interface ScriptedReply {
  readonly body?: unknown;
  readonly headers?: Record<string, string>;
  readonly error?: unknown;
}

/** A transport that answers from a script and records what it received. */
class ScriptedTransport implements Transport {
  readonly calls: RecordedCall[] = [];
  readonly #script: Map<string, ScriptedReply[]>;
  readonly #codec = new WireCodec('msgpack');
  closed = false;

  constructor(script: Record<string, ScriptedReply[]>) {
    this.#script = new Map(Object.entries(script));
  }

  async request(
    subject: string,
    body: Uint8Array,
    headers: RequestHeaders,
    _timeoutMs: number,
  ): Promise<Reply> {
    this.calls.push({ subject, body, headers });

    const queue = this.#script.get(subject);
    const next = queue?.shift();
    if (next === undefined) {
      throw new Error(`no scripted reply left for ${subject}`);
    }
    if (next.error !== undefined) {
      throw next.error;
    }
    // Responses travel in the same encoding a real server would use, so the
    // client's own decode path runs in the test.
    const encoded = next.body === undefined ? new Uint8Array() : this.#codec.encode(next.body);
    return { subject, body: encoded, headers: next.headers ?? {} };
  }

  async close(): Promise<void> {
    this.closed = true;
  }

  /** Decodes a recorded request body for assertion. */
  decodeCall(index: number): unknown {
    const call = this.calls[index];
    if (call === undefined) {
      throw new Error(`no call at index ${index}`);
    }
    return this.#codec.decode(call.body);
  }
}

function loginReply(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    success: true,
    account_id: '11111111-1111-1111-1111-111111111111',
    tenant_id: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    tenant_name: 'System',
    username: 'probe',
    email: 'probe@ores.web.test',
    password_reset_required: false,
    tenant_bootstrap_mode: false,
    party_setup_required: false,
    party_setup_warning: '',
    token: 'token-one',
    error_message: '',
    message: '',
    selected_party_id: '22222222-2222-2222-2222-222222222222',
    available_parties: [
      {
        id: '22222222-2222-2222-2222-222222222222',
        name: 'System Party',
        party_category: 'System',
        business_center_code: 'GBLO',
      },
    ],
    default_party_id: '',
    access_lifetime_s: 1800,
    session_id: '33333333-3333-3333-3333-333333333333',
    ...overrides,
  };
}

describe('OresClient login', () => {
  it('sends the credential in the principal field', async () => {
    const transport = new ScriptedTransport({ 'iam.v1.auth.login': [{ body: loginReply() }] });
    const client = new OresClient({ transport });

    await client.login({ principal: 'probe', password: 'secret' });

    expect(transport.decodeCall(0)).toEqual({ principal: 'probe', password: 'secret' });
  });

  it('sends no headers on the unauthenticated login call', async () => {
    const transport = new ScriptedTransport({ 'iam.v1.auth.login': [{ body: loginReply() }] });
    const client = new OresClient({ transport });

    await client.login({ principal: 'probe', password: 'secret' });

    expect(transport.calls[0]?.headers).toEqual({});
  });

  it('classifies a rejected login without throwing', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [
        { body: loginReply({ success: false, token: '', error_message: 'Invalid username or password' }) },
      ],
    });
    const client = new OresClient({ transport });

    const outcome = await client.login({ principal: 'probe', password: 'wrong' });

    expect(outcome).toEqual({ kind: 'rejected', message: 'Invalid username or password' });
    expect(client.hasToken).toBe(false);
  });

  it('asks for a party when the server selected none', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [{ body: loginReply({ selected_party_id: '' }) }],
    });
    const client = new OresClient({ transport });

    const outcome = await client.login({ principal: 'probe', password: 'secret' });

    expect(outcome.kind).toBe('party-selection-required');
    expect(client.hasToken).toBe(true);
  });

  it('activates the session when the server already selected a party', async () => {
    const transport = new ScriptedTransport({ 'iam.v1.auth.login': [{ body: loginReply() }] });
    const client = new OresClient({ transport });

    const outcome = await client.login({ principal: 'probe', password: 'secret' });

    expect(outcome.kind).toBe('active');
    if (outcome.kind === 'active') {
      expect(outcome.party.name).toBe('System Party');
      expect(outcome.sessionId).toBe('33333333-3333-3333-3333-333333333333');
    }
  });
});

describe('OresClient authenticated calls', () => {
  const accountsReply = {
    accounts: [],
    total_available_count: 0,
  };

  it('refuses an authenticated call before login', async () => {
    const transport = new ScriptedTransport({});
    const client = new OresClient({ transport });

    await expect(client.listAccounts()).rejects.toBeInstanceOf(NotAuthenticatedError);
    expect(transport.calls).toHaveLength(0);
  });

  it('carries the bearer token and a correlation id on each call', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [{ body: loginReply() }],
      'iam.v1.accounts.list': [{ body: accountsReply }],
    });
    const client = new OresClient({ transport });
    await client.login({ principal: 'probe', password: 'secret' });

    await client.listAccounts({ limit: 5 });

    const headers = transport.calls[1]?.headers ?? {};
    expect(headers['Authorization']).toBe('Bearer token-one');
    expect(headers['Nats-Session-Id']).toBe('33333333-3333-3333-3333-333333333333');
    expect(headers['X-Workspace-Id']).toBe('aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa');
    expect(headers['Nats-Correlation-Id']).toMatch(/^[0-9a-f-]{36}$/);
  });

  it('sends offset and limit even when the caller omits them', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [{ body: loginReply() }],
      'iam.v1.accounts.list': [{ body: accountsReply }],
    });
    const client = new OresClient({ transport });
    await client.login({ principal: 'probe', password: 'secret' });

    await client.listAccounts();

    // The server does not apply defaults on a missing field, so the client
    // must always write every declared field.
    expect(transport.decodeCall(1)).toEqual({ offset: 0, limit: 100 });
  });

  it('refreshes once and retries when the server reports an expired token', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [{ body: loginReply() }],
      'iam.v1.accounts.list': [
        { headers: { 'X-Error': 'token_expired' } },
        { body: accountsReply },
      ],
      'iam.v1.auth.refresh': [
        { body: { success: true, token: 'token-two', message: '', access_lifetime_s: 1800 } },
      ],
    });
    const client = new OresClient({ transport });
    await client.login({ principal: 'probe', password: 'secret' });

    await client.listAccounts();

    const subjects = transport.calls.map((call) => call.subject);
    expect(subjects).toEqual([
      'iam.v1.auth.login',
      'iam.v1.accounts.list',
      'iam.v1.auth.refresh',
      'iam.v1.accounts.list',
    ]);
    // The retry must use the fresh token, not the expired one.
    expect(transport.calls[3]?.headers['Authorization']).toBe('Bearer token-two');
    const refreshHeaders = transport.calls[2]?.headers ?? {};
    expect(refreshHeaders['Authorization']).toBe('Bearer token-one');
  });

  it('fails the call when the refresh is refused', async () => {
    const transport = new ScriptedTransport({
      'iam.v1.auth.login': [{ body: loginReply() }],
      'iam.v1.accounts.list': [{ headers: { 'X-Error': 'token_expired' } }],
      'iam.v1.auth.refresh': [{ body: { success: false, token: '', message: 'max_session_exceeded' } }],
    });
    const client = new OresClient({ transport });
    await client.login({ principal: 'probe', password: 'secret' });

    await expect(client.listAccounts()).rejects.toMatchObject({
      name: 'SessionExpiredError',
      code: 'max_session_exceeded',
    });
  });
});
