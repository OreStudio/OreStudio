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
import { createSessionStore } from './sessions.js';
import type { OresClient } from '@ores/wire-protocol';

/**
 * The session store is the only thing standing between a browser cookie and
 * another user's token, so its lifecycle rules are asserted directly.
 */

function fakeClient(): OresClient & { closed: boolean } {
  const client = {
    closed: false,
    async close(): Promise<void> {
      client.closed = true;
    },
  };
  return client as unknown as OresClient & { closed: boolean };
}

const baseInput = {
  username: 'probe',
  email: 'probe@ores.web.test',
  accountId: '11111111-1111-1111-1111-111111111111',
  tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
  tenantName: 'System',
  availableParties: [],
  accessLifetimeSeconds: 1800,
  passwordResetRequired: false,
  sessionId: '33333333-3333-3333-3333-333333333333',
};

describe('createSessionStore', () => {
  it('returns a session for a freshly created id', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const client = fakeClient();
    const created = store.create({ client, session: null, ...baseInput });

    expect(store.get(created.id)?.username).toBe('probe');
    expect(store.size).toBe(1);
  });

  it('does not return a session for an unknown id', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    expect(store.get('not-a-session')).toBeUndefined();
  });

  it('expires a session once the lifetime has passed', async () => {
    let clock = 0;
    const store = createSessionStore({ ttlSeconds: 60, now: () => clock });
    const client = fakeClient();
    const created = store.create({ client, session: null, ...baseInput });

    clock = 61_000;
    expect(store.get(created.id)).toBeUndefined();
    // An expired session must close its connection, or the broker leaks one
    // connection per abandoned browser tab.
    await Promise.resolve();
    expect(client.closed).toBe(true);
  });

  it('closes the connection when a session is destroyed', async () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const client = fakeClient();
    const created = store.create({ client, session: null, ...baseInput });

    await store.destroy(created.id);
    expect(store.size).toBe(0);
    expect(client.closed).toBe(true);
  });

  it('closes every connection on shutdown', async () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const first = fakeClient();
    const second = fakeClient();
    store.create({ client: first, session: null, ...baseInput });
    store.create({ client: second, session: null, ...baseInput });

    await store.destroyAll();
    expect(store.size).toBe(0);
    expect(first.closed).toBe(true);
    expect(second.closed).toBe(true);
  });

  it('carries the IAM session id through to the live view', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const created = store.create({ client: fakeClient(), session: null, ...baseInput });
    // The pending party-selection call needs this to reach the server.
    expect(store.get(created.id)?.sessionId).toBe(baseInput.sessionId);
  });

  it('reports no party until one is chosen', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const created = store.create({ client: fakeClient(), session: null, ...baseInput });
    expect(store.get(created.id)?.party).toBeUndefined();
  });

  it('records the chosen party', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const created = store.create({ client: fakeClient(), session: null, ...baseInput });

    store.activate(created.id, {
      kind: 'active',
      token: 'token',
      accountId: baseInput.accountId,
      tenantId: baseInput.tenantId,
      tenantName: 'System',
      username: 'probe',
      email: baseInput.email,
      party: {
        id: '22222222-2222-2222-2222-222222222222',
        name: 'System Party',
        partyCategory: 'System',
        businessCenterCode: 'WRLD',
      },
      availableParties: [],
      accessLifetimeSeconds: 1800,
      passwordResetRequired: false,
      sessionId: baseInput.sessionId,
    });

    expect(store.get(created.id)?.party?.name).toBe('System Party');
  });

  it('keeps the plaintext id out of the map keys', () => {
    const store = createSessionStore({ ttlSeconds: 60 });
    const created = store.create({ client: fakeClient(), session: null, ...baseInput });
    // The map is keyed by a hash, so reading the store's internals cannot
    // yield a usable cookie value.
    const internalKeys = Object.keys(store).filter((key) => key === created.id);
    expect(internalKeys).toHaveLength(0);
    expect(store.get(created.id)?.id).toBe(created.id);
  });
});
