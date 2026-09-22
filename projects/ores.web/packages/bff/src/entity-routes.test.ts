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

import Fastify, { type FastifyInstance } from 'fastify';
import { beforeEach, describe, expect, it } from 'vitest';
import { registerEntityRoutes, type EntityRouteDescriptor } from './entity-routes.js';
import type { LiveSession } from './sessions.js';

/**
 * What the factory sent, and what it was answered with.
 *
 * The subject and body are captured rather than the reply, because the shape of
 * the request is what these tests are about: the protocol states four
 * envelopes and a screen that states a different one is refused by a service
 * the tests cannot see.
 */
interface Sent {
  readonly subject: string;
  readonly body: Record<string, unknown>;
}

let sent: Sent[] = [];

/** The reply a service would give, per subject. */
let replies: Record<string, unknown> = {};

const client = {
  callAuthenticated: (
    subject: string,
    body: unknown,
    _schema: unknown,
  ): Promise<unknown> => {
    sent.push({ subject, body: body as Record<string, unknown> });
    return Promise.resolve(replies[subject] ?? {});
  },
} as unknown as LiveSession['client'];

const session = { client } as unknown as LiveSession;

const requireSession = (): LiveSession => session;

/**
 * A descriptor for an entity whose key is one field, which is most of them.
 */
function singleKeyDescriptor(): EntityRouteDescriptor {
  return {
    component: 'iam',
    entity: 'tenant',
    collection: 'tenants',
    keyFields: ['code'],
    subjects: {
      list: 'iam.v1.tenants.list',
      get: 'iam.v1.tenants.get',
      save: 'iam.v1.tenants.put',
      remove: 'iam.v1.tenants.delete',
      history: 'iam.v1.tenants_versions.list',
    },
    rowsField: 'tenants',
    getRowField: 'tenant',
    historyRowsField: 'versions',
  };
}

/**
 * A descriptor for an entity whose key is the pair it links.
 *
 * This is the case the path could not state before: a junction names two
 * values, and stating one of them would address half a row.
 */
function pairedKeyDescriptor(): EntityRouteDescriptor {
  return {
    component: 'iam',
    entity: 'account_party',
    collection: 'account_parties',
    keyFields: ['account_id', 'party_id'],
    subjects: {
      list: 'iam.v1.account_parties.list',
      get: 'iam.v1.account_parties.get',
      save: 'iam.v1.account_parties.put',
      remove: 'iam.v1.account_parties.delete',
    },
    rowsField: 'account_parties',
    getRowField: 'account_party',
  };
}

async function withServer(
  descriptor: EntityRouteDescriptor,
): Promise<FastifyInstance> {
  const server = Fastify();
  server.addHook('onRequest', (request, _reply, done) => {
    // The factory's own guard reads the session; nothing else about the
    // request is authenticated in a test.
    (request as unknown as { session: LiveSession }).session = session;
    done();
  });
  registerEntityRoutes(server, requireSession, descriptor);
  await server.ready();
  return server;
}

beforeEach(() => {
  sent = [];
  replies = {};
});

describe('the canonical envelopes', () => {
  it('states a key record for a single-record read', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({ method: 'GET', url: '/api/tenants/ACME' });
    expect(sent).toEqual([
      { subject: 'iam.v1.tenants.get', body: { key: { code: 'ACME' } } },
    ]);
    await server.close();
  });

  it('states a change and an intent for a write, and nothing else', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({
      method: 'POST',
      url: '/api/tenants',
      payload: {
        data: { code: 'ACME', name: 'Acme' },
        intent: { reason_code: 'crud_create', commentary: 'created' },
      },
    });
    expect(sent).toHaveLength(1);
    expect(sent[0]?.subject).toBe('iam.v1.tenants.put');
    expect(sent[0]?.body).toEqual({
      change: {
        write: { code: 'ACME', name: 'Acme' },
        precondition: { kind: 'must_not_exist', version: null },
      },
      intent: { reason_code: 'crud_create', commentary: 'created' },
    });
    // The record carries the entity's own members: the intent is beside it,
    // not inside it, and no audit member is sent at all.
    expect(sent[0]?.body).not.toHaveProperty('data');
    await server.close();
  });

  it('states the version a screen read as the precondition of a change', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({
      method: 'POST',
      url: '/api/tenants',
      payload: {
        data: { code: 'ACME' },
        intent: { reason_code: 'crud_update', commentary: '' },
        version: 7,
      },
    });
    expect(sent[0]?.body).toMatchObject({
      change: { precondition: { kind: 'must_match_version', version: 7 } },
    });
    await server.close();
  });

  it('states a removal and an intent for a delete', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({
      method: 'DELETE',
      url: '/api/tenants/ACME',
      payload: {
        intent: { reason_code: 'crud_delete', commentary: '' },
        version: 3,
      },
    });
    expect(sent[0]?.body).toEqual({
      removal: {
        key: { code: 'ACME' },
        precondition: { kind: 'must_match_version', version: 3 },
      },
      intent: { reason_code: 'crud_delete', commentary: '' },
    });
    await server.close();
  });

  it('states a type and an id for the one generic history request', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({ method: 'GET', url: '/api/tenants/ACME/history' });
    expect(sent[0]).toEqual({
      subject: 'iam.v1.tenants_versions.list',
      body: { entity_type: 'ores.iam.tenant', entity_id: 'ACME' },
    });
    await server.close();
  });

  it('states a page for a list', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({ method: 'GET', url: '/api/tenants?offset=0&limit=25' });
    expect(sent[0]).toEqual({
      subject: 'iam.v1.tenants.list',
      body: { offset: 0, limit: 25 },
    });
    await server.close();
  });
});

describe('a key with more than one member', () => {
  it('addresses one segment per member', async () => {
    const server = await withServer(pairedKeyDescriptor());
    await server.inject({
      method: 'GET',
      url: '/api/account_parties/ACC-1/PARTY-2',
    });
    expect(sent[0]).toEqual({
      subject: 'iam.v1.account_parties.get',
      body: { key: { account_id: 'ACC-1', party_id: 'PARTY-2' } },
    });
    await server.close();
  });

  it('states the whole pair in a removal, never half of it', async () => {
    const server = await withServer(pairedKeyDescriptor());
    await server.inject({
      method: 'DELETE',
      url: '/api/account_parties/ACC-1/PARTY-2',
      payload: { intent: { reason_code: 'crud_delete', commentary: '' } },
    });
    expect(sent[0]?.body).toMatchObject({
      removal: {
        key: { account_id: 'ACC-1', party_id: 'PARTY-2' },
        precondition: { kind: 'any', version: null },
      },
    });
    await server.close();
  });

  it('serves no history route at all', async () => {
    const server = await withServer(pairedKeyDescriptor());
    const response = await server.inject({
      method: 'GET',
      url: '/api/account_parties/ACC-1/PARTY-2/history',
    });
    // The generic request names one id and cannot state a pair, so the route is
    // absent rather than one that reaches nothing.
    expect(response.statusCode).toBe(404);
    expect(sent).toEqual([]);
    await server.close();
  });
});
