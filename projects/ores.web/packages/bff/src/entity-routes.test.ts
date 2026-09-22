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
import {
  MINTED_WRITE_DEFAULT,
  registerEntityRoutes,
  type EntityRouteDescriptor,
} from './entity-routes.js';
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
    writeFields: ['id', 'code', 'name'],
    writeDefaults: { id: MINTED_WRITE_DEFAULT, code: '', name: '' },
    listHasAsOf: false,
    listHasFilter: false,
    versionsHasFilter: false,
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
 * A descriptor whose list request declares the optional members, and whose
 * write record mints its surrogate key.
 */
function filteredDescriptor(): EntityRouteDescriptor {
  return {
    ...singleKeyDescriptor(),
    collection: 'account_contact_informations',
    keyFields: ['email'],
    writeFields: ['id', 'email'],
    writeDefaults: { id: MINTED_WRITE_DEFAULT, email: '' },
    listHasAsOf: true,
    listHasFilter: true,
    versionsHasFilter: true,
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
    listHasAsOf: false,
    listHasFilter: false,
    versionsHasFilter: false,
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
    const change = sent[0]?.body['change'] as { write: Record<string, unknown> };
    expect(change.write['code']).toBe('ACME');
    expect(change.write['name']).toBe('Acme');
    expect(sent[0]?.body['change']).toMatchObject({
      precondition: { kind: 'must_not_exist', version: null },
    });
    expect(sent[0]?.body['intent']).toEqual({
      reason_code: 'crud_create',
      commentary: 'created',
    });
    // The record carries the entity's own members: the intent is beside it,
    // not inside it, and no audit member is sent at all.
    expect(sent[0]?.body).not.toHaveProperty('data');
    await server.close();
  });

  it('states every member of the write record, minting the key', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({
      method: 'POST',
      url: '/api/tenants',
      payload: {
        data: { code: 'ACME', name: 'Acme' },
        intent: { reason_code: 'crud_create', commentary: '' },
      },
    });
    // The wire format states every member a record declares, so a member the
    // form did not carry takes the model's default, and the surrogate key the
    // caller has to name gets a fresh identifier.
    const write = (sent[0]?.body['change'] as { write: Record<string, unknown> })
      .write;
    expect(Object.keys(write).sort()).toEqual(['code', 'id', 'name']);
    expect(write['id']).toMatch(/^[0-9a-f-]{36}$/);
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

  it('answers a refused write with the service own words', async () => {
    const server = await withServer(singleKeyDescriptor());
    replies = {
      'iam.v1.tenants.put': {
        result: { outcome: 'conflicting', message: 'the version is stale' },
      },
    };
    const response = await server.inject({
      method: 'POST',
      url: '/api/tenants',
      payload: {
        data: { code: 'ACME' },
        intent: { reason_code: 'crud_update', commentary: '' },
        version: 7,
      },
    });
    expect(response.statusCode).toBe(409);
    expect(response.json()).toEqual({ message: 'the version is stale' });
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

  it('states the key and a page for the history request', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({ method: 'GET', url: '/api/tenants/ACME/history' });
    // The versions request addresses the entity's own key record, so the
    // address and the request carry one key rather than two spellings of it.
    // The order is the one order the store pages in.
    expect(sent[0]).toEqual({
      subject: 'iam.v1.tenants_versions.list',
      body: {
        key: { code: 'ACME' },
        offset: 0,
        limit: 500,
        order: { field: '', descending: false },
      },
    });
    await server.close();
  });

  it('answers a history newest first', async () => {
    const server = await withServer(singleKeyDescriptor());
    replies = {
      'iam.v1.tenants_versions.list': {
        result: { outcome: 'ok', message: '' },
        versions: [{ version: 1 }, { version: 2 }, { version: 3 }],
      },
    };
    const response = await server.inject({
      method: 'GET',
      url: '/api/tenants/ACME/history',
    });
    expect(response.json()).toEqual({
      versions: [{ version: 3 }, { version: 2 }, { version: 1 }],
      message: '',
    });
    await server.close();
  });

  it('states a page for a list', async () => {
    const server = await withServer(singleKeyDescriptor());
    await server.inject({ method: 'GET', url: '/api/tenants?offset=0&limit=25' });
    // The order travels with the page: the service's list request declares
    // it, and a payload without it does not decode.
    expect(sent[0]).toEqual({
      subject: 'iam.v1.tenants.list',
      body: {
        offset: 0,
        limit: 25,
        order: { field: '', descending: false },
      },
    });
    await server.close();
  });

  it('states the optional members the list request declares', async () => {
    const server = await withServer(filteredDescriptor());
    await server.inject({
      method: 'GET',
      url: '/api/account_contact_informations?offset=0&limit=25',
    });
    // A member the request declares is stated even when it carries nothing:
    // the wire format refuses a message with a member absent.
    expect(sent[0]?.body).toEqual({
      offset: 0,
      limit: 25,
      order: { field: '', descending: false },
      as_of: null,
      filter: null,
    });
    await server.close();
  });

  it('answers the list total from the canonical response', async () => {
    const server = await withServer(singleKeyDescriptor());
    replies = {
      'iam.v1.tenants.list': {
        result: { outcome: 'ok', message: '' },
        tenants: [{ code: 'ACME' }],
        total: 59,
      },
    };
    const response = await server.inject({ method: 'GET', url: '/api/tenants' });
    expect(response.json()).toEqual({ rows: [{ code: 'ACME' }], totalCount: 59 });
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
