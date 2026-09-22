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
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import { registerEntityRoutes, type EntityRouteDescriptor } from './entity-routes.js';
import { countryRoute } from './generated/refdata/country_route.js';
import { tenantRoute } from './generated/iam/tenant_route.js';
import type { LiveSession } from './sessions.js';

/**
 * The factory is the one place an entity's requests are built and its replies
 * read, so these cases pin the canonical envelope for every verb against the
 * descriptors codegen emits, rather than against a hand-built shape that could
 * agree with the factory and with nothing else.
 *
 * The two descriptors are the interesting pair: country is a lookup whose key
 * is its natural key, and tenant carries a surrogate key, so only country can
 * drive the routes that address one row.
 */

interface Call {
  readonly subject: string;
  readonly payload: Record<string, unknown>;
}

/** A server with one entity's routes and the calls it made to the client. */
function harness(
  descriptor: EntityRouteDescriptor,
  answer: (call: Call) => unknown,
): { readonly server: FastifyInstance; readonly calls: Call[] } {
  const calls: Call[] = [];
  const client = {
    callAuthenticated: async (subject: string, payload: Record<string, unknown>) => {
      const call = { subject, payload };
      calls.push(call);
      return answer(call);
    },
  } as unknown as OresClient;
  const server = Fastify();
  registerEntityRoutes(
    server,
    () => ({ client, tenantId: 'tenant' }) as unknown as LiveSession,
    descriptor,
  );
  return { server, calls };
}

/** The ok envelope every canonical response carries. */
const ok = { result: { outcome: 'ok', code: '', message: '', fields: [] } };

describe('the list route', () => {
  it('sends the window, the order and the as-of the entity declares', async () => {
    const { server, calls } = harness(countryRoute, () => ({
      ...ok,
      countries: [{ alpha2_code: 'US' }],
      total: 3,
    }));

    const response = await server.inject({
      method: 'GET',
      url: '/api/countries?offset=25&limit=25&asOf=2026-01-01',
    });

    expect(response.statusCode).toBe(200);
    expect(calls[0]?.payload).toEqual({
      offset: 25,
      limit: 25,
      order: { field: '', descending: false },
      as_of: '2026-01-01',
    });
    // The rows and the total are what the browser's page shape needs, and the
    // total is the canonical member rather than the retired one.
    expect(response.json()).toEqual({
      rows: [{ alpha2_code: 'US' }],
      totalCount: 3,
    });
  });

  it('asks for the present when no window was chosen', async () => {
    const { server, calls } = harness(countryRoute, () => ({ ...ok, countries: [], total: 0 }));

    await server.inject({ method: 'GET', url: '/api/countries?offset=0&limit=25' });

    expect(calls[0]?.payload['as_of']).toBeNull();
  });

  it('omits the window an entity whose request carries none', async () => {
    const { server, calls } = harness(tenantRoute, () => ({
      ...ok,
      tenants: [],
      total: 0,
    }));

    await server.inject({ method: 'GET', url: '/api/tenants?offset=0&limit=25' });

    expect('as_of' in (calls[0]?.payload ?? {})).toBe(false);
  });
});

describe('the single-record read', () => {
  it('names the row through its key record', async () => {
    const { server, calls } = harness(countryRoute, () => ({
      ...ok,
      country: { alpha2_code: 'US' },
    }));

    const response = await server.inject({ method: 'GET', url: '/api/countries/US' });

    expect(calls[0]?.subject).toBe('refdata.v1.countries.get');
    expect(calls[0]?.payload).toEqual({ key: { alpha2_code: 'US' } });
    expect(response.json()).toEqual({ row: { alpha2_code: 'US' } });
  });

  it('does not exist for an entity whose key record cannot be filled by the path', async () => {
    const { server } = harness(tenantRoute, () => ok);

    const response = await server.inject({ method: 'GET', url: '/api/tenants/ACME' });

    expect(response.statusCode).toBe(404);
  });
});

describe('the save route', () => {
  it('writes the record the model states, with a default for every member the form does not carry', async () => {
    const { server, calls } = harness(countryRoute, () => ({ ...ok, country: {} }));

    const response = await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: {
        mode: 'create',
        data: {
          version: 0,
          alpha2_code: 'GB',
          alpha3_code: 'GBR',
          numeric_code: '826',
          name: 'United Kingdom',
          official_name: 'United Kingdom of Great Britain and Northern Ireland',
          change_reason_code: 'system.import',
          change_commentary: 'loaded',
        },
      },
    });

    expect(response.statusCode).toBe(200);
    expect(calls[0]?.subject).toBe('refdata.v1.countries.put');
    expect(calls[0]?.payload).toEqual({
      change: {
        // image_id and coding_scheme_code are write members the form does not
        // show, and the model says both are optional.
        write: {
          alpha2_code: 'GB',
          alpha3_code: 'GBR',
          numeric_code: '826',
          name: 'United Kingdom',
          official_name: 'United Kingdom of Great Britain and Northern Ireland',
          image_id: null,
          coding_scheme_code: null,
        },
        precondition: { kind: 'must_not_exist', version: null },
      },
      intent: { reason_code: 'system.import', commentary: 'loaded' },
    });
  });

  it('mints the surrogate key the form does not carry', async () => {
    const { server, calls } = harness(tenantRoute, () => ({ ...ok, tenant: {} }));

    await server.inject({
      method: 'POST',
      url: '/api/tenants',
      payload: {
        mode: 'create',
        data: {
          version: 0,
          code: 'acme',
          name: 'Acme',
          type: 'internal',
          hostname: 'acme.example',
          status: 'Active',
        },
      },
    });

    const change = (calls[0]?.payload['change'] ?? {}) as Record<string, unknown>;
    const write = (change['write'] ?? {}) as Record<string, unknown>;
    expect(write['id']).toMatch(
      /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/,
    );
    expect(write['description']).toBeNull();
  });

  it('states the version an amend was made against', async () => {
    const { server, calls } = harness(countryRoute, () => ({ ...ok, country: {} }));

    await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: {
        mode: 'amend',
        data: { version: 3, alpha2_code: 'GB', name: 'United Kingdom' },
      },
    });

    const change = (calls[0]?.payload['change'] ?? {}) as Record<string, unknown>;
    expect(change['precondition']).toEqual({ kind: 'must_match_version', version: 3 });
  });

  it('refuses a save that does not state which question it answers', async () => {
    const { server, calls } = harness(countryRoute, () => ({ ...ok, country: {} }));

    const response = await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: { data: { version: 0, alpha2_code: 'GB' } },
    });

    expect(response.statusCode).toBe(400);
    expect(calls).toHaveLength(0);
  });

  it('refuses a version it cannot read rather than calling it a create', async () => {
    const { server, calls } = harness(countryRoute, () => ({ ...ok, country: {} }));

    const malformed = await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: { mode: 'amend', data: { version: 'three', alpha2_code: 'GB' } },
    });
    const inconsistent = await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: { mode: 'create', data: { version: 5, alpha2_code: 'GB' } },
    });

    expect(malformed.statusCode).toBe(400);
    expect(inconsistent.statusCode).toBe(400);
    expect(calls).toHaveLength(0);
  });

  it('answers a refused write with the service\'s own words', async () => {
    const { server } = harness(countryRoute, () => ({
      result: { outcome: 'conflict', code: 'version_conflict', message: 'the row moved on', fields: [] },
    }));

    const response = await server.inject({
      method: 'POST',
      url: '/api/countries',
      payload: { mode: 'amend', data: { version: 1, alpha2_code: 'GB' } },
    });

    expect(response.statusCode).toBe(409);
    expect(response.json()).toEqual({ message: 'the row moved on' });
  });
});

describe('the removal route', () => {
  it('names the row through a removal and states no precondition', async () => {
    const { server, calls } = harness(countryRoute, () => ok);

    const response = await server.inject({ method: 'DELETE', url: '/api/countries/US' });

    expect(calls[0]?.subject).toBe('refdata.v1.countries.delete');
    expect(calls[0]?.payload).toEqual({
      removal: {
        key: { alpha2_code: 'US' },
        precondition: { kind: 'any', version: null },
      },
      intent: { reason_code: '', commentary: '' },
    });
    expect(response.json()).toEqual({ ok: true, message: '' });
  });

  it('does not exist for an entity whose key record cannot be filled by the path', async () => {
    const { server } = harness(tenantRoute, () => ok);

    const response = await server.inject({ method: 'DELETE', url: '/api/tenants/ACME' });

    expect(response.statusCode).toBe(404);
  });
});

describe('the versions route', () => {
  it('asks for the row\'s versions newest first', async () => {
    const { server, calls } = harness(countryRoute, () => ({
      ...ok,
      versions: [{ alpha2_code: 'US', version: 2 }],
      total: 1,
    }));

    const response = await server.inject({
      method: 'GET',
      url: '/api/countries/US/history',
    });

    expect(calls[0]?.subject).toBe('refdata.v1.countries_versions.list');
    expect(calls[0]?.payload).toEqual({
      key: { alpha2_code: 'US' },
      offset: 0,
      limit: 1000,
      order: { field: '', descending: true },
      filter: null,
    });
    expect(response.json()).toEqual({
      versions: [{ alpha2_code: 'US', version: 2 }],
      message: '',
    });
  });

  it('does not exist for an entity whose key record cannot be filled by the path', async () => {
    const { server } = harness(tenantRoute, () => ok);

    const response = await server.inject({
      method: 'GET',
      url: '/api/tenants/ACME/history',
    });

    expect(response.statusCode).toBe(404);
  });
});
