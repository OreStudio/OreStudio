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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

/** The environment this file's site configuration serves. */
const ENVIRONMENT_ID = 'eager_maxwell';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

const ok = { outcome: 'ok', code: '', message: '' };

function buildTestServer(
    replies: Readonly<Record<string, unknown>>,
    tenantId = '44444444-4444-4444-4444-444444444444',
): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: { subject: string; body: unknown }[];
    readonly publish: (subject: string, message: unknown) => void;
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const listeners = new Map<string, (message: unknown) => void>();
    const client = {
        subscribeToMessages(
            subject: string,
            schema: { parse: (value: unknown) => unknown },
            onMessage: (message: unknown) => void,
        ): () => void {
            listeners.set(subject, (message) => onMessage(schema.parse(message)));
            return () => listeners.delete(subject);
        },
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'tenant_admin',
        email: 'tenant_admin@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId,
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
    });
    return {
        server: buildServer({
            config,
            site: siteConfiguration(),
            sessions,
            createClient: () => ({ client, connect: async () => undefined }),
        }),
        sessionId: session.id,
        calls,
        publish: (subject, message) => listeners.get(subject)?.(message),
    };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        cookies: { [SESSION_COOKIE]: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const ID = '55555555-5555-4555-8555-555555555555';
const OTHER = '66666666-6666-4666-8666-666666666666';
const COLLECTION_ID = '77777777-7777-4777-8777-777777777777';
const COLLECTION = '88888888-8888-4888-8888-888888888888';
const INTENT = { reason_code: 'common.correction', commentary: '' };
const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

const COMPONENT = {
    id: ID,
    fx_spot_config_id: OTHER,
    component_index: 0,
    description: 'calm',
    mean: 0,
    stdev: 0.01,
    weight: 0.8,
};

describe('synthetic resource routes', () => {
    it('lists the resources with the permission each write needs, and the read only ones say why', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/synthetic');
        const resources = response.json().resources as {
            key: string;
            writable: boolean;
            readOnlyBecause: string | null;
            writePermission: string;
        }[];
        expect(resources.map((resource) => resource.key)).toEqual([
            'folders',
            'collections',
            'providers',
            'fx-feeds',
            'gmm-components',
            'ir-curve-feeds',
            'ir-parameter-values',
            'ir-template-entries',
            'process-types',
            'parameter-definitions',
            'tenors',
            'instrument-codes',
            'payment-frequencies',
        ]);
        const tenors = resources.find((resource) => resource.key === 'tenors');
        expect(tenors?.writable).toBe(false);
        expect(tenors?.readOnlyBecause).toContain('reference data');
        const definitions = resources.find((resource) => resource.key === 'parameter-definitions');
        expect(definitions?.writable).toBe(false);
        expect(definitions?.readOnlyBecause).toContain('name alone');
        const mixture = resources.find((resource) => resource.key === 'gmm-components');
        expect(mixture?.writePermission).toBe('synthetic::gmm_components:write');
        const providers = resources.find((resource) => resource.key === 'providers');
        expect(providers?.writable).toBe(false);
        expect(providers?.readOnlyBecause).toContain('catalogue');
    });

    it('reads every row of a resource, as of now and unfiltered', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [{ id: ID, base_currency_code: 'EUR' }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/fx-feeds');
        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toEqual([{ id: ID, base_currency_code: 'EUR' }]);
        expect(calls[0]?.body).toMatchObject({ filter: null, as_of: null, offset: 0 });
    });

    it('reads one page searched and ordered on the server, with its total', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                total: 31,
                fx_spot_generation_configs: [{ id: ID, base_currency_code: 'EUR' }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/fx-feeds?offset=15&limit=15&search=eur&sort=source_name&descending=true',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            rows: [{ id: ID, base_currency_code: 'EUR' }],
            total: 31,
        });
        expect(calls[0]?.body).toEqual({
            offset: 15,
            limit: 15,
            order: { field: 'source_name', descending: true },
            filter: { id_one_of: null, search: 'eur' },
            as_of: null,
        });
    });

    it('refuses an order or a search the model does not declare, before any call', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const order = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/fx-feeds?limit=15&sort=gmm_initial_price',
        );
        expect(order.statusCode).toBe(400);
        const search = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/gmm-components?limit=15&search=calm',
        );
        expect(search.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it("reads one feed's mixture through its parent filter", async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.gmm_components.list': { result: ok, gmm_components: [COMPONENT] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/gmm-components/by/${OTHER}`,
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toEqual([COMPONENT]);
        expect(calls[0]?.body).toMatchObject({
            filter: { fx_spot_config_id: OTHER, id_one_of: null, fx_spot_config_id_one_of: null },
        });
    });

    it('reads the template entries and the parameter values of one curve feed by its parent', async () => {
        const entry = { id: ID, ir_curve_config_id: OTHER, sequence_index: 0 };
        const value = { id: ID, config_id: OTHER, parameter_value: 0.04 };
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ir_curve_template_entries.list': {
                result: ok,
                ir_curve_template_entries: [entry],
            },
            'synthetic.v1.ir_curve_generation_config_process_parameter_values.list': {
                result: ok,
                process_parameter_values: [value],
            },
        });
        const entries = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/ir-template-entries/by/${OTHER}`,
        );
        const values = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/ir-parameter-values/by/${OTHER}`,
        );
        expect(entries.json().rows).toEqual([entry]);
        expect(values.json().rows).toEqual([value]);
        expect(calls[0]?.body).toMatchObject({
            filter: { ir_curve_config_id: OTHER, id_one_of: null, ir_curve_config_id_one_of: null },
        });
        expect(calls[1]?.body).toMatchObject({
            filter: { config_id: OTHER, id_one_of: null, config_id_one_of: null },
        });
    });

    it('answers 404 for a parent read of a resource that has no parent', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', `/api/synthetic/fx-feeds/by/${ID}`);
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(0);
    });

    it('answers 404 for a resource the table does not name', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/synthetic/stub-feeds');
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(0);
    });

    it('reads a process type by its code through the code filter', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.yield_curve_process_types.list': {
                result: ok,
                process_types: [{ code: 'vasicek', name: 'Vasicek' }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/process-types/key/vasicek',
        );
        expect(response.json().row.name).toBe('Vasicek');
        expect(calls[0]?.body).toMatchObject({ filter: { code_one_of: ['vasicek'] }, limit: 1 });
        expect(Object.keys((calls[0]?.body as { filter: object }).filter)).toEqual(['code_one_of']);
    });

    it('reads one feed with every member of its filter, because the server refuses a missing one', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [{ id: ID, config_id: COLLECTION_ID }],
            },
            'synthetic.v1.market_data_generation_configs.list': {
                result: ok,
                market_data_generation_configs: [{ id: COLLECTION_ID, provider_code: 'bloomberg' }],
            },
        });
        const response = await send(server, sessionId, 'GET', `/api/synthetic/fx-feeds/key/${ID}`);
        expect(response.statusCode).toBe(200);
        expect((calls[0]?.body as { filter: unknown }).filter).toEqual({
            id_one_of: [ID],
            search: null,
        });
    });

    it('names the provider of the collection a feed sits under, which the feed record does not carry', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ir_curve_generation_configs.list': {
                result: ok,
                ir_curve_generation_configs: [{ id: ID, config_id: COLLECTION_ID }],
            },
            'synthetic.v1.market_data_generation_configs.list': {
                result: ok,
                market_data_generation_configs: [{ id: COLLECTION_ID, provider_code: 'lseg' }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/ir-curve-feeds/key/${ID}`,
        );
        expect(response.json().row).toEqual({
            id: ID,
            config_id: COLLECTION_ID,
            provider_code: 'lseg',
        });
        expect(calls[1]?.body).toMatchObject({ filter: { id_one_of: [COLLECTION_ID] } });
    });

    it('reads a row that is not a feed without a provider, since only a feed sits under a collection', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.gmm_components.list': {
                result: ok,
                gmm_components: [{ id: ID, weight: 0.5 }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/gmm-components/key/${ID}`,
        );
        expect(response.json().row).toEqual({ id: ID, weight: 0.5 });
        expect(calls).toHaveLength(1);
    });

    it('reads a parameter definition by its id, never by the name types share', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.yield_curve_process_parameter_definitions.list': {
                result: ok,
                parameter_definitions: [{ id: ID, parameter_name: 'kappa' }],
            },
        });
        const byId = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/parameter-definitions/key/${ID}`,
        );
        expect(byId.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ filter: { id_one_of: [ID] } });

        const byName = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/parameter-definitions/key/kappa',
        );
        expect(byName.statusCode).toBe(400);
        expect(calls).toHaveLength(1);
    });

    it('answers 404 when no row has the key', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.gmm_components.list': { result: ok, gmm_components: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/gmm-components/key/${ID}`,
        );
        expect(response.statusCode).toBe(404);
    });
});

describe('synthetic writes', () => {
    it('saves a reordered mixture in one call, each row claiming the version read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.gmm_components.put_many': {
                result: ok,
                gmm_components: [
                    { ...COMPONENT, version: 4 },
                    { ...COMPONENT, id: OTHER, version: 1 },
                ],
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/gmm-components', {
            intent: INTENT,
            changes: [
                { write: COMPONENT, version: 3 },
                { write: { ...COMPONENT, id: OTHER, component_index: 1 }, version: null },
            ],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toHaveLength(2);
        expect(calls).toHaveLength(1);
        expect(calls[0]?.body).toEqual({
            changes: [
                { write: COMPONENT, precondition: { kind: 'must_match_version', version: 3 } },
                {
                    write: { ...COMPONENT, id: OTHER, component_index: 1 },
                    precondition: { kind: 'must_not_exist', version: null },
                },
            ],
            intent: INTENT,
        });
    });

    it('refuses a malformed row with 400 before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/gmm-components', {
            intent: INTENT,
            changes: [{ write: { ...COMPONENT, weight: 'heavy' }, version: null }],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it("refuses a write that states its party, which is the session's", async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/gmm-components', {
            intent: INTENT,
            changes: [
                {
                    write: { ...COMPONENT, party_id: '77777777-7777-4777-8777-777777777777' },
                    version: null,
                },
            ],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('answers 409 when a row moved on since it was read', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.gmm_components.put_many': {
                result: { outcome: 'conflict', code: '', message: '' },
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/gmm-components', {
            intent: INTENT,
            changes: [{ write: COMPONENT, version: 3 }],
        });
        expect(response.statusCode).toBe(409);
        expect(response.json().message).toContain('Reload');
    });

    it('refuses a write to a parameter definition with 403 and says why', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/synthetic/parameter-definitions',
            {
                intent: INTENT,
                changes: [{ write: { id: ID }, version: null }],
            },
        );
        expect(response.statusCode).toBe(403);
        expect(response.json().message).toContain('name alone');
        expect(calls).toHaveLength(0);
    });

    it('removes one row with the version it read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ir_curve_template_entries.delete': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/synthetic/ir-template-entries',
            {
                intent: INTENT,
                removals: [{ key: ID, version: 2 }],
            },
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toEqual({
            removal: { key: { id: ID }, precondition: { kind: 'must_match_version', version: 2 } },
            intent: INTENT,
        });
    });

    it('refuses a tenant session a write to the shared process type catalogue', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const catalogue = await send(server, sessionId, 'GET', '/api/synthetic');
        const types = (catalogue.json().resources as { key: string; writable: boolean }[]).find(
            (resource) => resource.key === 'process-types',
        );
        expect(types?.writable).toBe(false);
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/process-types', {
            intent: INTENT,
            changes: [
                {
                    write: { code: 'vasicek', name: 'Vasicek', description: '', display_order: 1 },
                    version: 1,
                },
            ],
        });
        expect(response.statusCode).toBe(403);
        expect(response.json().message).toContain('only the system tenant');
        expect(calls).toHaveLength(0);
    });

    it('removes a process type by its code from a system session', async () => {
        const { server, sessionId, calls } = buildTestServer(
            { 'synthetic.v1.yield_curve_process_types.delete': { result: ok } },
            SYSTEM_TENANT,
        );
        const response = await send(server, sessionId, 'DELETE', '/api/synthetic/process-types', {
            intent: INTENT,
            removals: [{ key: 'vasicek' }],
        });
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            removal: { key: { code: 'vasicek' }, precondition: { kind: 'any', version: null } },
        });
    });

    it('removes many rows unconditionally in one call', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.gmm_components.delete_many': { result: ok },
        });
        const response = await send(server, sessionId, 'DELETE', '/api/synthetic/gmm-components', {
            intent: INTENT,
            removals: [{ key: ID }, { key: OTHER }],
        });
        expect(response.statusCode).toBe(204);
        expect(calls).toHaveLength(1);
        expect(calls[0]?.body).toEqual({
            removals: [
                { key: { id: ID }, precondition: { kind: 'any', version: null } },
                { key: { id: OTHER }, precondition: { kind: 'any', version: null } },
            ],
            intent: INTENT,
        });
    });

    it('refuses a save that names one row twice before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/gmm-components', {
            intent: INTENT,
            changes: [
                { write: COMPONENT, version: 3 },
                { write: { ...COMPONENT, weight: 0.2 }, version: 3 },
            ],
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toContain('names each row once');
        expect(calls).toHaveLength(0);
    });

    it('refuses a removal that names one row twice before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'DELETE', '/api/synthetic/gmm-components', {
            intent: INTENT,
            removals: [{ key: ID }, { key: ID }],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses a batch removal that names a version, since the server cannot check it', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'DELETE', '/api/synthetic/gmm-components', {
            intent: INTENT,
            removals: [{ key: ID, version: 1 }, { key: OTHER }],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});

describe('synthetic operations', () => {
    it('keeps the ticks of a feed the service publishes and serves the newest ones', async () => {
        const replies: Record<string, unknown> = {
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.eurusd'],
            },
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [{ id: ID, source_name: 'live.eurusd' }],
            },
        };
        const { server, sessionId, publish } = buildTestServer(replies);
        await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);

        const subject = 'synthetic.v1.ops.tick.live.eurusd';
        publish(subject, { value: '1.1', observation_time: '2026-10-11 09:00:00Z' });
        publish(subject, { value: 'not a price', observation_time: '2026-10-11 09:00:30Z' });
        publish(subject, { value: '1.2', observation_time: '2026-10-11 09:01:00Z' });
        publish(subject, { value: '1.3', observation_time: '2026-10-11 09:02:00Z' });

        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/feeds/${ID}/ticks?limit=2`,
        );
        expect(response.json()).toEqual({
            sourceName: 'live.eurusd',
            ticks: [
                { at: '2026-10-11 09:01:00Z', value: 1.2, uri: '' },
                { at: '2026-10-11 09:02:00Z', value: 1.3, uri: '' },
            ],
        });
    });

    it('forgets a run when its feed stops, so a later run under the name starts empty', async () => {
        const replies: Record<string, unknown> = {
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.eurusd'],
            },
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [{ id: ID, source_name: 'live.eurusd' }],
            },
        };
        const { server, sessionId, publish } = buildTestServer(replies);
        const subject = 'synthetic.v1.ops.tick.live.eurusd';
        await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        publish(subject, { value: '1.1', observation_time: '2026-10-11 09:00:00Z' });

        replies['synthetic.v1.feed_configs.list'] = { success: true, running_source_names: [] };
        const stopped = await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        expect(stopped.json().ticks).toEqual([]);
        publish(subject, { value: '9.9', observation_time: '2026-10-11 09:03:00Z' });

        replies['synthetic.v1.feed_configs.list'] = {
            success: true,
            running_source_names: ['live.eurusd'],
        };
        await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        publish(subject, { value: '1.5', observation_time: '2026-10-11 09:04:00Z' });
        const again = await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        expect(again.json().ticks).toEqual([{ at: '2026-10-11 09:04:00Z', value: 1.5, uri: '' }]);
    });

    it('starts no listening when the service publishes no feed under the name', async () => {
        const { server, sessionId, publish } = buildTestServer({
            'synthetic.v1.feed_configs.list': { success: true, running_source_names: [] },
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [{ id: ID, source_name: 'live.eurusd' }],
            },
        });
        await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        publish('synthetic.v1.ops.tick.live.eurusd', {
            value: '1.1',
            observation_time: '2026-10-11 09:00:00Z',
        });
        const response = await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        expect(response.json().ticks).toEqual([]);
    });

    it('answers 404 for the ticks of a feed the session cannot read', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: ok,
                fx_spot_generation_configs: [],
            },
        });
        const response = await send(server, sessionId, 'GET', `/api/synthetic/feeds/${ID}/ticks`);
        expect(response.statusCode).toBe(404);
    });

    it('serves the ticks of an IR curve feed with the pillar each one is a value of', async () => {
        const replies: Record<string, unknown> = {
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.usdsofr'],
            },
            'synthetic.v1.ir_curve_generation_configs.list': {
                result: ok,
                ir_curve_generation_configs: [{ id: ID, source_name: 'live.usdsofr' }],
            },
        };
        const { server, sessionId, publish, calls } = buildTestServer(replies);
        await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/feeds/${ID}/ticks?resource=ir-curve-feeds`,
        );
        const pillar = (end: string): string =>
            `oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=${end}`;

        const subject = 'synthetic.v1.ops.tick.live.usdsofr';
        publish(subject, {
            value: '0.04',
            observation_time: '2026-10-11 09:00:00Z',
            oresmd_uri: pillar('30D'),
        });
        publish(subject, {
            value: '0.045',
            observation_time: '2026-10-11 09:00:00Z',
            oresmd_uri: pillar('1826D'),
        });

        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/feeds/${ID}/ticks?resource=ir-curve-feeds`,
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().ticks).toEqual([
            { at: '2026-10-11 09:00:00Z', value: 0.04, uri: pillar('30D') },
            { at: '2026-10-11 09:00:00Z', value: 0.045, uri: pillar('1826D') },
        ]);
        // The kind is named, so the feed is read through it alone: no FX read is made.
        expect(calls.map((call) => call.subject)).not.toContain(
            'synthetic.v1.fx_spot_generation_configs.list',
        );
    });

    it('reads a curve feed for a person who may read curve feeds and not FX feeds', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.feed_configs.list': { success: true, running_source_names: [] },
            'synthetic.v1.fx_spot_generation_configs.list': {
                result: { outcome: 'permission_denied', message: 'No FX feeds for you.' },
            },
            'synthetic.v1.ir_curve_generation_configs.list': {
                result: ok,
                ir_curve_generation_configs: [{ id: ID, source_name: 'live.usdsofr' }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/feeds/${ID}/ticks?resource=ir-curve-feeds`,
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().sourceName).toBe('live.usdsofr');
    });

    it('refuses a ticks read of a resource that is not a feed', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/synthetic/feeds/${ID}/ticks?resource=folders`,
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('answers 409 with the server words when a feed start is refused', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.start_feed': {
                success: false,
                message: 'The binding collides with Realistic 2026.',
            },
        });
        const response = await send(server, sessionId, 'POST', `/api/synthetic/feeds/${ID}/start`);
        expect(response.statusCode).toBe(409);
        expect(response.json().message).toBe('The binding collides with Realistic 2026.');
        expect(calls[0]?.body).toEqual({ config_id: ID });
    });

    it('stops a feed by its config id', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.stop_feed': { success: true, message: 'Stopped.' },
        });
        const response = await send(server, sessionId, 'POST', `/api/synthetic/feeds/${ID}/stop`);
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toEqual({ config_id: ID, source_name: '' });
    });

    it('starts a folder and reports the counts by kind', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.start_feeds_under_folder': {
                success: true,
                message: '',
                started: 3,
                already_running: 1,
                skipped: 2,
                by_kind: { fx: { started: 3, already_running: 1, skipped: 0 } },
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            `/api/synthetic/folders/${COLLECTION}/start`,
        );
        expect(response.json()).toMatchObject({
            started: 3,
            alreadyRunning: 1,
            skipped: 2,
            byKind: { fx: { started: 3, already_running: 1, skipped: 0 } },
        });
    });

    it('stops a folder and reports the count', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.stop_feeds_under_folder': {
                success: true,
                message: '',
                stopped: 4,
                stopped_by_kind: { fx: 4 },
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            `/api/synthetic/folders/${COLLECTION}/stop`,
        );
        expect(response.json()).toMatchObject({ stopped: 4, byKind: { fx: 4 } });
    });

    it('reads the vintage validity', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.get_vintage_validity': {
                success: true,
                message: '',
                entries: [{ vintage_source: 'ecb' }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/vintages');
        expect(response.json().entries).toHaveLength(1);
    });

    it('simulates FX paths from the mixture on the screen', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.simulate_fx_spot_paths': {
                success: true,
                message: '',
                paths: [[1.1, 1.11]],
            },
        });
        const request = {
            gmm_means: [0],
            gmm_stdevs: [0.01],
            gmm_weights: [1],
            process_type: 'geometric',
            initial_price: 1.1,
            num_ticks: 2,
            num_paths: 1,
            seed: 42,
        };
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/synthetic/simulate/fx',
            request,
        );
        expect(response.json()).toEqual({ paths: [[1.1, 1.11]] });
        expect(calls[0]?.body).toEqual(request);
    });

    it('refuses a mixture whose arrays differ in length before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/synthetic/simulate/fx', {
            gmm_means: [0, 0],
            gmm_stdevs: [0.01],
            gmm_weights: [1],
            process_type: 'geometric',
            initial_price: 1.1,
            num_ticks: 2,
            num_paths: 1,
            seed: 42,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('converts a mixture to another engine through the synthetic service', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.convert_fx_mixture': {
                success: true,
                message: '',
                gmm_means: [0],
                gmm_stdevs: [0.001],
                gmm_weights: [0.046],
            },
        });
        const body = {
            gmm_means: [0, 0],
            gmm_stdevs: [0.0005, 0.002],
            gmm_weights: [0.95, 0.05],
            from_process_type: 'geometric',
            to_process_type: 'ornstein_uhlenbeck',
            initial_price: 2,
            ticks_per_hour: 60,
        };
        const response = await send(server, sessionId, 'POST', '/api/synthetic/convert/fx', body);
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            gmm_means: [0],
            gmm_stdevs: [0.001],
            gmm_weights: [0.046],
        });
        expect(calls[0]?.body).toEqual(body);
    });

    it('refuses a conversion to an engine it does not know before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/synthetic/convert/fx', {
            gmm_means: [0],
            gmm_stdevs: [0.001],
            gmm_weights: [1],
            from_process_type: 'geometric',
            to_process_type: 'jump',
            initial_price: 2,
            ticks_per_hour: 60,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('reads the children of a folder with the feeds under each', async () => {
        const folder = {
            id: 'f1',
            parent_id: '',
            name: 'Synthetic',
            kind: 'root',
            provider_code: '',
            fx_feeds: 26,
            ir_feeds: 33,
            has_children: true,
        };
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.list_feed_folders': { success: true, message: '', folders: [folder] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-folders?parent=f0',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ folders: [folder] });
        expect(calls[0]?.body).toEqual({ parent_id: 'f0' });
    });

    it('asks for the roots when no folder is named', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.list_feed_folders': { success: true, message: '', folders: [] },
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/feed-folders');
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ folders: [] });
        expect(calls[0]?.body).toEqual({ parent_id: '' });
    });

    it('refuses a folder request with a parameter it does not know', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-folders?depth=2',
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('reads a page of the feeds in a folder', async () => {
        const feed = {
            id: 'x1',
            kind: 'fx_spot',
            label: 'EUR/USD',
            source_name: 'synthetic.bloomberg.eurusd',
            oresmd_uri: 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD',
            folder_id: 'f1',
            enabled: true,
            provider_code: 'bloomberg',
            asset_class_code: 'fx',
            underlying: 'EUR/USD',
            engine: 'geometric',
            ticks_per_hour: 1800,
            auto_start: true,
        };
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                message: '',
                total: 26,
                feeds: [feed],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/folder-feeds?folder=f1&provider=bloomberg&assetClass=fx&state=started&search=usd&offset=60&limit=30',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ total: 26, feeds: [feed] });
        expect(calls[0]?.body).toEqual({
            folder_id: 'f1',
            provider_code: 'bloomberg',
            asset_class: 'fx',
            state: 'started',
            should_run: false,
            search: 'usd',
            offset: 60,
            limit: 30,
        });
    });

    it('asks for the feeds that should run and do not', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                message: '',
                total: 0,
                feeds: [],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/folder-feeds?shouldRun=true',
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ should_run: true });
    });

    it('asks for the first page of every folder when nothing is named', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                message: '',
                total: 0,
                feeds: [],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/folder-feeds');
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toEqual({
            folder_id: '',
            provider_code: '',
            asset_class: '',
            state: '',
            should_run: false,
            search: '',
            offset: 0,
            limit: 60,
        });
    });

    it('refuses a feed page that asks for too many or for an unknown parameter', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        for (const query of ['limit=500', 'limit=0', 'offset=-1', 'depth=2', 'state=running']) {
            const response = await send(
                server,
                sessionId,
                'GET',
                `/api/synthetic/folder-feeds?${query}`,
            );
            expect(response.statusCode, query).toBe(400);
        }
        expect(calls).toEqual([]);
    });

    it('starts the picked feeds through the service', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.start_selected_feeds': {
                success: true,
                message: '2 started, 0 already running, 1 skipped',
                started: 2,
                already_running: 0,
                skipped: 1,
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/synthetic/feed-selection/start',
            { feedIds: ['a', 'b', 'c'] },
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            message: '2 started, 0 already running, 1 skipped',
            started: 2,
            alreadyRunning: 0,
            skipped: 1,
        });
        expect(calls[0]?.body).toEqual({
            folder_id: '',
            provider_code: '',
            asset_class: '',
            state: '',
            should_run: false,
            search: '',
            feed_ids: ['a', 'b', 'c'],
            excluded_ids: [],
        });
    });

    it('starts the feeds that should run, named by a flag in the body', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.start_selected_feeds': { success: true, message: '', started: 18 },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/synthetic/feed-selection/start',
            { shouldRun: true },
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ should_run: true, feed_ids: [] });
    });

    it('stops the feeds a filter matches, less those left out', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.stop_selected_feeds': {
                success: true,
                message: '9 stopped',
                stopped: 9,
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/synthetic/feed-selection/stop',
            { provider: 'bloomberg', state: 'started', excludedIds: ['x'] },
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ message: '9 stopped', stopped: 9 });
        expect(calls[0]?.body).toEqual({
            folder_id: '',
            provider_code: 'bloomberg',
            asset_class: '',
            state: 'started',
            should_run: false,
            search: '',
            feed_ids: [],
            excluded_ids: ['x'],
        });
    });

    it('refuses a selection with a state it does not know or too many ids', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        for (const body of [
            { state: 'running' },
            { feedIds: Array.from({ length: 2001 }, (_, i) => `id${i}`) },
            { unknown: true },
        ]) {
            const response = await send(
                server,
                sessionId,
                'POST',
                '/api/synthetic/feed-selection/start',
                body,
            );
            expect(response.statusCode).toBe(400);
        }
        expect(calls).toEqual([]);
    });

    it('reads an FX mixture as the simple view through the synthetic service', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.convert_fx_simple': {
                success: true,
                message: '',
                has_simple_form: true,
                trend_pct_per_hour: 0.05,
                volatility_pct_per_hour: 0.1,
                jump_share_pct: 10,
                half_life_minutes: 0,
            },
        });
        const body = {
            process_type: 'geometric',
            to_simple: true,
            initial_price: 1.1,
            ticks_per_hour: 3600,
            gmm_means: [0],
            gmm_stdevs: [0.001],
            gmm_weights: [1],
        };
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/synthetic/convert/fx-simple',
            body,
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            has_simple_form: true,
            volatility_pct_per_hour: 0.1,
            jump_share_pct: 10,
        });
        expect(calls[0]?.body).toMatchObject(body);
    });

    it('refuses an FX simple conversion for an engine it does not know', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/synthetic/convert/fx-simple', {
            process_type: 'jump',
            to_simple: true,
            initial_price: 1,
            ticks_per_hour: 60,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('converts IR parameters to the simple view through the synthetic service', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.ops.convert_ir_parameters': {
                success: true,
                message: '',
                has_simple_form: true,
                parameter_names: ['start_rate', 'half_life_years'],
                parameter_values: [0.03, 1.4],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/synthetic/convert/ir', {
            process_type: 'VASICEK',
            to_simple: true,
            values: { kappa: 0.5, theta: 0.03 },
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            has_simple_form: true,
            message: '',
            caution: '',
            values: { start_rate: 0.03, half_life_years: 1.4 },
        });
        expect(calls[0]?.body).toEqual({
            process_type: 'VASICEK',
            to_simple: true,
            parameter_names: ['kappa', 'theta'],
            parameter_values: [0.5, 0.03],
        });
    });

    it('answers 400 with the server words when an IR conversion is refused', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.ops.convert_ir_parameters': {
                success: false,
                message: 'The half-life must be longer than zero.',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/synthetic/convert/ir', {
            process_type: 'VASICEK',
            to_simple: false,
            values: { half_life_years: 0 },
        });
        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({
            message: 'The half-life must be longer than zero.',
        });
    });

    it('answers 400 with the server words when a preview is refused', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.ops.simulate_ir_curve_paths': {
                success: false,
                message: 'Unknown process type: black_karasinski.',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/synthetic/simulate/ir', {
            process_type: 'black_karasinski',
            parameters: [],
            num_ticks: 10,
            num_paths: 1,
            seed: 1,
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe('Unknown process type: black_karasinski.');
    });

    it('previews the curve shape at each ladder row', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.ops.preview_ir_curve_shape': {
                success: true,
                message: '',
                points: [
                    { sequence_index: 0, start_tenor_code: '0D', end_tenor_code: '1Y', rate: 0.03 },
                ],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/synthetic/preview/ir-shape', {
            process_type: 'vasicek',
            parameters: [{ parameter_name: 'kappa', parameter_value: 0.1 }],
            fixed_leg_payment_frequency_code: 'Annual',
            entries: [
                {
                    sequence_index: 0,
                    start_tenor_code: '0D',
                    end_tenor_code: '1Y',
                    instrument_code: 'OIS',
                },
            ],
        });
        expect(response.json().points[0].rate).toBe(0.03);
    });

    it('serves the history of a synthetic row from the synthetic service', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'synthetic.v1.history.get': { success: true, versions: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/history?entityType=ores.synthetic.gmm_component&entityId=${ID}`,
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.subject).toBe('synthetic.v1.history.get');
    });
});
