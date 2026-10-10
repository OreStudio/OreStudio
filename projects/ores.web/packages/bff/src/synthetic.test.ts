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
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
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
const PARTY = '77777777-7777-4777-8777-777777777777';
const COLLECTION = '88888888-8888-4888-8888-888888888888';
const INTENT = { reason_code: 'common.correction', commentary: '' };
const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

const COMPONENT = {
    id: ID,
    party_id: PARTY,
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
            'fx-feeds',
            'gmm-components',
            'ir-curve-feeds',
            'ir-parameter-values',
            'ir-template-entries',
            'process-types',
            'parameter-definitions',
        ]);
        const definitions = resources.find((resource) => resource.key === 'parameter-definitions');
        expect(definitions?.writable).toBe(false);
        expect(definitions?.readOnlyBecause).toContain('name alone');
        const mixture = resources.find((resource) => resource.key === 'gmm-components');
        expect(mixture?.writePermission).toBe('synthetic::gmm_components:write');
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
    it('reads the running source names', async () => {
        const { server, sessionId } = buildTestServer({
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['fx.EURUSD'],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/running');
        expect(response.json()).toEqual({ sourceNames: ['fx.EURUSD'] });
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
