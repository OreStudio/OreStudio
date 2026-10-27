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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The sensitivity template routes.
 *
 * What is checked is what the routes promise a screen: the page comes from the
 * server with its total and its search, one key is read by its code, a write
 * carries the change reason and derives the method's token, a create refused
 * for a code the tenant holds says so, and a removal states what it removed.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

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

const LIST = 'analytics.v1.sensitivity_templates.list';
const GET = 'analytics.v1.sensitivity_templates.get';
const PUT = 'analytics.v1.sensitivity_templates.put';
const DELETE = 'analytics.v1.sensitivity_templates.delete';
const HISTORY = 'analytics.v1.history.get';

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
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
        tenantId: '44444444-4444-4444-4444-444444444444',
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

/** One stored row, in the server's own field names. */
function stored(
    code: string,
    assetClass: string,
    method: string,
    methodCode: string,
    description = `${code} explained`,
): Record<string, unknown> {
    return {
        version: 2,
        tenant_id: '44444444-4444-4444-4444-444444444444',
        code,
        asset_class: assetClass,
        method,
        method_code: methodCode,
        description,
        modified_by: 'priya',
        performed_by: 'ores_analytics_service',
        change_reason_code: 'common.rectification',
        change_commentary: 'Tidy up',
        recorded_at: '2026-10-04 09:05:00.000000Z',
    };
}

const reason = { reasonCode: 'common.rectification', commentary: 'Tidy up' };

describe('GET /api/analytics/sensitivity-templates', () => {
    it('answers one page of the tenant keys with the server total', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: {
                result: ok,
                total: 19,
                templates: [stored('IR_FD', 'IR', 'Finite difference', 'FD')],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates?offset=15&limit=15',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            rows: [
                {
                    code: 'IR_FD',
                    assetClass: 'IR',
                    method: 'Finite difference',
                    methodCode: 'FD',
                    description: 'IR_FD explained',
                    version: 2,
                    modifiedBy: 'priya',
                    performedBy: 'ores_analytics_service',
                    reasonCode: 'common.rectification',
                    commentary: 'Tidy up',
                    recordedAt: '2026-10-04 09:05:00.000000Z',
                },
            ],
            total: 19,
        });
        expect(calls[0]).toEqual({
            subject: LIST,
            body: {
                offset: 15,
                limit: 15,
                order: { field: '', descending: false },
                filter: { code_one_of: null, search: null },
                as_of: null,
            },
        });
    });

    it('sends the search and the order to the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, total: 0, templates: [] },
        });
        await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates?search=MC&sort=method&descending=true',
        );
        expect(calls[0]?.body).toMatchObject({
            filter: { code_one_of: null, search: 'MC' },
            order: { field: 'method', descending: true },
        });
    });

    it('says why the server refused the read', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST]: {
                result: {
                    outcome: 'invalid',
                    code: 'order_not_supported',
                    message: 'No such order.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates',
        );
        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ message: 'No such order.' });
    });

    it('refuses a caller with no session', async () => {
        const { server } = buildTestServer({});
        const response = await server.inject({
            method: 'GET',
            url: '/api/analytics/sensitivity-templates',
        });
        expect(response.statusCode).toBe(401);
    });
});

describe('GET /api/analytics/sensitivity-templates/:code', () => {
    it('answers one key', async () => {
        const { server, sessionId } = buildTestServer({
            [GET]: { result: ok, sensitivity_template: stored('EQ_MC', 'EQ', 'Monte Carlo', 'MC') },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/EQ_MC',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            template: { code: 'EQ_MC', assetClass: 'EQ', methodCode: 'MC' },
        });
    });

    it('answers 404 for a code the tenant does not hold', async () => {
        const { server, sessionId } = buildTestServer({
            [GET]: {
                result: {
                    outcome: 'missing',
                    code: 'not_found',
                    message: 'There is no sensitivity template NOPE.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/NOPE',
        );
        expect(response.statusCode).toBe(404);
        expect(response.json()).toMatchObject({
            message: 'There is no sensitivity template NOPE.',
        });
    });

    it('answers 404 for a row the service answered with no key', async () => {
        const { server, sessionId } = buildTestServer({
            [GET]: { result: ok, sensitivity_template: null },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/NOPE',
        );
        expect(response.statusCode).toBe(404);
    });
});

describe('POST /api/analytics/sensitivity-templates', () => {
    it('writes a new key with the reason and the token its method implies', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [PUT]: {
                result: ok,
                sensitivity_template: stored(
                    'IR_Grid',
                    'IR',
                    'Finite difference',
                    'FD',
                    'Grid keys',
                ),
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/analytics/sensitivity-templates',
            {
                code: 'IR_Grid',
                assetClass: 'IR',
                method: 'Finite difference',
                description: 'Grid keys',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(201);
        expect(response.json()).toMatchObject({
            template: { code: 'IR_Grid', methodCode: 'FD', reasonCode: 'common.rectification' },
        });
        expect(calls[0]).toEqual({
            subject: PUT,
            body: {
                change: {
                    write: {
                        code: 'IR_Grid',
                        asset_class: 'IR',
                        method: 'Finite difference',
                        method_code: 'FD',
                        description: 'Grid keys',
                    },
                    precondition: { kind: 'must_not_exist', version: null },
                },
                intent: { reason_code: 'common.rectification', commentary: 'Tidy up' },
            },
        });
    });

    it('refuses a code the tenant already holds, naming it', async () => {
        const { server, sessionId } = buildTestServer({
            [PUT]: {
                result: {
                    outcome: 'conflict',
                    code: 'already_exists',
                    message: 'A sensitivity template with this code already exists.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/analytics/sensitivity-templates',
            {
                code: 'IR_FD',
                assetClass: 'IR',
                method: 'Finite difference',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({
            code: 'conflict',
            message: 'A sensitivity template with this code already exists.',
        });
    });

    it('refuses a method the table does not allow, before the write', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/analytics/sensitivity-templates',
            {
                code: 'IR_Bespoke',
                assetClass: 'IR',
                method: 'Bespoke',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({
            message: expect.stringContaining('Bespoke is not a method'),
        });
        expect(calls).toHaveLength(0);
    });

    it('refuses an asset class the table does not allow', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/analytics/sensitivity-templates',
            {
                code: 'XX_FD',
                assetClass: 'XX',
                method: 'Finite difference',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(400);
    });

    it('refuses a create that carries no reason', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/analytics/sensitivity-templates',
            {
                code: 'IR_Grid',
                assetClass: 'IR',
                method: 'Finite difference',
            },
        );
        expect(response.statusCode).toBe(400);
    });
});

describe('PUT /api/analytics/sensitivity-templates/:code', () => {
    it('corrects a key against the version read, with the reason', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [PUT]: {
                result: ok,
                sensitivity_template: stored('IR_FD', 'IR', 'Monte Carlo', 'MC', 'Corrected'),
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/analytics/sensitivity-templates/IR_FD',
            {
                assetClass: 'IR',
                method: 'Monte Carlo',
                description: 'Corrected',
                version: 2,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ template: { methodCode: 'MC' } });
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { code: 'IR_FD', method_code: 'MC' },
                precondition: { kind: 'must_match_version', version: 2 },
            },
        });
    });

    it('refuses a code the tenant does not hold', async () => {
        const { server, sessionId } = buildTestServer({
            [PUT]: {
                result: {
                    outcome: 'missing',
                    code: 'not_found',
                    message: 'There is no sensitivity template NOPE.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/analytics/sensitivity-templates/NOPE',
            {
                assetClass: 'IR',
                method: 'Monte Carlo',
                description: '',
                version: 1,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(404);
    });

    it('refuses a row that changed since it was read', async () => {
        const { server, sessionId } = buildTestServer({
            [PUT]: {
                result: {
                    outcome: 'conflict',
                    code: 'version_conflict',
                    message: 'Sensitivity template IR_FD was version 3, not 2.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/analytics/sensitivity-templates/IR_FD',
            {
                assetClass: 'IR',
                method: 'Monte Carlo',
                description: '',
                version: 2,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({ code: 'conflict' });
    });

    it('refuses a correction that states no version', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/analytics/sensitivity-templates/IR_FD',
            { assetClass: 'IR', method: 'Monte Carlo', ...reason },
        );
        expect(response.statusCode).toBe(400);
    });
});

describe('DELETE /api/analytics/sensitivity-templates/:code', () => {
    it('states what it removed', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [GET]: {
                result: ok,
                sensitivity_template: stored('IR_FD', 'IR', 'Finite difference', 'FD'),
            },
            [DELETE]: { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/analytics/sensitivity-templates/IR_FD',
            { version: 2, ...reason },
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            removed: { code: 'IR_FD', method: 'Finite difference', methodCode: 'FD', version: 2 },
        });
        expect(calls[1]).toEqual({
            subject: DELETE,
            body: {
                removal: {
                    key: { code: 'IR_FD' },
                    precondition: { kind: 'must_match_version', version: 2 },
                },
                intent: { reason_code: 'common.rectification', commentary: 'Tidy up' },
            },
        });
    });

    it('refuses a removal of a key the tenant does not hold', async () => {
        const { server, sessionId } = buildTestServer({
            [GET]: { result: ok, sensitivity_template: null },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/analytics/sensitivity-templates/NOPE',
            { version: 1, ...reason },
        );
        expect(response.statusCode).toBe(404);
    });

    it('refuses a removal the server rejected as stale', async () => {
        const { server, sessionId } = buildTestServer({
            [GET]: {
                result: ok,
                sensitivity_template: stored('IR_FD', 'IR', 'Finite difference', 'FD'),
            },
            [DELETE]: {
                result: {
                    outcome: 'conflict',
                    code: 'version_conflict',
                    message: 'Sensitivity template IR_FD moved on.',
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/analytics/sensitivity-templates/IR_FD',
            { version: 2, ...reason },
        );
        expect(response.statusCode).toBe(409);
    });
});

describe('GET /api/analytics/sensitivity-templates/:code/history', () => {
    it('answers every version of one key, newest first', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [HISTORY]: {
                success: true,
                versions: [
                    {
                        version: 2,
                        modified_by: 'priya',
                        recorded_at: '2026-10-04 09:05:00.000000Z',
                        fields: [{ name: 'Description', value: 'Grid keys' }],
                        changes: {
                            entries: [
                                {
                                    field_name: 'Description',
                                    old_value: '',
                                    new_value: 'Grid keys',
                                },
                            ],
                        },
                    },
                    {
                        version: 1,
                        modified_by: 'priya',
                        recorded_at: '2026-10-01 09:05:00.000000Z',
                        fields: [{ name: 'Description', value: '' }],
                        changes: { entries: [] },
                    },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/IR_FD/history',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            versions: [
                {
                    version: 2,
                    modifiedBy: 'priya',
                    changes: [{ field: 'Description', before: '', after: 'Grid keys' }],
                },
                { version: 1, changes: [] },
            ],
        });
        expect(calls[0]).toEqual({
            subject: HISTORY,
            body: { entity_type: 'ores.analytics.sensitivity_template', entity_id: 'IR_FD' },
        });
    });

    it('answers the refusal when the history cannot be read', async () => {
        const { server, sessionId } = buildTestServer({
            [HISTORY]: { success: false, message: 'No history is served.' },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/IR_FD/history',
        );
        // The shared history read refuses as a failed operation, which is what
        // the generic history route answers too, so a screen receives the same.
        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({ message: 'No history is served.' });
    });
});

describe('GET /api/analytics/sensitivity-templates/choices', () => {
    it('answers the six asset classes and the five methods with their tokens', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/analytics/sensitivity-templates/choices',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            assetClasses: ['IR', 'FX', 'EQ', 'COMM', 'CR', 'ALL'],
            methods: [
                { label: 'Analytical', code: 'Analytical' },
                { label: 'Semianalytical', code: 'Semianalytical' },
                { label: 'Finite difference', code: 'FD' },
                { label: 'Monte Carlo', code: 'MC' },
                { label: 'Trade level', code: 'Trade' },
            ],
        });
    });
});
