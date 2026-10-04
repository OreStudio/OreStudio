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
import { buildServer } from './server.js';
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

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: 'eager_maxwell',
    });
}

const ok = { outcome: 'ok', code: '', message: '' };

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
        cookies: { ores_web_session: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const reason = { reasonCode: 'common.rectification', commentary: 'Tidy up' };

describe('GET /api/classifications', () => {
    it('answers the 28 lists without their subjects', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/classifications');
        expect(response.statusCode).toBe(200);
        const lists = (response.json() as { lists: Record<string, unknown>[] }).lists;
        expect(lists).toHaveLength(28);
        expect(lists[0]).toEqual({
            key: 'monetary-nature',
            entityType: 'ores.refdata.monetary_nature',
            topic: 'currencies',
            shape: 'named',
            editable: true,
            writePermission: 'refdata::monetary_natures:write',
            deletePermission: 'refdata::monetary_natures:delete',
            count: null,
        });
    });

    it('refuses a caller with no session', async () => {
        const { server } = buildTestServer({});
        const response = await server.inject({ method: 'GET', url: '/api/classifications' });
        expect(response.statusCode).toBe(401);
    });
});

describe('the catalogue counts', () => {
    it('answers the count each list reports', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.monetary_natures.list': { result: ok, types: [], total: 4 },
        });
        const response = await send(server, sessionId, 'GET', '/api/classifications');
        const lists = (response.json() as { lists: { key: string; count: number | null }[] }).lists;
        expect(lists.find((list) => list.key === 'monetary-nature')?.count).toBe(4);
        expect(lists.find((list) => list.key === 'rounding-type')?.count).toBeNull();
    });
});

describe('the labels', () => {
    it('joins each row to its label', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.rounding_types.list': {
                result: ok,
                types: [
                    { version: 1, code: 'Up', name: 'Up', display_order: 1 },
                    { version: 1, code: 'Odd', name: 'Odd', display_order: 2 },
                ],
            },
            'dq.v1.badge_mappings.list_by_code_domain_code': {
                result: ok,
                badge_mappings: [
                    {
                        code_domain_code: 'rounding_type',
                        entity_code: 'Up',
                        badge_code: 'rounding_type_up',
                    },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/classifications/rounding-type');
        expect(response.json()).toMatchObject({
            rows: [
                { code: 'Up', labelCode: 'rounding_type_up' },
                { code: 'Odd', labelCode: null },
            ],
        });
    });

    it('still answers the rows when the labels cannot be read', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.rounding_types.list': {
                result: ok,
                types: [{ version: 1, code: 'Up', name: 'Up', display_order: 1 }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/classifications/rounding-type');
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ rows: [{ code: 'Up', labelCode: null }] });
    });

    it('answers the label catalogue', async () => {
        const { server, sessionId } = buildTestServer({
            'dq.v1.badge_definitions.list': {
                definitions: [
                    { code: 'rounding_type_up', name: 'Up', background_colour: '#8b5cf6' },
                ],
            },
            'dq.v1.badge_mappings.list': {
                result: ok,
                badge_mappings: [
                    {
                        code_domain_code: 'rounding_type',
                        entity_code: 'Up',
                        badge_code: 'rounding_type_up',
                    },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/labels');
        expect(response.json()).toMatchObject({
            labels: [{ code: 'rounding_type_up', label: 'Up' }],
            domains: { rounding_type: ['rounding_type_up'] },
        });
    });

    it('labels a row of a read-only list, because a label is not a spelling', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'dq.v1.badge_mappings.put': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/day-counter/rows/A360/label',
            {
                badgeCode: 'active',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: {
                    code_domain_code: 'day_counter',
                    entity_code: 'A360',
                    badge_code: 'active',
                },
            },
        });
    });

    it('takes a label away when the badge is null', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'dq.v1.badge_mappings.delete': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/rounding-type/rows/Up/label',
            {
                badgeCode: null,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.subject).toBe('dq.v1.badge_mappings.delete');
    });
});

describe('GET /api/classifications/:list', () => {
    it('answers the rows of a list', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.rounding_types.list': {
                result: ok,
                types: [
                    { version: 1, code: 'Up', name: 'Up', description: 'Away', display_order: 10 },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/classifications/rounding-type');
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            rows: [{ code: 'Up', name: 'Up', displayOrder: 10, version: 1 }],
        });
    });

    it('answers 404 for a list the catalogue does not name', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/classifications/currencies');
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(0);
    });
});

describe('the classification writes', () => {
    it('adds a row as a create', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.rounding_types.put': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/classifications/rounding-type/rows',
            {
                code: 'Up',
                name: 'Up',
                description: 'Away from zero',
                displayOrder: 20,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_not_exist', version: null } },
            intent: { reason_code: 'common.rectification', commentary: 'Tidy up' },
        });
    });

    it('refuses a write with no change reason', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/classifications/rounding-type/rows',
            {
                code: 'Up',
            },
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses to change a list of ORE spellings', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/day-counter/rows/A360',
            {
                description: 'Actual/360',
                version: 1,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });

    it('answers 409 when the row moved on since it was read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.rounding_types.put': {
                result: { outcome: 'conflict', code: 'version_conflict', message: 'Stale.' },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/rounding-type/rows/Up',
            {
                name: 'Up',
                description: 'Away',
                displayOrder: 10,
                version: 1,
                ...reason,
            },
        );
        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({ message: 'Stale.' });
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { code: 'Up' },
                precondition: { kind: 'must_match_version', version: 1 },
            },
        });
    });

    it('says what a conflict means when the server sends no words', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.rounding_types.put': {
                result: { outcome: 'conflict', code: '', message: '' },
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/classifications/rounding-type/rows',
            {
                code: 'Up',
                ...reason,
            },
        );
        expect(response.statusCode).toBe(409);
        expect((response.json() as { message: string }).message).toContain('Reload and try again');
    });

    it('writes a new order in one call', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.rounding_types.put_many': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/rounding-type/order',
            {
                rows: [
                    { code: 'Up', name: 'Up', description: '', displayOrder: 20, version: 1 },
                    { code: 'Down', name: 'Down', description: '', displayOrder: 10, version: 2 },
                ],
                ...reason,
            },
        );
        expect(response.statusCode).toBe(204);
        expect((calls[0]?.body as { changes: unknown[] }).changes).toHaveLength(2);
    });

    it('refuses to reorder a list of ORE spellings', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/classifications/calendar-name/order',
            {
                rows: [{ code: 'TARGET', name: '', description: '', displayOrder: 1, version: 1 }],
                ...reason,
            },
        );
        expect(response.statusCode).toBe(403);
    });

    it('removes a row with its reason', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.rounding_types.delete': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/classifications/rounding-type/rows/Up',
            reason,
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({ removal: { key: { code: 'Up' } } });
    });
});

describe('GET /api/history', () => {
    it('answers the versions of a row of a served list', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.history.get': {
                success: true,
                versions: [{ version: 1, modified_by: 'system', recorded_at: 't1' }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/history?entityType=ores.refdata.rounding_type&entityId=Up',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ versions: [{ version: 1, modifiedBy: 'system' }] });
        expect(calls[0]?.subject).toBe('refdata.v1.history.get');
    });

    it('answers 404 for an entity type no served list has', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/history?entityType=ores.iam.account&entityId=x',
        );
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(0);
    });
});
