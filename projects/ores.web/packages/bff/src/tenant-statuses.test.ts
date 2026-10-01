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

/**
 * How a code domain's values are painted.
 *
 * What is asserted here is the join: the mapping says which badge a value gets,
 * the catalogue says what that badge looks like, and a screen should not have to
 * hold both to draw one pill. What the route does not do is as important as what
 * it does — a value nobody has mapped is absent from the answer rather than
 * painted like everything else, and a mapping whose badge has gone is dropped
 * rather than answered with empty colours.
 */

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

const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

const party = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'System Party',
    partyCategory: 'System',
    businessCenterCode: 'WRLD',
};

function status(code: string, name: string, badgeCode: string | null): unknown {
    return {
        status: code,
        name,
        description: `${name} explanation.`,
        display_order: 0,
        badge_code: badgeCode,
    };
}

function definition(code: string, name: string, background: string): unknown {
    return {
        code,
        name,
        description: `${name} explanation.`,
        background_colour: background,
        text_colour: '#ffffff',
        severity_code: 'primary',
        css_class: 'badge',
        display_order: 1,
    };
}

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
        username: 'super_admin',
        email: 'super_admin@system.ores',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: SYSTEM_TENANT,
        tenantName: 'Root Tenant',
        mode: 'system-administration',
        version: 'v0.0.25 (test)',
        availableParties: [party],
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

describe('GET /api/tenant-statuses', () => {
    it('joins a status row to the badge the row names', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.tenant_statuses.list': {
                statuses: [
                    status('suspended', 'Suspended', 'frozen'),
                    status('active', 'Active', 'active'),
                ],
            },
            'dq.v1.badge_definitions.list': {
                definitions: [
                    definition('active', 'Active', '#22c55e'),
                    definition('frozen', 'Frozen', '#eab308'),
                ],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenant-statuses',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            statuses: [
                {
                    code: 'suspended',
                    name: 'Suspended',
                    description: 'Suspended explanation.',
                    badge: {
                        code: 'frozen',
                        label: 'Frozen',
                        description: 'Frozen explanation.',
                        backgroundColour: '#eab308',
                        textColour: '#ffffff',
                        severity: 'primary',
                    },
                },
                {
                    code: 'active',
                    name: 'Active',
                    description: 'Active explanation.',
                    badge: {
                        code: 'active',
                        label: 'Active',
                        description: 'Active explanation.',
                        backgroundColour: '#22c55e',
                        textColour: '#ffffff',
                        severity: 'primary',
                    },
                },
            ],
        });
        expect(calls[0]?.subject).toBe('iam.v1.tenant_statuses.list');

        await server.close();
    });

    /*
     * The words survive a badge that has gone. The status is still readable, it
     * has simply lost its colours, which is worse to look at and better than
     * being hidden.
     */
    it('keeps the words when a status names a badge the catalogue has lost', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.tenant_statuses.list': {
                statuses: [status('suspended', 'Suspended', 'gone')],
            },
            'dq.v1.badge_definitions.list': { definitions: [] },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenant-statuses',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().statuses).toEqual([
            {
                code: 'suspended',
                name: 'Suspended',
                description: 'Suspended explanation.',
                badge: null,
            },
        ]);

        await server.close();
    });

    it('answers a status that names no badge as unpainted', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.tenant_statuses.list': {
                statuses: [status('retired', 'Retired', null)],
            },
            'dq.v1.badge_definitions.list': { definitions: [] },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenant-statuses',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().statuses[0].badge).toBeNull();

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server } = buildTestServer({});

        const response = await server.inject({ method: 'GET', url: '/api/tenant-statuses' });

        expect(response.statusCode).toBe(401);

        await server.close();
    });
});
