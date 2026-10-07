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
 * The access routes: a person's own roles, giving and taking away a role, the
 * role catalogue and an account's picture.
 *
 * The server checks every permission, so these cases pin what the BFF decides:
 * the shape it answers, the request it sends, and that a refusal reaches the
 * browser in the server's words.
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
const PRIYA = '11111111-1111-1111-1111-111111111111';
const DANIEL = '22222222-2222-2222-2222-222222222222';
const TRADING = '33333333-3333-3333-3333-333333333333';
const PHOTO = '44444444-4444-4444-4444-444444444444';

function wireRole(id: string, name: string) {
    return { id, version: 2, name, description: `${name} role` };
}

type Answer = unknown | ((body: unknown) => unknown);

function buildTestServer(answers: Record<string, Answer>) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            const answer = answers[subject];
            if (answer === undefined) throw new Error(`unexpected subject ${subject}`);
            return schema.parse(typeof answer === 'function' ? answer(body) : answer);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'priya',
        email: 'priya@acme.example',
        accountId: PRIYA,
        tenantId: SYSTEM_TENANT,
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: '55555555-5555-5555-5555-555555555555',
                name: 'Acme',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '66666666-6666-6666-6666-666666666666',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { ores_web_session: session.id }, calls };
}

describe('access routes', () => {
    it('answers the signed-in person roles with who gave each one and why', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.get_my_roles': {
                result: { outcome: 'ok' },
                roles: [
                    {
                        role: wireRole(TRADING, 'Trading'),
                        permission_codes: ['refdata::currencies:read'],
                        assigned_by: 'priya',
                        assigned_at: '2026-10-04 09:00:00Z',
                        change_reason_code: 'access.new_joiner',
                        change_commentary: 'Desk start',
                    },
                ],
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/access', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            roles: [
                {
                    roleId: TRADING,
                    name: 'Trading',
                    description: 'Trading role',
                    permissionCodes: ['refdata::currencies:read'],
                    givenBy: 'priya',
                    givenAt: '2026-10-04 09:00:00Z',
                    reasonCode: 'access.new_joiner',
                    commentary: 'Desk start',
                },
            ],
        });
    });

    it('gives a role for the reason the person chose', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.assign_role': { success: true, error_message: '' },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${DANIEL}/roles`,
            cookies,
            payload: {
                roleId: TRADING,
                reasonCode: 'access.cover_for_absence',
                note: 'Covers the desk',
            },
        });
        await server.close();

        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toEqual({
            account_id: DANIEL,
            role_id: TRADING,
            change_reason_code: 'access.cover_for_absence',
            change_commentary: 'Covers the desk',
        });
    });

    it('asks for a reason before giving a role, and sends nothing without one', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${DANIEL}/roles`,
            cookies,
            payload: { roleId: TRADING, reasonCode: '' },
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('passes on the server words when it refuses to take a role away', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.revoke_role': {
                success: false,
                error_message: 'You cannot take a role away from yourself.',
            },
        });

        const response = await server.inject({
            method: 'DELETE',
            url: `/api/accounts/${PRIYA}/roles/${TRADING}`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(409);
        expect(response.json().message).toBe('You cannot take a role away from yourself.');
    });

    it('reads what each role grants, but not what a service role grants', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.roles.list': {
                result: { outcome: 'ok' },
                roles: [wireRole(TRADING, 'Trading'), wireRole(PRIYA, 'IamService')],
                total: 2,
            },
            'iam.v1.ops.get_role_permissions': {
                result: { outcome: 'ok' },
                permission_codes: ['refdata::currencies:read'],
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/roles', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().roles).toEqual([
            {
                id: TRADING,
                version: 2,
                name: 'Trading',
                description: 'Trading role',
                service: false,
                registrationDefault: false,
                permissionCodes: ['refdata::currencies:read'],
            },
            {
                id: PRIYA,
                version: 2,
                name: 'IamService',
                description: 'IamService role',
                service: true,
                registrationDefault: false,
                permissionCodes: [],
            },
        ]);
        expect(calls.filter((c) => c.subject === 'iam.v1.ops.get_role_permissions')).toHaveLength(
            1,
        );
    });

    it('keeps a role the registration default when it is renamed', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.roles.put': { result: { outcome: 'ok' }, role: null },
        });

        const response = await server.inject({
            method: 'PUT',
            url: `/api/roles/${TRADING}`,
            cookies,
            payload: {
                name: 'Trading',
                description: 'Desk',
                version: 2,
                registrationDefault: true,
            },
        });
        await server.close();

        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { id: TRADING, name: 'Trading', is_registration_default: true },
                precondition: { kind: 'must_match_version', version: 2 },
            },
        });
    });

    it('saves exactly the set of permissions the screen sends', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.roles_permissions.put': (body) => ({
                result: { outcome: 'ok' },
                permission_codes: (body as { permission_codes: string[] }).permission_codes,
            }),
        });

        const response = await server.inject({
            method: 'PUT',
            url: `/api/roles/${TRADING}/permissions`,
            cookies,
            payload: { codes: ['refdata::*', 'iam::accounts:read'], note: 'Desk needs it' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ codes: ['refdata::*', 'iam::accounts:read'] });
        expect(calls[0]?.body).toMatchObject({
            role_id: TRADING,
            change_commentary: 'Desk needs it',
        });
    });

    it('answers an account picture, and no picture for an account without one', async () => {
        const account = (image: string | null) => ({
            result: { outcome: 'ok' },
            account: {
                version: 1,
                id: DANIEL,
                tenant_id: SYSTEM_TENANT,
                username: 'daniel',
                full_name: 'Daniel Okafor',
                email: 'daniel@acme.example',
                account_type: 'user',
                job_title: '',
                image_id: image,
                modified_by: 'priya',
                change_reason_code: 'system.new_record',
                change_commentary: '',
                performed_by: 'priya',
                recorded_at: '2026-10-04 09:00:00Z',
            },
        });
        const withPicture = buildTestServer({
            'iam.v1.accounts.get': account(PHOTO),
            'assets.v1.images.list': {
                result: { outcome: 'ok' },
                images: [{ id: PHOTO, mime_type: 'image/jpeg', data: [0xff, 0xd8] }],
            },
        });
        const picture = await withPicture.server.inject({
            method: 'GET',
            url: '/api/accounts/daniel/picture',
            cookies: withPicture.cookies,
        });
        await withPicture.server.close();
        const without = buildTestServer({ 'iam.v1.accounts.get': account(null) });
        const none = await without.server.inject({
            method: 'GET',
            url: '/api/accounts/daniel/picture',
            cookies: without.cookies,
        });
        await without.server.close();

        expect(picture.statusCode).toBe(200);
        expect(picture.headers['content-type']).toBe('image/jpeg');
        expect(picture.headers['x-content-type-options']).toBe('nosniff');
        expect(picture.headers['content-security-policy']).toContain('sandbox');
        expect(none.statusCode).toBe(404);
    });
});
