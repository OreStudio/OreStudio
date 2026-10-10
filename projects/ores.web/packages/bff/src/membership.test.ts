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

/**
 * The membership route: the parties the signed-in person works in.
 *
 * The association read answers with an identifier alone, so the server names
 * each party. These cases pin what the BFF decides: the shape it answers, and
 * that a party the server cannot name arrives as a stated gap rather than a
 * missing row.
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

const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const PRIYA = '11111111-1111-1111-1111-111111111111';
const UK = '22222222-2222-2222-2222-222222222222';
const US = '33333333-3333-3333-3333-333333333333';

type Answer = unknown | ((body: unknown) => unknown);

/** One account as the server writes it, for the reporting-line reply. */
function wireAccount(overrides: Record<string, unknown> = {}) {
    return {
        version: 4,
        id: PRIYA,
        tenant_id: SYSTEM_TENANT,
        username: 'priya',
        full_name: 'Priya Raman',
        email: 'priya@acme.example',
        account_type: 'user',
        job_title: 'Analyst',
        reports_to_account_id: null,
        default_party_id: null,
        image_id: null,
        modified_by: 'priya',
        change_reason_code: 'common.non_material_update',
        change_commentary: '',
        performed_by: 'priya',
        recorded_at: '2026-10-09 12:00:00Z',
        ...overrides,
    };
}

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
        availableParties: [],
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
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

describe('membership routes', () => {
    it('answers the parties the person works in with their names, and the default', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.get_my_parties': {
                result: { outcome: 'ok' },
                default_party_id: US,
                parties: [
                    {
                        party_id: UK,
                        name: 'ACME Corporation UK plc',
                        short_code: 'ACCOUK',
                        party_category: 'Operational',
                        business_center_code: 'GBLO',
                    },
                    {
                        party_id: US,
                        name: 'ACME Corporation US Inc',
                        short_code: 'ACCOUS',
                        party_category: 'Operational',
                        business_center_code: 'USNY',
                    },
                ],
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/parties', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            defaultPartyId: US,
            parties: [
                {
                    partyId: UK,
                    name: 'ACME Corporation UK plc',
                    shortCode: 'ACCOUK',
                    partyCategory: 'Operational',
                    businessCenterCode: 'GBLO',
                },
                {
                    partyId: US,
                    name: 'ACME Corporation US Inc',
                    shortCode: 'ACCOUS',
                    partyCategory: 'Operational',
                    businessCenterCode: 'USNY',
                },
            ],
        });
        expect(calls).toEqual([{ subject: 'iam.v1.ops.get_my_parties', body: {} }]);
    });

    it('states a party the server cannot name instead of dropping it', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.get_my_parties': {
                result: { outcome: 'ok' },
                default_party_id: '',
                parties: [
                    {
                        party_id: UK,
                        name: '',
                        short_code: '',
                        party_category: '',
                        business_center_code: '',
                    },
                ],
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/parties', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            defaultPartyId: '',
            parties: [
                {
                    partyId: UK,
                    name: '',
                    shortCode: '',
                    partyCategory: '',
                    businessCenterCode: '',
                },
            ],
        });
    });

    it('refuses a caller with no session', async () => {
        const { server } = buildTestServer({});

        const response = await server.inject({ method: 'GET', url: '/api/me/parties' });
        await server.close();

        expect(response.statusCode).toBe(401);
    });

    it('sets the default party the member chose', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.set_my_default_party': { success: true, message: '' },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/default-party',
            cookies,
            payload: { partyId: US },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ defaultPartyId: US });
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.set_my_default_party', body: { party_id: US } },
        ]);
    });

    it('clears the default when the party is empty', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.set_my_default_party': { success: true, message: '' },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/default-party',
            cookies,
            payload: { partyId: '' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ defaultPartyId: '' });
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.set_my_default_party', body: { party_id: '' } },
        ]);
    });

    it('reports the server\u2019s refusal when the party is not the member\u2019s', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.set_my_default_party': {
                success: false,
                message: 'User is not a member of requested party',
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/default-party',
            cookies,
            payload: { partyId: UK },
        });
        await server.close();

        expect(response.statusCode).toBeGreaterThanOrEqual(400);
        expect(response.body).toContain('not a member');
    });

    it('writes who an account reports to and states the version the screen read', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.set_reporting_line': {
                result: { outcome: 'ok' },
                account: wireAccount({ reports_to_account_id: US, version: 4 }),
            },
        });

        const response = await server.inject({
            method: 'PUT',
            url: `/api/accounts/${PRIYA}/reporting-line`,
            cookies,
            payload: { reportsToAccountId: US, expectedVersion: '3' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            {
                subject: 'iam.v1.ops.set_reporting_line',
                body: {
                    account_id: PRIYA,
                    reports_to_account_id: US,
                    expected_version: '3',
                    change_reason_code: '',
                    change_commentary: '',
                },
            },
        ]);
        expect(response.json()).toMatchObject({ id: PRIYA, reportsToAccountId: US, version: 4 });
    });

    it('reports a version conflict in the server\u2019s words', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.set_reporting_line': {
                result: {
                    outcome: 'conflict',
                    message: 'The account has changed since it was read.',
                },
                account: null,
            },
        });

        const response = await server.inject({
            method: 'PUT',
            url: `/api/accounts/${PRIYA}/reporting-line`,
            cookies,
            payload: { reportsToAccountId: US, expectedVersion: '1' },
        });
        await server.close();

        expect(response.statusCode).toBeGreaterThanOrEqual(400);
        expect(response.body).toContain('changed since');
    });

    it('answers the reporting shape with each person\u2019s manager and depth', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.get_reporting_tree': {
                result: { outcome: 'ok' },
                unrooted: 0,
                nodes: [
                    {
                        account_id: PRIYA,
                        username: 'priya',
                        full_name: 'Priya Raman',
                        job_title: 'Head of Desk',
                        image_id: US,
                        reports_to_account_id: '',
                        reports_outside_scope: false,
                        depth: 0,
                        direct_reports: 1,
                        party_ids: [UK, US],
                    },
                    {
                        account_id: UK,
                        username: 'uk.person',
                        full_name: 'UK Person',
                        job_title: 'Analyst',
                        image_id: '',
                        reports_to_account_id: PRIYA,
                        reports_outside_scope: false,
                        depth: 1,
                        direct_reports: 0,
                        party_ids: [UK],
                    },
                ],
                parties: [
                    {
                        party_id: UK,
                        name: 'ACME UK plc',
                        short_code: 'ACCOUK',
                        parent_party_id: US,
                    },
                    {
                        party_id: US,
                        name: 'ACME US Inc',
                        short_code: 'ACCOUS',
                        parent_party_id: '',
                    },
                ],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/reporting-tree',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.get_reporting_tree', body: { root_account_id: '' } },
        ]);
        expect(response.json()).toEqual({
            unrooted: 0,
            nodes: [
                {
                    accountId: PRIYA,
                    username: 'priya',
                    fullName: 'Priya Raman',
                    jobTitle: 'Head of Desk',
                    imageId: US,
                    reportsToAccountId: null,
                    reportsOutsideScope: false,
                    depth: 0,
                    directReports: 1,
                    partyIds: [UK, US],
                },
                {
                    accountId: UK,
                    username: 'uk.person',
                    fullName: 'UK Person',
                    jobTitle: 'Analyst',
                    imageId: null,
                    reportsToAccountId: PRIYA,
                    reportsOutsideScope: false,
                    depth: 1,
                    directReports: 0,
                    partyIds: [UK],
                },
            ],
            parties: [
                { partyId: UK, name: 'ACME UK plc', shortCode: 'ACCOUK', parentPartyId: US },
                { partyId: US, name: 'ACME US Inc', shortCode: 'ACCOUS', parentPartyId: null },
            ],
        });
    });

    it('asks for one branch when a root is named, and states the unrooted count', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.get_reporting_tree': {
                result: { outcome: 'ok' },
                unrooted: 1,
                nodes: [
                    {
                        account_id: UK,
                        username: 'uk.person',
                        full_name: 'UK Person',
                        job_title: 'Analyst',
                        reports_to_account_id: '',
                        reports_outside_scope: true,
                        depth: -1,
                        direct_reports: 0,
                    },
                ],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: `/api/reporting-tree?root=${UK}`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.get_reporting_tree', body: { root_account_id: UK } },
        ]);
        expect(response.json()).toEqual({
            unrooted: 1,
            nodes: [
                {
                    accountId: UK,
                    username: 'uk.person',
                    fullName: 'UK Person',
                    jobTitle: 'Analyst',
                    imageId: null,
                    reportsToAccountId: null,
                    reportsOutsideScope: true,
                    depth: -1,
                    directReports: 0,
                    partyIds: [],
                },
            ],
            parties: [],
        });
    });
});
