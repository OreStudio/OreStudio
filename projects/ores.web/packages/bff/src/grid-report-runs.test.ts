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
import type { OresClient, SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The report run route: the run behind a batch, read on the session's own client.
 *
 * What is checked is what the BFF decides: the id is checked first, a run the
 * session cannot see is a 404, and the health word and the batch are decorations
 * that leave a cell empty when their reads fail.
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

const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const ACCOUNT = '11111111-1111-4111-8111-111111111111';
const APP = '22222222-2222-4222-8222-222222222222';
const VERSION = '33333333-3333-4333-8333-333333333333';
const OLDER = '44444444-4444-4444-8444-444444444444';
const LINUX = '55555555-5555-4555-8555-555555555555';
const WINDOWS = '66666666-6666-4666-8666-666666666666';

const RUN = '22222222-2222-4222-8222-222222222222';
const DEFINITION = '33333333-3333-4333-8333-333333333333';
const PARTY = '44444444-4444-4444-8444-444444444444';
const BATCH = '55555555-5555-4555-8555-555555555555';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };

const wireRun = {
    version: 2,
    tenant_id: SYSTEM_TENANT,
    id: RUN,
    name: 'nightly_instance',
    description: 'A nightly run',
    party_id: PARTY,
    definition_id: DEFINITION,
    kind: 'production',
    run_grant_id: null,
    fsm_state_id: null,
    trigger_id: '66666666-6666-4666-8666-666666666666',
    output_message: 'All trades priced',
    output_storage_key: '',
    started_at: '2026-10-23 07:00:00.000000Z',
    completed_at: null,
    modified_by: 'scheduler',
    performed_by: 'scheduler',
    change_reason_code: 'system.new_record',
    change_commentary: '',
    recorded_at: '2026-10-23 07:00:00.000000Z',
};

const wireBatch = (externalRef: string) => ({
    version: 1,
    tenant_id: SYSTEM_TENANT,
    id: BATCH,
    party_id: PARTY,
    external_ref: externalRef,
    status: 'dispatched',
    modified_by: 'scheduler',
    performed_by: 'scheduler',
    change_reason_code: 'system.new_record',
    change_commentary: '',
    recorded_at: '2026-10-23 07:00:01.000000Z',
});

type Answers = Readonly<Record<string, unknown>>;

const ANSWERS: Answers = {
    'reporting.v1.report_instances.list': { result: OK, instances: [wireRun], total: 1 },
    'reporting.v1.ops.get_report_instance_health': {
        result: OK,
        health: [{ report_instance_id: RUN, health: 'running', state_name: 'Running' }],
    },
    'compute.v1.batches.list': { result: OK, batches: [wireBatch(RUN)], total: 1 },
    'reporting.v1.report_definitions.list': {
        result: OK,
        definitions: [{ id: DEFINITION, name: 'NPV' }],
        total: 1,
    },
};

function buildTestServer(mode: SessionMode, answers: Answers = ANSWERS) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in answers)) {
                throw new Error(`unexpected call ${subject}`);
            }
            return schema.parse(answers[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'sysadmin',
        email: 'sysadmin@acme.example',
        accountId: ACCOUNT,
        tenantId: SYSTEM_TENANT,
        tenantName: 'System',
        mode,
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: SYSTEM_TENANT,
                name: 'System',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

async function get(mode: SessionMode, url: string, answers: Answers = ANSWERS) {
    const { server, cookies, calls } = buildTestServer(mode, answers);
    const response = await server.inject({ method: 'GET', url, cookies });
    await server.close();
    return { response, calls };
}

const URL_OF_RUN = `/api/operations/grid/report-runs/${RUN}`;

describe('GET /api/operations/grid/report-runs/:id', () => {
    it('serves the run with its health word and the batch that names it', async () => {
        const { response, calls } = await get('application', URL_OF_RUN);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            id: RUN,
            name: 'nightly_instance',
            definition_id: DEFINITION,
            kind: 'production',
            health: 'running',
            state_name: 'Running',
            batch_id: BATCH,
            completed_at: null,
            definition_name: 'NPV',
            party_name: null,
        });
        expect(calls[0]?.body).toMatchObject({ filter: { id_one_of: [RUN] }, limit: 1 });
    });

    it('states no batch when none of the visible batches names the run', async () => {
        const { response } = await get('application', URL_OF_RUN, {
            ...ANSWERS,
            'compute.v1.batches.list': { result: OK, batches: [wireBatch('another')], total: 1 },
        });

        expect(response.json<{ batch_id: string | null }>().batch_id).toBeNull();
    });

    it('serves the run when the health and the batch reads fail', async () => {
        const answers: Record<string, unknown> = { ...ANSWERS };
        delete answers['reporting.v1.ops.get_report_instance_health'];
        delete answers['compute.v1.batches.list'];
        const { response } = await get('application', URL_OF_RUN, answers);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ id: RUN, health: null, batch_id: null });
    });

    it('answers 404 when the session sees no such run', async () => {
        const { response } = await get('application', URL_OF_RUN, {
            ...ANSWERS,
            'reporting.v1.report_instances.list': { result: OK, instances: [], total: 0 },
        });

        expect(response.statusCode).toBe(404);
    });

    it('answers 403 when the reporting service denies the read', async () => {
        const { response } = await get('application', URL_OF_RUN, {
            ...ANSWERS,
            'reporting.v1.report_instances.list': {
                result: { ...OK, outcome: 'denied', message: 'no' },
                instances: [],
                total: 0,
            },
        });

        expect(response.statusCode).toBe(403);
    });

    it('refuses an id that is not a uuid before any call goes out', async () => {
        const { response, calls } = await get(
            'application',
            '/api/operations/grid/report-runs/nope',
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});
