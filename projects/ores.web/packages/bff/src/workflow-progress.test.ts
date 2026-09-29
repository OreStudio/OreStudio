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
import type {
    OresClient,
    RetryWorkflowInstanceRequest,
    WorkflowProgress,
} from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The two routes a journey follows a run through.
 *
 * The rail is a rendering of the progress read and the retry asks the engine
 * to resume a stopped run, so what is asserted here is the answer a browser
 * receives and the input the client method was given.
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

interface TestServer {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly progressReads: string[];
    readonly retries: RetryWorkflowInstanceRequest[];
}

function buildTestServer(progress: unknown, retry: unknown): TestServer {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const progressReads: string[] = [];
    const retries: RetryWorkflowInstanceRequest[] = [];
    const client = {
        async workflowProgress(instanceId: string): Promise<unknown> {
            progressReads.push(instanceId);
            return progress;
        },
        async retryWorkflowInstance(input: RetryWorkflowInstanceRequest): Promise<unknown> {
            retries.push(input);
            return retry;
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'admin',
        email: 'admin@acme.test',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'System',
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
        progressReads,
        retries,
    };
}

const instanceId = '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d';

const progress: WorkflowProgress = {
    success: true,
    message: '',
    status: 'failed',
    error: 'The bundle did not publish.',
    step_count: 4,
    current_step_index: 2,
    steps: [
        {
            id: 'aaaa1111-1111-1111-1111-111111111111',
            name: 'publish_bundle',
            label: 'Publish the reference data',
            description: 'Publishes the reference data the tenant works from.',
            status: 'completed',
            step_index: 0,
            created_at: '2026-09-28T17:00:00Z',
            started_at: '2026-09-28T17:00:01Z',
            completed_at: '2026-09-28T17:00:10Z',
            error: '',
            log: [],
        },
        {
            id: 'bbbb2222-2222-2222-2222-222222222222',
            name: 'provision_party',
            label: "Create the tenant's parties",
            description: 'Creates the party that represents the tenant itself.',
            status: 'failed',
            step_index: 2,
            created_at: '2026-09-28T17:00:11Z',
            started_at: '2026-09-28T17:00:12Z',
            completed_at: '2026-09-28T17:00:20Z',
            error: 'The bundle did not publish.',
            log: [{ level: 'error', message: 'The bundle did not publish.', context: '' }],
        },
    ],
};

const retried = {
    success: true,
    message: '',
    instanceId,
    stepIndex: 2,
    stepName: 'provision_party',
};

describe('GET /api/provision-tenant/:instanceId', () => {
    it('serves the run the browser asked about and passes its id on', async () => {
        const { server, sessionId, progressReads } = buildTestServer(progress, retried);

        const response = await server.inject({
            method: 'GET',
            url: `/api/provision-tenant/${instanceId}`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(progress);
        expect(progressReads).toEqual([instanceId]);

        await server.close();
    });

    it('refuses a progress read with no session', async () => {
        const { server } = buildTestServer(progress, retried);

        const response = await server.inject({
            method: 'GET',
            url: `/api/provision-tenant/${instanceId}`,
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });
});

describe('POST /api/provision-tenant/:instanceId/retry', () => {
    it('resumes the run from the step the engine names, and serves which step it resumed', async () => {
        const { server, sessionId, retries } = buildTestServer(progress, retried);

        const response = await server.inject({
            method: 'POST',
            url: `/api/provision-tenant/${instanceId}/retry`,
            cookies: { ores_web_session: sessionId },
            payload: {},
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(retried);
        expect(retries).toEqual([{ workflowInstanceId: instanceId, stepName: '' }]);

        await server.close();
    });

    it('passes the step a person chose when the body names one', async () => {
        const { server, sessionId, retries } = buildTestServer(progress, retried);

        const response = await server.inject({
            method: 'POST',
            url: `/api/provision-tenant/${instanceId}/retry`,
            cookies: { ores_web_session: sessionId },
            payload: { stepName: 'provision_party' },
        });

        expect(response.statusCode).toBe(200);
        expect(retries).toEqual([{ workflowInstanceId: instanceId, stepName: 'provision_party' }]);

        await server.close();
    });

    it('serves a refusal the engine stated as a result, not as a failed call', async () => {
        const refusal = {
            success: false,
            message: 'The run has not stopped, so there is nothing to resume.',
            instanceId,
            stepIndex: -1,
            stepName: '',
        };
        const { server, sessionId } = buildTestServer(progress, refusal);

        const response = await server.inject({
            method: 'POST',
            url: `/api/provision-tenant/${instanceId}/retry`,
            cookies: { ores_web_session: sessionId },
            payload: {},
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(refusal);

        await server.close();
    });

    it('refuses a retry with no session', async () => {
        const { server } = buildTestServer(progress, retried);

        const response = await server.inject({
            method: 'POST',
            url: `/api/provision-tenant/${instanceId}/retry`,
            payload: {},
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });
});
