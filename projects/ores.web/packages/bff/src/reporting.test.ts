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
const ENVIRONMENT_ID = 'bright_faraday';
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

const ok = { outcome: 'ok', code: '', message: '', fields: [] };

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

const PARTY = '77777777-7777-4777-8777-777777777777';
const OTHER_PARTY = '88888888-8888-4888-8888-888888888888';
const DAILY = '55555555-5555-4555-8555-555555555555';
const MONTH_END = '66666666-6666-4666-8666-666666666666';
const DAILY_RUN = '99999999-9999-4999-8999-999999999999';
const QUEST = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const INTENT = { reason_code: 'common.non_material_update', commentary: '' };

const DEFINITIONS = 'reporting.v1.report_definitions.list';
const LATEST = 'reporting.v1.ops.get_latest_report_instances_by_definition';
const HEALTH = 'reporting.v1.ops.get_report_instance_health';
const COUNTS = 'reporting.v1.ops.get_report_error_counts';
const NEXT_FIRE = 'reporting.v1.ops.get_next_fire';
const MY_PARTIES = 'iam.v1.ops.get_my_parties';

function definition(id: string, name: string, reportType: string): Record<string, unknown> {
    return {
        version: 3,
        id,
        name,
        party_id: PARTY,
        description: `${name} definition`,
        report_type: reportType,
        fsm_state_id: null,
        schedule_expression: '0 0 9 * * ?',
        concurrency_policy: 'forbid',
        scheduler_job_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
        run_grant_id: null,
        pre_processing: 'none',
        prepared_input_key: 'acme/daily',
        post_processing: 'none',
        is_official: true,
        modified_by: 'priya',
        performed_by: 'priya',
        change_reason_code: 'common.new_record',
        change_commentary: '',
        recorded_at: '2026-10-17 08:00:00.000000Z',
    };
}

/** The reads a list of these definitions makes, with the values this file asserts. */
function listReplies(): Record<string, unknown> {
    return {
        [DEFINITIONS]: {
            result: ok,
            definitions: [
                definition(DAILY, 'ACME daily PnL', 'pnl'),
                definition(MONTH_END, 'ACME month-end risk', 'risk'),
            ],
            total: 2,
        },
        [MY_PARTIES]: {
            result: ok,
            default_party_id: PARTY,
            parties: [
                {
                    party_id: PARTY,
                    name: 'ACME Corporation',
                    short_code: 'ACMCOR',
                    party_category: 'Operational',
                    business_center_code: 'GBLO',
                },
            ],
        },
        [LATEST]: {
            result: ok,
            instances: [
                {
                    id: DAILY_RUN,
                    definition_id: DAILY,
                    started_at: '2026-10-17 08:00:00.000000Z',
                    completed_at: '2026-10-17 08:00:12.500000Z',
                },
            ],
        },
        [HEALTH]: {
            result: ok,
            health: [
                {
                    report_instance_id: DAILY_RUN,
                    state_name: 'completed',
                    health: 'completed with errors',
                },
            ],
        },
        [COUNTS]: {
            result: ok,
            counts: [{ report_instance_id: DAILY_RUN, errors: 3, warnings: 2 }],
        },
        [NEXT_FIRE]: {
            result: ok,
            schedules: [
                { definition_id: DAILY, next_fire_at: '2026-10-18 09:00:00.000000Z' },
                { definition_id: MONTH_END, next_fire_at: null },
            ],
        },
    };
}

describe('reporting routes', () => {
    it('composes one row per definition from the six reads it makes', async () => {
        const { server, sessionId, calls } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            rows: [
                {
                    name: 'ACME daily PnL',
                    reportType: 'pnl',
                    partyId: PARTY,
                    partyCode: 'ACMCOR',
                    scheduleExpression: '0 0 9 * * ?',
                    health: 'completed with errors',
                    stateName: 'completed',
                    latestRun: {
                        id: DAILY_RUN,
                        startedAt: '2026-10-17 08:00:00.000000Z',
                        completedAt: '2026-10-17 08:00:12.500000Z',
                        errors: 3,
                        warnings: 2,
                    },
                    nextFireAt: '2026-10-18 09:00:00.000000Z',
                },
                {
                    name: 'ACME month-end risk',
                    reportType: 'risk',
                    partyId: PARTY,
                    partyCode: 'ACMCOR',
                    scheduleExpression: '0 0 9 * * ?',
                    health: 'never run',
                    stateName: '',
                    latestRun: null,
                    nextFireAt: null,
                },
            ],
            tally: {
                failed: 0,
                completedWithErrors: 1,
                running: 0,
                neverRun: 1,
                completedClean: 0,
                unknown: 0,
                unstated: 0,
            },
            unread: [],
        });
        expect(calls.map((call) => call.subject)).toEqual([
            DEFINITIONS,
            MY_PARTIES,
            LATEST,
            NEXT_FIRE,
            HEALTH,
            COUNTS,
        ]);
    });

    it('keeps the row and states the reason when one read fails', async () => {
        const replies = listReplies();
        delete replies[NEXT_FIRE];
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.rows).toHaveLength(2);
        expect(body.rows[0].name).toBe('ACME daily PnL');
        expect(body.rows[0].nextFireAt).toBeNull();
        expect(body.rows[0].health).toBe('completed with errors');
        expect(body.rows[0].latestRun.errors).toBe(3);
        expect(body.unread).toEqual([
            { cell: 'nextFire', message: `No canned reply for ${NEXT_FIRE}` },
        ]);
    });

    it('leaves health unknown and records the unread latest run when that read fails', async () => {
        const replies = listReplies();
        delete replies[LATEST];
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        const body = response.json();
        // No run was observed, so there is no word to state: never run is a fact
        // about the definition and this read is a fact about the request.
        expect(body.rows.map((row: Record<string, unknown>) => row.health)).toEqual([null, null]);
        expect(body.rows[0].latestRun).toBeNull();
        // The cells the other reads filled survive the one that failed.
        expect(body.rows[0].partyCode).toBe('ACMCOR');
        expect(body.rows[0].nextFireAt).toBe('2026-10-18 09:00:00.000000Z');
        expect(body.unread).toEqual([
            { cell: 'latestRun', message: `No canned reply for ${LATEST}` },
        ]);
        expect(body.tally).toEqual({
            failed: 0,
            completedWithErrors: 0,
            running: 0,
            neverRun: 0,
            completedClean: 0,
            unknown: 2,
            unstated: 0,
        });
    });

    it('refuses the list when the definitions themselves cannot be read', async () => {
        const { server, sessionId } = buildTestServer({
            [DEFINITIONS]: { result: { ...ok, outcome: 'denied', message: 'Refused.' } },
        });

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(403);
        expect(response.json().code).toBe('forbidden');
    });

    it('states a party that owns no definition as no rows, not as a failure', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [DEFINITIONS]: { result: ok, definitions: [], total: 0 },
        });

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            rows: [],
            tally: {
                failed: 0,
                completedWithErrors: 0,
                running: 0,
                neverRun: 0,
                completedClean: 0,
                unknown: 0,
                unstated: 0,
            },
            unread: [],
        });
        expect(calls).toHaveLength(1);
    });

    it('reads one definition with the row the list would draw for it', async () => {
        const replies = listReplies();
        const { server, sessionId, calls } = buildTestServer({
            ...replies,
            [LATEST]: {
                result: ok,
                instances: [
                    {
                        id: DAILY_RUN,
                        definition_id: DAILY,
                        started_at: '2026-10-17 08:00:00.000000Z',
                        completed_at: '2026-10-17 08:00:12.500000Z',
                    },
                ],
            },
            'reporting.v1.report_definitions.get': {
                result: ok,
                report_definition: definition(DAILY, 'ACME daily PnL', 'pnl'),
            },
        });

        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/reports/ACME%20daily%20PnL',
        );

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.report.name).toBe('ACME daily PnL');
        expect(body.report.health).toBe('completed with errors');
        expect(body.definition).toEqual({
            name: 'ACME daily PnL',
            description: 'ACME daily PnL definition',
            reportType: 'pnl',
            partyId: PARTY,
            scheduleExpression: '0 0 9 * * ?',
            concurrencyPolicy: 'forbid',
            isOfficial: true,
            preProcessing: 'none',
            preparedInputKey: 'acme/daily',
            postProcessing: 'none',
            schedulerJobId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
            runGrantId: null,
            fsmStateId: null,
            version: 3,
            modifiedBy: 'priya',
            performedBy: 'priya',
            changeReasonCode: 'common.new_record',
            changeCommentary: '',
            recordedAt: '2026-10-17 08:00:00.000000Z',
        });
        expect(calls[0]).toEqual({
            subject: 'reporting.v1.report_definitions.get',
            body: { key: { name: 'ACME daily PnL' } },
        });
    });

    it('answers a definition that does not exist as absent', async () => {
        const { server, sessionId } = buildTestServer({
            'reporting.v1.report_definitions.get': { result: ok, report_definition: null },
        });

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports/none');

        expect(response.statusCode).toBe(404);
        expect(response.json().code).toBe('not-found');
    });

    it('writes the six offered fields and rebuilds the rest from the read', async () => {
        const replies = listReplies();
        const { server, sessionId, calls } = buildTestServer({
            ...replies,
            'reporting.v1.report_definitions.get': {
                result: ok,
                report_definition: definition(DAILY, 'ACME daily PnL', 'pnl'),
            },
            'reporting.v1.report_definitions.put': {
                result: ok,
                report_definition: {
                    ...definition(DAILY, 'ACME daily PnL', 'pnl'),
                    schedule_expression: '0 30 8 * * ?',
                    version: 4,
                },
            },
        });

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/reporting/reports/ACME%20daily%20PnL',
            {
                version: 3,
                change: {
                    description: 'Daily PnL for the desk',
                    schedule_expression: '0 30 8 * * ?',
                    is_official: false,
                    pre_processing: 'none',
                    prepared_input_key: 'acme/daily',
                    post_processing: 'none',
                },
                intent: INTENT,
            },
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().definition.scheduleExpression).toBe('0 30 8 * * ?');
        expect(response.json().definition.version).toBe(4);
        expect(calls.map((call) => call.subject)).toContain('reporting.v1.report_definitions.put');
        const put = calls.find((call) => call.subject === 'reporting.v1.report_definitions.put')
            ?.body as { change: { write: Record<string, unknown>; precondition: unknown } };
        expect(put.change.precondition).toEqual({ kind: 'must_match_version', version: 3 });
        expect(put.change.write).toEqual({
            id: DAILY,
            name: 'ACME daily PnL',
            party_id: PARTY,
            description: 'Daily PnL for the desk',
            report_type: 'pnl',
            fsm_state_id: null,
            schedule_expression: '0 30 8 * * ?',
            concurrency_policy: 'forbid',
            scheduler_job_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
            run_grant_id: null,
            pre_processing: 'none',
            prepared_input_key: 'acme/daily',
            post_processing: 'none',
            is_official: false,
        });
    });

    it('reports a refused write with the status the server stated', async () => {
        const { server, sessionId } = buildTestServer({
            'reporting.v1.report_definitions.get': {
                result: ok,
                report_definition: definition(DAILY, 'ACME daily PnL', 'pnl'),
            },
            'reporting.v1.report_definitions.put': {
                result: { ...ok, outcome: 'conflict', message: 'The definition moved on.' },
            },
        });

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/reporting/reports/ACME%20daily%20PnL',
            {
                version: 3,
                change: {
                    description: '',
                    schedule_expression: '0 0 9 * * ?',
                    is_official: true,
                    pre_processing: 'none',
                    prepared_input_key: 'acme/daily',
                    post_processing: 'none',
                },
                intent: INTENT,
            },
        );

        expect(response.statusCode).toBe(409);
        expect(response.json()).toEqual({
            code: 'conflict',
            message: 'The definition moved on.',
        });
    });

    it('refuses a write that states no intent', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/reporting/reports/ACME%20daily%20PnL',
            {
                version: 3,
                change: {
                    description: '',
                    schedule_expression: '0 0 9 * * ?',
                    is_official: true,
                    pre_processing: 'none',
                    prepared_input_key: 'acme/daily',
                    post_processing: 'none',
                },
            },
        );
        expect(response.statusCode).toBe(400);
    });

    it('leaves the party cell empty when the parties cannot be named', async () => {
        const replies = listReplies();
        delete replies[MY_PARTIES];
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.json().rows[0].partyCode).toBeNull();
        expect(response.json().unread).toEqual([
            { cell: 'parties', message: `No canned reply for ${MY_PARTIES}` },
        ]);
    });

    it('states a party the caller does not work in as an empty cell', async () => {
        const replies = listReplies();
        replies[MY_PARTIES] = {
            result: ok,
            default_party_id: OTHER_PARTY,
            parties: [
                {
                    party_id: OTHER_PARTY,
                    name: 'Other',
                    short_code: 'OTHER',
                    party_category: 'Operational',
                    business_center_code: 'USNY',
                },
            ],
        };
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.json().rows[0].partyId).toBe(PARTY);
        expect(response.json().rows[0].partyCode).toBeNull();
        expect(response.json().unread).toEqual([]);
    });
});
