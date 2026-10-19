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

/** A canned reply, or a function of the request body for a read whose answer depends on it. */
type Canned = unknown | ((body: unknown) => unknown);

function buildTestServer(replies: Readonly<Record<string, Canned>>): {
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
            const canned = replies[subject];
            return schema.parse(typeof canned === 'function' ? canned(body) : canned);
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

/** The login party, and a party below it that the same account also works in. */
const PARTY = '77777777-7777-4777-8777-777777777777';
const OTHER_PARTY = '88888888-8888-4888-8888-888888888888';

const DAILY = '55555555-5555-4555-8555-555555555555';
const AUDIT_AT_HOME = '66666666-6666-4666-8666-666666666666';
const AUDIT_ELSEWHERE = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const DAILY_RUN = '99999999-9999-4999-8999-999999999999';
const UNKNOWN_ID = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const INTENT = { reason_code: 'common.non_material_update', commentary: '' };

const DEFINITIONS = 'reporting.v1.report_definitions.list';
const LATEST = 'reporting.v1.ops.get_latest_report_instances_by_definition';
const HEALTH = 'reporting.v1.ops.get_report_instance_health';
const COUNTS = 'reporting.v1.ops.get_report_error_counts';
const NEXT_FIRE = 'reporting.v1.ops.get_next_fire';
const MY_PARTIES = 'iam.v1.ops.get_my_parties';
const PUT = 'reporting.v1.report_definitions.put';

/** The name two parties both give a definition. */
const AUDIT_NAME = 'Audit Trail Summary';

function definition(
    id: string,
    name: string,
    reportType: string,
    partyId: string = PARTY,
): Record<string, unknown> {
    return {
        version: 3,
        id,
        name,
        party_id: partyId,
        description: `${name} definition`,
        report_type: reportType,
        fsm_state_id: null,
        schedule_expression: '0 0 9 * * ?',
        concurrency_policy: 'forbid',
        scheduler_job_id: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
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

/** Every definition the session's parties own, in the order the read states them. */
const ALL_DEFINITIONS = [
    definition(DAILY, 'ACME daily PnL', 'pnl'),
    definition(AUDIT_AT_HOME, AUDIT_NAME, 'risk'),
    definition(AUDIT_ELSEWHERE, AUDIT_NAME, 'risk', OTHER_PARTY),
];

/**
 * The definitions read, answered as the generated read answers it: the page it
 * was asked for, narrowed by the party and by the ids it named, with the total
 * of every row that matches.
 */
function definitionsReply(body: unknown): Record<string, unknown> {
    const request = (body ?? {}) as {
        offset?: number;
        limit?: number;
        filter?: { id_one_of?: string[] | null; party_id?: string | null } | null;
    };
    let rows = ALL_DEFINITIONS;
    // A member that is set narrows the rows; one stated as null narrows nothing.
    const ids = request.filter?.id_one_of;
    const partyId = request.filter?.party_id;
    if (ids != null) {
        rows = rows.filter((row) => ids.includes(String(row['id'])));
    }
    if (partyId != null) {
        rows = rows.filter((row) => row['party_id'] === partyId);
    }
    const offset = request.offset ?? 0;
    const limit = request.limit ?? rows.length;
    return { result: ok, definitions: rows.slice(offset, offset + limit), total: rows.length };
}

/** The reads a list of these definitions makes, with the values this file asserts. */
function listReplies(): Record<string, Canned> {
    return {
        [DEFINITIONS]: definitionsReply,
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
                {
                    party_id: OTHER_PARTY,
                    name: 'ACME Hong Kong',
                    short_code: 'ACCOHK',
                    party_category: 'Operational',
                    business_center_code: 'HKHH',
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
                { definition_id: AUDIT_AT_HOME, next_fire_at: null },
                { definition_id: AUDIT_ELSEWHERE, next_fire_at: null },
            ],
        },
    };
}

/** The subjects a read of the list makes, in the order it makes them. */
const LIST_SUBJECTS = [DEFINITIONS, MY_PARTIES, LATEST, NEXT_FIRE, HEALTH, COUNTS];

describe('the list of reports the parties run', () => {
    it('gives every row the id that names one definition exactly', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        expect(response.json().rows.map((row: { id: string }) => row.id)).toEqual([
            DAILY,
            AUDIT_AT_HOME,
            AUDIT_ELSEWHERE,
        ]);
        expect(response.json().total).toBe(3);
    });

    it('composes each row from the reads it makes, and carries no tally nobody counted', async () => {
        const { server, sessionId, calls } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.json().rows[0]).toEqual({
            id: DAILY,
            name: 'ACME daily PnL',
            reportType: 'pnl',
            partyId: PARTY,
            partyName: 'ACME Corporation',
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
        });
        expect(response.json().tally).toBeUndefined();
        expect(calls.map((call) => call.subject)).toEqual(LIST_SUBJECTS);
    });

    it('names each row party by name, so the tag shows a name and not a code', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        const elsewhere = response
            .json()
            .rows.find((row: { id: string }) => row.id === AUDIT_ELSEWHERE);
        expect(elsewhere.partyName).toBe('ACME Hong Kong');
        expect(elsewhere.partyCode).toBe('ACCOHK');
    });

    it('reads one page and states the total the pager is built from', async () => {
        const { server, sessionId, calls } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/reports?offset=1&limit=1',
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toHaveLength(1);
        expect(response.json().rows[0].id).toBe(AUDIT_AT_HOME);
        expect(response.json().total).toBe(3);
        // The page's decorations are read for the page's rows only.
        const latest = calls.find((call) => call.subject === LATEST);
        expect(latest?.body).toEqual({ definition_ids: [AUDIT_AT_HOME] });
    });

    it('narrows the page to one party on the server, not in the browser', async () => {
        const { server, sessionId, calls } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports?party_id=${OTHER_PARTY}`,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().rows.map((row: { id: string }) => row.id)).toEqual([
            AUDIT_ELSEWHERE,
        ]);
        expect(response.json().total).toBe(1);
        const read = calls.find((call) => call.subject === DEFINITIONS);
        // Every member is stated, because the wire decoder reads the filter whole.
        expect((read?.body as { filter: unknown }).filter).toEqual({
            party_id: OTHER_PARTY,
            id_one_of: null,
            party_id_one_of: null,
        });
    });

    it('refuses a page a caller may not ask for', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports?limit=5000');

        expect(response.statusCode).toBe(400);
        expect(response.json().message).toContain('limit');
    });

    it('keeps the row and states the reason when one read fails', async () => {
        const replies = listReplies();
        delete replies[NEXT_FIRE];
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.rows).toHaveLength(3);
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
        expect(body.rows.map((row: Record<string, unknown>) => row.health)).toEqual([
            null,
            null,
            null,
        ]);
        expect(body.rows[0].latestRun).toBeNull();
        // The cells the other reads filled survive the one that failed.
        expect(body.rows[0].partyCode).toBe('ACMCOR');
        expect(body.rows[0].nextFireAt).toBe('2026-10-18 09:00:00.000000Z');
        expect(body.unread).toEqual([
            { cell: 'latestRun', message: `No canned reply for ${LATEST}` },
        ]);
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
        expect(response.json()).toEqual({ rows: [], total: 0, unread: [] });
        expect(calls).toHaveLength(1);
    });

    it('leaves the party cells empty when the parties cannot be named', async () => {
        const replies = listReplies();
        delete replies[MY_PARTIES];
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.json().rows[0].partyCode).toBeNull();
        expect(response.json().rows[0].partyName).toBe('');
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
                    name: 'ACME Hong Kong',
                    short_code: 'ACCOHK',
                    party_category: 'Operational',
                    business_center_code: 'HKHH',
                },
            ],
        };
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'GET', '/api/reporting/reports');

        expect(response.json().rows[0].partyId).toBe(PARTY);
        expect(response.json().rows[0].partyName).toBe('');
        expect(response.json().rows[0].partyCode).toBeNull();
        expect(response.json().unread).toEqual([]);
    });
});

describe('one report definition', () => {
    it('reads the definition the id names, and not one of the same name elsewhere', async () => {
        const { server, sessionId, calls } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${AUDIT_ELSEWHERE}`,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().report.id).toBe(AUDIT_ELSEWHERE);
        expect(response.json().report.partyName).toBe('ACME Hong Kong');
        expect(response.json().definition).toEqual({
            id: AUDIT_ELSEWHERE,
            name: AUDIT_NAME,
            description: `${AUDIT_NAME} definition`,
            report_type: 'risk',
            party_id: OTHER_PARTY,
            party_name: 'ACME Hong Kong',
            party_code: 'ACCOHK',
            schedule_expression: '0 0 9 * * ?',
            concurrency_policy: 'forbid',
            is_official: true,
            pre_processing: 'none',
            prepared_input_key: 'acme/daily',
            post_processing: 'none',
            version: 3,
            modified_by: 'priya',
            performed_by: 'priya',
            change_reason_code: 'common.new_record',
            change_commentary: '',
            recorded_at: '2026-10-17 08:00:00.000000Z',
        });
        // The read names the id, not the name, so two rows of one name cannot be
        // confused for each other.
        expect(calls[0]).toEqual({
            subject: DEFINITIONS,
            body: {
                offset: 0,
                limit: 1,
                order: { field: '', descending: false },
                filter: { party_id: null, id_one_of: [AUDIT_ELSEWHERE], party_id_one_of: null },
                as_of: null,
            },
        });
    });

    it('opens two different records for two definitions of one name', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const home = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${AUDIT_AT_HOME}`,
        );
        const elsewhere = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${AUDIT_ELSEWHERE}`,
        );

        expect(home.json().definition.name).toBe(AUDIT_NAME);
        expect(elsewhere.json().definition.name).toBe(AUDIT_NAME);
        expect(home.json().definition.id).not.toBe(elsewhere.json().definition.id);
        expect(home.json().definition.party_name).toBe('ACME Corporation');
        expect(elsewhere.json().definition.party_name).toBe('ACME Hong Kong');
    });

    it('answers a definition that does not exist as absent', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${UNKNOWN_ID}`,
        );

        expect(response.statusCode).toBe(404);
        expect(response.json().code).toBe('not-found');
    });

    it('opens the definition a unique name in the address names', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/reports/ACME%20daily%20PnL',
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().definition.id).toBe(DAILY);
        expect(response.json().definition.name).toBe('ACME daily PnL');
    });

    it('opens a unique name that holds slashes, sent as one encoded segment', async () => {
        const slashed = definition('dddddddd-dddd-4ddd-8ddd-dddddddddddd', 'CVA/DVA/FVA', 'risk');
        const { server, sessionId } = buildTestServer({
            ...listReplies(),
            [DEFINITIONS]: { result: ok, definitions: [slashed], total: 1 },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${encodeURIComponent('CVA/DVA/FVA')}`,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json().definition.id).toBe(slashed['id']);
    });

    it('answers a name several definitions carry with the choices, and not an id', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/${encodeURIComponent('Audit Trail Summary')}`,
        );

        expect(response.statusCode).toBe(409);
        const body = response.json();
        expect(body.code).toBe('conflict');
        expect(body.message).not.toContain('report_definition');
        expect(body.choices).toEqual([
            {
                id: AUDIT_AT_HOME,
                name: AUDIT_NAME,
                partyName: 'ACME Corporation',
                scheduleExpression: '0 0 9 * * ?',
                health: 'never run',
                stateName: '',
                hasRun: false,
            },
            {
                id: AUDIT_ELSEWHERE,
                name: AUDIT_NAME,
                partyName: 'ACME Hong Kong',
                scheduleExpression: '0 0 9 * * ?',
                health: 'never run',
                stateName: '',
                hasRun: false,
            },
        ]);
    });

    it('answers a name no definition carries as absent, in the standard words', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(server, sessionId, 'GET', '/api/reporting/reports/not-an-id');

        expect(response.statusCode).toBe(404);
        expect(response.json().code).toBe('not-found');
        expect(response.json().message).toBe('There is no such record, or you may not see it.');
    });

    it('resolves a retired name-based address to the one definition that carries it', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/reports/by-name/ACME%20daily%20PnL',
        );

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ id: DAILY });
    });

    it('answers a shared name-based address with the choices, not a refusal to guess', async () => {
        const { server, sessionId } = buildTestServer(listReplies());
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/reports/by-name/${encodeURIComponent(AUDIT_NAME)}`,
        );

        expect(response.statusCode).toBe(409);
        expect(response.json().message).not.toContain('report_definition');
        expect(response.json().choices.map((choice: { id: string }) => choice.id)).toEqual([
            AUDIT_AT_HOME,
            AUDIT_ELSEWHERE,
        ]);
    });

    it('writes the record it read, and not another party row of the same name', async () => {
        const replies = listReplies();
        replies[PUT] = {
            result: ok,
            report_definition: {
                ...definition(AUDIT_ELSEWHERE, AUDIT_NAME, 'risk', OTHER_PARTY),
                schedule_expression: '0 30 8 * * ?',
                version: 4,
            },
        };
        const { server, sessionId, calls } = buildTestServer(replies);

        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/reports/${AUDIT_ELSEWHERE}`,
            {
                version: 3,
                change: {
                    description: 'Audit trail, month end',
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
        expect(response.json().definition.schedule_expression).toBe('0 30 8 * * ?');
        expect(response.json().definition.version).toBe(4);
        const put = calls.find((call) => call.subject === PUT)?.body as {
            change: { write: Record<string, unknown>; precondition: unknown };
        };
        expect(put.change.precondition).toEqual({ kind: 'must_match_version', version: 3 });
        expect(put.change.write).toEqual({
            id: AUDIT_ELSEWHERE,
            name: AUDIT_NAME,
            party_id: OTHER_PARTY,
            description: 'Audit trail, month end',
            report_type: 'risk',
            fsm_state_id: null,
            schedule_expression: '0 30 8 * * ?',
            concurrency_policy: 'forbid',
            scheduler_job_id: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
            run_grant_id: null,
            pre_processing: 'none',
            prepared_input_key: 'acme/daily',
            post_processing: 'none',
            is_official: false,
        });
    });

    it('reports a refused write with the status the server stated', async () => {
        const replies = listReplies();
        replies[PUT] = {
            result: { ...ok, outcome: 'conflict', message: 'The definition moved on.' },
        };
        const { server, sessionId } = buildTestServer(replies);

        const response = await send(server, sessionId, 'PUT', `/api/reporting/reports/${DAILY}`, {
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
        });

        expect(response.statusCode).toBe(409);
        expect(response.json()).toEqual({
            code: 'conflict',
            message: 'The definition moved on.',
        });
    });

    it('refuses a write that states no intent', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', `/api/reporting/reports/${DAILY}`, {
            version: 3,
            change: {
                description: '',
                schedule_expression: '0 0 9 * * ?',
                is_official: true,
                pre_processing: 'none',
                prepared_input_key: 'acme/daily',
                post_processing: 'none',
            },
        });
        expect(response.statusCode).toBe(400);
    });
});
