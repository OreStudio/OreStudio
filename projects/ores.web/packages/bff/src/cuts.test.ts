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
import { cutKindChoicesSchema } from '@ores/wire-protocol/browser';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The cut routes: the kinds of cut Reference data draws, and the cuts Reporting
 * draws.
 *
 * What is checked is what the routes promise: the signed-or-not a type shows is
 * derived from the approval kind it points at, the counts of a type's cuts come
 * from the cut read, the cuts list passes the person's narrowing to the
 * operation that derives the state and the label, a record answers the gaps and
 * the run tally, and a save keeps the field the form does not offer.
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
        siteConfig: undefined,
        environmentId: ENVIRONMENT_ID,
    });
}

const ok = { outcome: 'ok', code: '', message: '', fields: [] };

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
        version: 'v0.0.29 (test)',
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

const KINDS = {
    result: ok,
    kinds: [
        {
            version: 3,
            code: 'EOD',
            name: 'End of day',
            description: 'The day’s official revaluation of a book.',
            family_code: 'end_of_day',
            approval_kind_code: 'reporting.eod_cut_authorisation',
            output_is_indicative: false,
            resolution_rule_code: 'same_day_close',
            retention_months: 25,
            retention_destination_code: 'cold',
            legal_hold_default: false,
            display_order: 10,
        },
        {
            version: 1,
            code: 'FLASH',
            name: 'Intraday flash',
            description: 'An indicative view of value while the day is still moving.',
            family_code: 'intraday',
            approval_kind_code: null,
            output_is_indicative: true,
            resolution_rule_code: 'the_cut_named',
            retention_months: 3,
            retention_destination_code: 'cold',
            legal_hold_default: false,
            display_order: 40,
        },
    ],
    total: 2,
};

const FAMILIES = {
    result: ok,
    families: [
        { code: 'end_of_day', name: 'End of day' },
        { code: 'intraday', name: 'Intraday' },
    ],
    total: 2,
};

const RULES = {
    result: ok,
    rules: [{ code: 'same_day_close', name: 'The close of the day the report covers' }],
    total: 1,
};

const APPROVAL_KINDS = {
    result: ok,
    kinds: [
        {
            version: 1,
            code: 'reporting.eod_cut_authorisation',
            name: 'End of day cut authorisation',
            description: 'Signs the day’s official cut.',
            decide_permission_code: 'inbox::approvals:decide_finance',
            approvals_required: 1,
        },
    ],
    total: 1,
};

const CUT_ROWS = 'reporting.v1.ops.list_cut_rows';
const CUT_RECORD = 'reporting.v1.ops.get_cut_record';

function cutRow(over: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        cut_id: '11111111-1111-4111-8111-111111111111',
        label: 'EOD.ACME-LONDON.2026-10-06',
        state: 'authorised',
        kind_code: 'EOD',
        kind_name: 'End of day',
        timepoint: '2026-10-06 16:00:00.000000Z',
        timepoint_timezone: 'Europe/London',
        scope_code: 'ACME-LONDON',
        business_centre_code: 'LONDON',
        taken_by: 'a.reyes',
        taken_at: '2026-10-06 16:02:00.000000Z',
        approved_by: 'm.silva',
        approved_at: '2026-10-06 16:20:00.000000Z',
        refused_at: '',
        requires_authorisation: true,
        output_is_indicative: false,
        take_ordinal: 1,
        run_count: 4,
        superseded_by_id: '',
        ...over,
    };
}

const THE_KIND = {
    version: 3,
    code: 'EOD',
    name: 'End of day',
    description: 'The day’s official revaluation of a book.',
    family_code: 'end_of_day',
    approval_kind_code: 'reporting.eod_cut_authorisation',
    output_is_indicative: false,
    resolution_rule_code: 'same_day_close',
    retention_months: 25,
    retention_destination_code: 'cold',
    legal_hold_default: false,
    display_order: 10,
};

const THE_CUT = {
    version: 2,
    id: '11111111-1111-4111-8111-111111111111',
    party_id: '77777777-7777-4777-8777-777777777777',
    kind_code: 'EOD',
    scope_code: 'ACME-LONDON',
    business_centre_code: 'LONDON',
    timepoint: '2026-10-06 16:00:00.000000Z',
    take_ordinal: 1,
    timepoint_timezone: 'Europe/London',
    cut_time_code: 'LONDON-CLOSE',
    market_data_snapshot_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb1',
    deal_population_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb2',
    reference_data_version: '2026.10.06',
    engine_version: 'ORE 1.8.15',
    code_version: 'a1b2c3d',
    taken_by: 'a.reyes',
    taken_at: '2026-10-06 16:02:00.000000Z',
    requires_authorisation: true,
    approval_request_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb3',
    approved_by: 'm.silva',
    approved_at: '2026-10-06 16:20:00.000000Z',
    refused_at: null,
    refusal_comment: '',
    retention_months: 25,
    retention_destination_code: 'cold',
    legal_hold: false,
    output_is_indicative: false,
    modified_by: 'a.reyes',
    performed_by: 'ores_reporting_service',
    change_reason_code: 'common.creation',
    change_commentary: '',
    recorded_at: '2026-10-06 16:02:00.000000Z',
};

const CENTRES = [
    { code: 'GBLO', name: 'London', version: 1 },
    { code: 'USNY', name: 'New York', version: 1 },
];

const BASIS = {
    'reporting.v1.cut_kinds.list': KINDS,
    'refdata.v1.business_centres.list': { result: ok, centres: CENTRES, total: 2 },
    'reporting.v1.cut_families.list': FAMILIES,
    'reporting.v1.cut_resolution_rules.list': RULES,
    'inbox.v1.approval_kinds.list': APPROVAL_KINDS,
    [CUT_ROWS]: { result: ok, rows: [cutRow()], total: 1 },
    [CUT_RECORD]: {
        result: ok,
        cut: THE_CUT,
        kind: THE_KIND,
        label: 'EOD.ACME-LONDON.2026-10-06',
        state: 'authorised',
        superseded_by_id: '',
        gaps: [],
        run_count: 4,
        detail: {
            id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb4',
            request_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb3',
            cut_id: '11111111-1111-4111-8111-111111111111',
            party_id: '77777777-7777-4777-8777-777777777777',
            deal_count: 1284,
            market_data_set_count: 7,
            gap_count: 0,
            applied_by: '',
            applied_at: null,
        },
    },
};

describe('GET /api/reporting/cut-kinds', () => {
    it('derives the signed-or-not from the approval kind and counts the cuts of each type', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=0&limit=15',
        );
        expect(answer.statusCode).toBe(200);
        const page = answer.json() as {
            rows: readonly Record<string, unknown>[];
            total: number;
            partial: boolean;
        };
        expect(page.total).toBe(2);
        expect(page.partial).toBe(false);
        const eod = page.rows.find((row) => row['code'] === 'EOD');
        const flash = page.rows.find((row) => row['code'] === 'FLASH');
        // The end of day type points at an approval kind, so a person signs it
        // and the words are that target's.
        expect(eod).toMatchObject({
            signed: true,
            approvalKindName: 'End of day cut authorisation',
            decidePermissionCode: 'inbox::approvals:decide_finance',
            familyName: 'End of day',
            resolutionRuleName: 'The close of the day the report covers',
            cutsCount: 1,
        });
        // The intraday type points at nothing, so nobody signs it.
        expect(flash).toMatchObject({ signed: false, approvalKindCode: null, cutsCount: 0 });
    });

    it('reads the cuts once for the page, not once for each type', async () => {
        const { server, sessionId, calls } = buildTestServer(BASIS);
        await send(server, sessionId, 'GET', '/api/reporting/cut-kinds?offset=0&limit=15');
        expect(calls.filter((call) => call.subject === CUT_ROWS)).toHaveLength(1);
    });

    it('draws a type when the account may not read the inbox, losing only the decider', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            // A refusal is the server's own answer: the inbox read comes back
            // denied, which the route reads as "no decider's name", not as an
            // outage.
            'inbox.v1.approval_kinds.list': {
                result: {
                    outcome: 'denied',
                    code: 'forbidden',
                    message: 'This account may not read the approval kinds.',
                    fields: [],
                },
                kinds: [],
                total: 0,
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=0&limit=15',
        );
        expect(answer.statusCode).toBe(200);
    });

    it('answers one page of the kinds, with the server’s total beside it', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=1&limit=1',
        );
        expect(answer.statusCode).toBe(200);
        const page = answer.json() as { rows: readonly Record<string, unknown>[]; total: number };
        // The read answers the row the person's page holds and the count of
        // every row the query matched, so the pager can state both.
        expect(page.total).toBe(2);
        expect(page.rows).toHaveLength(1);
        expect(page.rows[0]?.['code']).toBe('FLASH');
    });

    it('searches and orders the kinds on the server', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const searched = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=0&limit=15&search=flash',
        );
        const page = searched.json() as { rows: readonly Record<string, unknown>[] };
        expect(page.rows.map((row) => row['code'])).toEqual(['FLASH']);

        const ordered = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=0&limit=15&sort=code&descending=true',
        );
        expect(
            (ordered.json() as { rows: readonly Record<string, unknown>[] }).rows.map(
                (row) => row['code'],
            ),
        ).toEqual(['FLASH', 'EOD']);

        const refused = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cut-kinds?offset=0&limit=15&sort=retention',
        );
        expect(refused.statusCode).toBe(400);
    });

    it('refuses a read that states no page', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cut-kinds');
        expect(answer.statusCode).toBe(400);
    });
});

describe('GET /api/reporting/cut-kind-choices', () => {
    it('answers every lookup a type names, including the destinations the store holds', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cut-kind-choices');
        expect(answer.statusCode).toBe(200);
        // The client parses this with the wire schema the type form reads, so a
        // missing member would empty the form's selects.
        const choices = cutKindChoicesSchema.parse(answer.json());
        expect(choices.families).toEqual([
            { code: 'end_of_day', name: 'End of day' },
            { code: 'intraday', name: 'Intraday' },
        ]);
        expect(choices.resolutionRules).toEqual([
            { code: 'same_day_close', name: 'The close of the day the report covers' },
        ]);
        expect(choices.approvalKinds).toEqual([
            { code: 'reporting.eod_cut_authorisation', name: 'End of day cut authorisation' },
        ]);
        // The kinds both keep their cuts cold, so the store knows one tier.
        expect(choices.retentionDestinations).toEqual(['cold']);
        // The take fixes the scope's books against one centre, so the choices
        // name the centres a person may state.
        expect(choices.businessCentres).toEqual([
            { code: 'GBLO', name: 'London' },
            { code: 'USNY', name: 'New York' },
        ]);
    });

    it('names every tier the kinds hold, once each and in order', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            'reporting.v1.cut_kinds.list': {
                result: ok,
                kinds: [
                    { ...THE_KIND, code: 'EOD', retention_destination_code: 'warm' },
                    { ...THE_KIND, code: 'EOM', retention_destination_code: 'cold' },
                    { ...THE_KIND, code: 'EOY', retention_destination_code: 'cold' },
                ],
                total: 3,
            },
        });
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cut-kind-choices');
        expect(cutKindChoicesSchema.parse(answer.json()).retentionDestinations).toEqual([
            'cold',
            'warm',
        ]);
    });
});

describe('GET /api/reporting/cut-kinds/:code', () => {
    it('answers the type with the cuts the party holds of it', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cut-kinds/EOD');
        expect(answer.statusCode).toBe(200);
        const record = answer.json() as {
            kind: Record<string, unknown>;
            cuts: readonly Record<string, unknown>[];
        };
        expect(record.kind['code']).toBe('EOD');
        expect(record.cuts).toHaveLength(1);
        expect(record.cuts[0]?.['label']).toBe('EOD.ACME-LONDON.2026-10-06');
    });

    it('refuses a type the store does not hold', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cut-kinds/EOY');
        expect(answer.statusCode).toBe(404);
    });
});

describe('PUT /api/reporting/cut-kinds/:code', () => {
    it('keeps the retention destination the form does not offer', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...BASIS,
            'reporting.v1.cut_kinds.get': { result: ok, cut_kind: THE_KIND },
            'reporting.v1.cut_kinds.put': { result: ok, cut_kind: THE_KIND },
        });
        const answer = await send(server, sessionId, 'PUT', '/api/reporting/cut-kinds/EOD', {
            version: 3,
            change: {
                name: 'End of day',
                description: 'The day’s official revaluation of a book.',
                family_code: 'end_of_day',
                approval_kind_code: 'reporting.eod_cut_authorisation',
                output_is_indicative: false,
                resolution_rule_code: 'same_day_close',
                retention_months: 26,
                legal_hold_default: true,
                display_order: 10,
            },
            intent: { reason_code: 'common.non_material_update', commentary: '' },
        });
        expect(answer.statusCode).toBe(200);
        const write = calls.find((call) => call.subject === 'reporting.v1.cut_kinds.put');
        expect(write?.body).toMatchObject({
            change: {
                write: { code: 'EOD', retention_months: 26, retention_destination_code: 'cold' },
                precondition: { kind: 'must_match_version', version: 3 },
            },
        });
    });

    it('refuses a save that names a field the form does not offer', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'PUT', '/api/reporting/cut-kinds/EOD', {
            version: 3,
            change: { name: 'End of day' },
            intent: { reason_code: 'common.non_material_update', commentary: '' },
        });
        expect(answer.statusCode).toBe(400);
    });
});

describe('DELETE /api/reporting/cut-kinds/:code', () => {
    it('names the version it read, so a type that moved on is not removed silently', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...BASIS,
            'reporting.v1.cut_kinds.delete': { result: ok },
        });
        const answer = await send(server, sessionId, 'DELETE', '/api/reporting/cut-kinds/FLASH', {
            version: 1,
            intent: { reason_code: 'common.retired', commentary: 'Superseded by the close.' },
        });
        expect(answer.statusCode).toBe(204);
        expect(
            calls.find((call) => call.subject === 'reporting.v1.cut_kinds.delete')?.body,
        ).toMatchObject({
            removal: {
                key: { code: 'FLASH' },
                precondition: { kind: 'must_match_version', version: 1 },
            },
        });
    });
});

describe('GET /api/reporting/cuts', () => {
    it('passes the kind, the month, the state and the search to the read that derives them', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...BASIS,
            [CUT_ROWS]: { result: ok, rows: [cutRow()], total: 1 },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts?offset=30&limit=15&kind_code=EOD&month=2026-10&state=authorised&search=06',
        );
        expect(answer.statusCode).toBe(200);
        expect(calls.find((call) => call.subject === CUT_ROWS)?.body).toMatchObject({
            kind_code: 'EOD',
            month: '2026-10',
            state: 'authorised',
            search: '06',
            offset: 30,
            limit: 15,
        });
        const page = answer.json() as {
            rows: readonly Record<string, unknown>[];
            partial: boolean;
        };
        expect(page.rows[0]).toMatchObject({
            label: 'EOD.ACME-LONDON.2026-10-06',
            state: 'authorised',
            runCount: 4,
            approvedBy: 'm.silva',
            supersededById: null,
        });
        expect(page.partial).toBe(false);
    });

    it('says when the rows are one page of a scope that is larger', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [CUT_ROWS]: {
                result: ok,
                rows: [cutRow(), cutRow({ label: 'EOD.ACME-LONDON.2026-10-05' })],
                total: 40,
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts?offset=0&limit=15',
        );
        expect((answer.json() as { partial: boolean }).partial).toBe(true);
    });

    it('refuses a read that states no page, and an order the read does not keep', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        expect((await send(server, sessionId, 'GET', '/api/reporting/cuts')).statusCode).toBe(400);
        expect(
            (
                await send(
                    server,
                    sessionId,
                    'GET',
                    '/api/reporting/cuts?offset=0&limit=15&sort=label',
                )
            ).statusCode,
        ).toBe(400);
    });

    it('refuses a state the design does not word', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts?offset=0&limit=15&state=happy',
        );
        expect(answer.statusCode).toBe(400);
    });

    it('turns the read’s refusal of a malformed month into a bad request', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [CUT_ROWS]: {
                result: {
                    outcome: 'invalid',
                    code: 'filter_range_invalid',
                    message: "A month is stated as YYYY-MM, and this one is '2026-13'.",
                    fields: [],
                },
                rows: [],
                total: 0,
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts?offset=0&limit=15&month=2026-13',
        );
        expect(answer.statusCode).toBe(400);
        expect((answer.json() as { message: string }).message).toContain('YYYY-MM');
    });
});

describe('GET /api/reporting/cuts/:id/runs', () => {
    const RUNS = {
        result: ok,
        instances: [
            {
                id: '99999999-9999-4999-8999-999999999991',
                name: 'Board Risk Dashboard',
                cut_id: '11111111-1111-4111-8111-111111111111',
                started_at: '2026-10-06 17:22:00.000000Z',
                completed_at: '2026-10-06 17:31:00.000000Z',
                priced_at: '2026-10-06 17:30:00.000000Z',
            },
            {
                id: '99999999-9999-4999-8999-999999999992',
                name: 'NPV',
                cut_id: '11111111-1111-4111-8111-111111111111',
                started_at: '2026-10-06 18:05:00.000000Z',
                completed_at: '2026-10-06 18:09:00.000000Z',
                priced_at: '2026-10-06 18:08:00.000000Z',
            },
        ],
        total: 2,
    };

    const HEALTH = {
        result: ok,
        health: [
            {
                report_instance_id: '99999999-9999-4999-8999-999999999991',
                state_name: 'completed',
                health: 'completed clean',
            },
            {
                report_instance_id: '99999999-9999-4999-8999-999999999992',
                state_name: 'completed',
                health: 'completed with errors',
            },
        ],
    };

    it('answers the runs that priced from the cut, newest start first, with their state', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...BASIS,
            'reporting.v1.report_instances.list': RUNS,
            'reporting.v1.ops.get_report_instance_health': HEALTH,
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111/runs',
        );
        expect(answer.statusCode).toBe(200);
        const body = answer.json() as {
            runs: readonly {
                id: string;
                name: string;
                stateName: string;
                health: string | null;
                startedAt: string | null;
                pricedAt: string | null;
            }[];
        };
        expect(body.runs.map((run) => run.name)).toEqual(['NPV', 'Board Risk Dashboard']);
        expect(body.runs[0]?.health).toBe('completed with errors');
        expect(body.runs[0]?.stateName).toBe('completed');
        expect(body.runs[1]?.startedAt).toBe('2026-10-06 17:22:00.000000Z');

        // The filter is the read's own: a run names the cut it resolved to, so
        // the tab reads the runs by that column and nothing else.
        const listCall = calls.find(
            (call) => call.subject === 'reporting.v1.report_instances.list',
        );
        expect(listCall?.body).toMatchObject({
            filter: { cut_id_one_of: ['11111111-1111-4111-8111-111111111111'] },
        });
        // The states are one read for the whole page, not one for each run.
        expect(
            calls.filter((call) => call.subject === 'reporting.v1.ops.get_report_instance_health'),
        ).toHaveLength(1);
    });

    it('leaves the state empty when the health read fails, and does not fail the tab', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            'reporting.v1.report_instances.list': RUNS,
            'reporting.v1.ops.get_report_instance_health': {
                result: { outcome: 'failed', code: 'internal_error', message: 'No.', fields: [] },
                health: [],
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111/runs',
        );
        expect(answer.statusCode).toBe(200);
        const body = answer.json() as { runs: readonly { health: string | null }[] };
        expect(body.runs[0]?.health).toBeNull();
    });

    it('refuses an address that is not a cut id', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cuts/not-an-id/runs');
        expect(answer.statusCode).toBe(400);
    });
});

const TAKE_CUT = 'reporting.v1.ops.take_cut';

/** What the take answers for a body the store could assemble whole. */
const A_TAKEN_BODY = {
    result: ok,
    cut_id: '11111111-1111-4111-8111-111111111111',
    cut_label: 'EOD.ACME-LONDON.2026-10-06',
    cut_state: 'taken',
    requires_authorisation: true,
    output_is_indicative: false,
    retention_months: 25,
    retention_destination_code: 'cold',
    approval_request_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb3',
    approval_kind_code: 'reporting.eod_cut_authorisation',
    approval_kind_name: 'End of day cut authorisation',
    decide_permission_code: 'inbox::approvals:decide_finance',
    parts: [
        {
            part_code: 'kind',
            state: 'closed',
            summary: 'EOD · End of day cut · signed by reporting.eod_cut_authorisation',
            reference: '',
            item: '',
            reason: '',
            remedy: '',
            snapshot_note: 'kept 25 months in the cold tier',
        },
        {
            part_code: 'deal_population',
            state: 'closed',
            summary: '1284 deals held at the instant',
            reference: '',
            item: '',
            reason: '',
            remedy: '',
            snapshot_note: 'the store holds no copy of the population',
        },
    ],
    gaps: [],
    deal_count: 1284,
    market_data_set_count: 7,
    reference_data_version: '4 report configuration(s) in force',
    engine_version: '1.8.17.0',
    code_version: '',
};

/** What a person states when they take a cut, as the screen sends it. */
const A_TAKE = {
    kindCode: 'EOD',
    scopeCode: 'ACME-LONDON',
    businessCentreCode: 'GBLO',
    timepoint: '2026-10-06T16:00:00.000000Z',
    timepointTimezone: 'Europe/London',
    cutTimeCode: '',
    reason: 'The day is closed and the reports wait on the body',
    preview: false,
};

describe('POST /api/reporting/cuts/take', () => {
    it('answers the body part by part and the press it wrote', async () => {
        const { server, sessionId } = buildTestServer({ ...BASIS, [TAKE_CUT]: A_TAKEN_BODY });
        const answer = await send(server, sessionId, 'POST', '/api/reporting/cuts/take', A_TAKE);
        expect(answer.statusCode).toBe(200);
        expect(answer.json()).toMatchObject({
            cutId: '11111111-1111-4111-8111-111111111111',
            cutLabel: 'EOD.ACME-LONDON.2026-10-06',
            cutState: 'taken',
            requiresAuthorisation: true,
            retentionMonths: 25,
            retentionDestinationCode: 'cold',
            approvalRequestId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb3',
            approvalKindName: 'End of day cut authorisation',
            decidePermissionCode: 'inbox::approvals:decide_finance',
            dealCount: 1284,
            marketDataSetCount: 7,
        });
        const body = answer.json() as {
            parts: readonly { partCode: string; state: string; snapshotNote: string }[];
        };
        expect(body.parts[0]).toMatchObject({
            partCode: 'kind',
            state: 'closed',
            snapshotNote: 'kept 25 months in the cold tier',
        });
    });

    it('answers the gaps of a body it could not assemble, and the draft it kept', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [TAKE_CUT]: {
                ...A_TAKEN_BODY,
                cut_state: 'incomplete',
                approval_request_id: '',
                parts: [
                    {
                        part_code: 'deal_population',
                        state: 'gapped',
                        summary: '',
                        reference: '',
                        item: 'the deal population',
                        reason: 'The store holds no deal at the instant.',
                        remedy: 'Correct the deal population, or state an instant at which one is held.',
                        snapshot_note: '',
                    },
                ],
                gaps: [
                    {
                        id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb5',
                        part_code: 'deal_population',
                        item: 'the deal population',
                        reason: 'The store holds no deal at the instant.',
                    },
                ],
                deal_count: 0,
            },
        });
        const answer = await send(server, sessionId, 'POST', '/api/reporting/cuts/take', A_TAKE);
        expect(answer.statusCode).toBe(200);
        expect(answer.json()).toMatchObject({
            cutState: 'incomplete',
            approvalRequestId: '',
            gaps: [{ partCode: 'deal_population', item: 'the deal population' }],
        });
    });

    it('refuses a body that names no kind', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'POST', '/api/reporting/cuts/take', {
            ...A_TAKE,
            kindCode: '',
        });
        expect(answer.statusCode).toBe(400);
    });

    it('answers the sentence that refused the take', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [TAKE_CUT]: {
                result: {
                    outcome: 'invalid',
                    code: 'as_of_invalid',
                    message: 'A body cannot be fixed at an instant that has not happened.',
                    fields: [],
                },
            },
        });
        const answer = await send(server, sessionId, 'POST', '/api/reporting/cuts/take', {
            ...A_TAKE,
            timepoint: '2030-10-06T16:00:00.000000Z',
        });
        expect(answer.statusCode).toBe(400);
        expect(answer.json()).toMatchObject({
            message: 'A body cannot be fixed at an instant that has not happened.',
        });
    });
});

describe('GET /api/reporting/cuts/:id', () => {
    it('answers the body, the acts, the gaps and the run tally', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111',
        );
        expect(answer.statusCode).toBe(200);
        const record = answer.json() as {
            label: string;
            state: string;
            runCount: number;
            gaps: readonly unknown[];
            cut: Record<string, unknown>;
            kind: Record<string, unknown>;
        };
        expect(record.label).toBe('EOD.ACME-LONDON.2026-10-06');
        expect(record.state).toBe('authorised');
        expect(record.runCount).toBe(4);
        expect(record.gaps).toEqual([]);
        expect(record.cut['dealPopulationId']).toBe('bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbb2');
        expect(record.cut['engineVersion']).toBe('ORE 1.8.15');
        expect(record.kind['resolutionRuleName']).toBe('The close of the day the report covers');
    });

    it('names the body that replaced a superseded cut', async () => {
        const later = '22222222-2222-4222-8222-222222222222';
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [CUT_RECORD]: (body: unknown) =>
                (body as { cut_id: string }).cut_id === later
                    ? {
                          result: ok,
                          cut: { ...THE_CUT, id: later },
                          kind: THE_KIND,
                          label: 'EOD.ACME-LONDON.2026-10-07',
                          state: 'authorised',
                          superseded_by_id: '',
                          gaps: [],
                          run_count: 0,
                      }
                    : {
                          result: ok,
                          cut: THE_CUT,
                          kind: THE_KIND,
                          label: 'EOD.ACME-LONDON.2026-10-06',
                          state: 'superseded',
                          superseded_by_id: later,
                          gaps: [],
                          run_count: 4,
                      },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111',
        );
        expect(answer.statusCode).toBe(200);
        expect(answer.json()).toMatchObject({
            state: 'superseded',
            supersededById: later,
            supersededByLabel: 'EOD.ACME-LONDON.2026-10-07',
        });
    });

    it('draws a cut whose kind the store no longer holds, and says the kind is missing', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            'reporting.v1.cut_kinds.list': { result: ok, kinds: [], total: 0 },
            [CUT_RECORD]: {
                result: ok,
                cut: THE_CUT,
                // A kind the store does not hold arrives empty, and the cut's own
                // copied facts are what the page draws.
                kind: {},
                label: '',
                state: 'authorised',
                superseded_by_id: '',
                gaps: [],
                run_count: 4,
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111',
        );
        expect(answer.statusCode).toBe(200);
        expect(answer.json()).toMatchObject({
            label: '',
            kind: {
                code: 'EOD',
                name: '',
                signed: true,
                retentionMonths: 25,
                retentionDestinationCode: 'cold',
            },
        });
    });

    it('answers a missing record as a missing record', async () => {
        const { server, sessionId } = buildTestServer({
            ...BASIS,
            [CUT_RECORD]: {
                result: {
                    outcome: 'missing',
                    code: 'not_found',
                    message: 'The cut does not exist.',
                    fields: [],
                },
                cut: null,
                kind: null,
                label: '',
                state: '',
                superseded_by_id: '',
                gaps: [],
                run_count: 0,
            },
        });
        const answer = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/cuts/11111111-1111-4111-8111-111111111111',
        );
        expect(answer.statusCode).toBe(404);
    });

    it('refuses an address that is not a cut id', async () => {
        const { server, sessionId } = buildTestServer(BASIS);
        const answer = await send(server, sessionId, 'GET', '/api/reporting/cuts/not-an-id');
        expect(answer.statusCode).toBe(400);
    });
});
