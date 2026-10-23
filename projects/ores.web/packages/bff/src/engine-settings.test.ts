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

const HK_PARTY = {
    id: '0a12c57f-1111-4222-8333-444444444444',
    name: 'ACME Corporation HK',
    partyCategory: 'Operational',
    businessCenterCode: 'HKHK',
};

type Replies = Readonly<Record<string, unknown>>;

function buildTestServer(
    replies: Replies,
    actingParty?: { readonly id: string; readonly name: string },
): {
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
    const party = actingParty && {
        id: actingParty.id,
        name: actingParty.name,
        partyCategory: 'Operational',
        businessCenterCode: 'GBLO',
    };
    const session = sessions.create({
        client,
        session: party
            ? {
                  kind: 'active',
                  token: 'token',
                  accountId: '11111111-1111-1111-1111-111111111111',
                  tenantId: '44444444-4444-4444-4444-444444444444',
                  tenantName: 'Acme',
                  tenantBootstrapping: false,
                  version: 'v0.0.25 (test)',
                  database: { fingerprint: '', environment: '', commit: '', created: '' },
                  username: 'analyst',
                  email: 'analyst@acme.example',
                  party,
                  availableParties: [party, HK_PARTY],
                  accessLifetimeSeconds: 1800,
                  passwordResetRequired: false,
                  sessionId: '33333333-3333-3333-3333-333333333333',
              }
            : null,
        username: 'analyst',
        email: 'analyst@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: party ? [party, HK_PARTY] : [],
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

const SETTINGS_ID = '55555555-5555-4555-8555-555555555555';
const HEADER_ID = '99999999-9999-4999-8999-999999999999';
const PART_ID = '66666666-6666-4666-8666-666666666666';
const REPORT_ID = '77777777-7777-4777-8777-777777777777';
const PARTY_ID = '0a12c57f-1111-4222-8333-444444444444';
const INTENT = { reason_code: 'common.correction', commentary: 'Use four threads' };

const central = {
    id: SETTINGS_ID,
    party_id: PARTY_ID,
    version: 4,
    name: 'HK standard',
    description: 'What the Hong Kong reports share',
    configuration_id: HEADER_ID,
    modified_by: 'analyst',
    recorded_at: '2026-10-20 09:00:00.000000Z',
    change_reason_code: 'common.correction',
    change_commentary: '',
};
const execution = {
    id: PART_ID,
    version: 2,
    engine_settings_id: SETTINGS_ID,
    n_threads: 4,
    log_mask: null,
    gzip_compression_level: null,
    modified_by: 'analyst',
    recorded_at: '2026-10-20 09:00:00.000000Z',
    change_reason_code: 'common.correction',
    change_commentary: '',
};

const reads = {
    'iam.v1.ops.get_my_parties': {
        result: ok,
        default_party_id: PARTY_ID,
        parties: [
            {
                party_id: PARTY_ID,
                name: 'ACME Corporation HK',
                short_code: 'ACMEHK',
                party_category: 'Operational',
                business_center_code: 'HKHK',
            },
        ],
    },
    'reporting.v1.engine_settings.list': { result: ok, settings: [central] },
    'reporting.v1.engine_settings_fixings.list': { result: ok, engine_settings_fixings: [] },
    'reporting.v1.engine_settings_building.list': { result: ok, engine_settings_building: [] },
    'reporting.v1.engine_settings_execution.list': {
        result: ok,
        engine_settings_execution: [execution],
    },
    'reporting.v1.report_configurations.list': {
        result: ok,
        report_configurations: [
            {
                id: '88888888-8888-4888-8888-888888888888',
                report_definition_id: REPORT_ID,
                configuration_type_code: 'engine_settings',
                configuration_id: HEADER_ID,
            },
        ],
    },
    'reporting.v1.report_definitions.list': {
        result: ok,
        definitions: [{ id: REPORT_ID, name: 'HK daily VaR' }],
    },
};

describe('engine settings routes', () => {
    it('lists each configuration with how many groups it sets and its threads', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(server, sessionId, 'GET', '/api/reporting/engine-settings');
        expect(response.statusCode).toBe(200);
        expect(response.json().rows).toEqual([
            expect.objectContaining({
                id: SETTINGS_ID,
                name: 'HK standard',
                partyName: 'ACME Corporation HK',
                groupsSet: 1,
                groups: 3,
                threads: 4,
            }),
        ]);
    });

    it('reads one configuration with its groups, the unset ones as null, and the reports it reaches', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/engine-settings/${SETTINGS_ID}`,
        );
        expect(response.statusCode).toBe(200);
        const document = response.json();
        expect(document.groups.fixings).toBeNull();
        expect(document.groups.building).toBeNull();
        expect(document.groups.execution).toMatchObject({
            id: PART_ID,
            version: 2,
            values: { n_threads: 4, log_mask: null, gzip_compression_level: null },
        });
        expect(document.reports).toEqual([{ id: REPORT_ID, name: 'HK daily VaR' }]);
    });

    it('answers a 404 for a configuration that does not exist', async () => {
        const { server, sessionId } = buildTestServer({
            ...reads,
            'reporting.v1.engine_settings.list': { result: ok, settings: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/reporting/engine-settings/${SETTINGS_ID}`,
        );
        expect(response.statusCode).toBe(404);
    });

    it('takes the wording of each setting from its ore parameter and marks one with none', async () => {
        const { server, sessionId } = buildTestServer({
            'reporting.v1.ore_parameters.list': {
                result: ok,
                parameters: [
                    {
                        schema_name: 'ore.xsd',
                        scope: 'Setup',
                        name: 'nThreads',
                        xpath: "/ORE/Setup/Parameter[@name='nThreads']",
                        label: 'Threads',
                        help: 'A whole number of at least 1.',
                        description: 'The number of threads the engine may use.',
                        default_text: '1.',
                        format: 'A whole number, at least 1.',
                        introduced_in: '1.8.9.0',
                        removed_in: '',
                    },
                    {
                        schema_name: 'ore.xsd',
                        scope: 'analytic',
                        name: 'implyTodaysFixings',
                        label: 'Not a Setup parameter',
                    },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/reporting/engine-settings/documentation',
        );
        expect(response.statusCode).toBe(200);
        const groups = response.json().groups as {
            id: string;
            changesNumbers: boolean;
            settings: { ore: string; documented: boolean; label: string }[];
        }[];
        expect(groups.map((group) => group.id)).toEqual(['fixings', 'building', 'execution']);
        const threads = groups[2]?.settings.find((setting) => setting.ore === 'nThreads');
        expect(threads).toMatchObject({
            documented: true,
            label: 'Threads',
            introducedIn: '1.8.9.0',
        });
        const imply = groups[0]?.settings.find((setting) => setting.ore === 'implyTodaysFixings');
        expect(imply).toMatchObject({ documented: false, label: '' });
        expect(groups.map((group) => group.changesNumbers)).toEqual([true, true, false]);
        const observation = groups[1]?.settings.find(
            (setting) => setting.ore === 'observationModel',
        );
        expect(observation).toMatchObject({
            changesNumbers: false,
            options: ['None', 'Disable', 'Defer', 'Unregister'],
        });
        expect(threads).toMatchObject({ changesNumbers: false, min: 1, max: null });
    });

    it('writes a group against the version the person read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...reads,
            'reporting.v1.engine_settings_execution.put': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/engine-settings/${SETTINGS_ID}/groups/execution`,
            {
                version: 2,
                values: { n_threads: 8, log_mask: null, gzip_compression_level: 6 },
                intent: INTENT,
            },
        );
        expect(response.statusCode).toBe(200);
        const put = calls.find((call) => call.subject.endsWith('execution.put'));
        expect(put?.body).toMatchObject({
            change: {
                write: {
                    id: PART_ID,
                    engine_settings_id: SETTINGS_ID,
                    n_threads: 8,
                    log_mask: null,
                    gzip_compression_level: 6,
                },
                precondition: { kind: 'must_match_version', version: 2 },
            },
            intent: INTENT,
        });
    });

    it('creates the row of a group that had none', async () => {
        const { server, sessionId, calls } = buildTestServer({
            ...reads,
            'reporting.v1.engine_settings_fixings.put': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/engine-settings/${SETTINGS_ID}/groups/fixings`,
            {
                version: null,
                values: {
                    imply_todays_fixings: true,
                    include_todays_cash_flows: null,
                    include_reference_date_events: null,
                    enrich_index_fixings: null,
                    ignore_fixing_lag: null,
                    ignore_fixing_lead: null,
                },
                intent: INTENT,
            },
        );
        expect(response.statusCode).toBe(200);
        const put = calls.find((call) => call.subject.endsWith('fixings.put'));
        expect(put?.body).toMatchObject({
            change: { precondition: { kind: 'must_not_exist', version: null } },
        });
    });

    it.each([
        ['a thread count below one', 'execution', { n_threads: 0 }],
        ['a compression level above nine', 'execution', { gzip_compression_level: 10 }],
        ['an observation model ORE does not know', 'building', { observation_model: 'Bogus' }],
        ['a setting the group does not hold', 'execution', { imply_todays_fixings: true }],
    ])('refuses %s and writes nothing', async (_what, group, values) => {
        const { server, sessionId, calls } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/engine-settings/${SETTINGS_ID}/groups/${group}`,
            { version: null, values, intent: INTENT },
        );
        expect(response.statusCode).toBe(400);
        expect(calls.some((call) => call.subject.endsWith('.put'))).toBe(false);
    });

    it('refuses a write without a reason', async () => {
        const { server, sessionId } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/engine-settings/${SETTINGS_ID}/groups/execution`,
            { version: 2, values: {} },
        );
        expect(response.statusCode).toBe(400);
    });

    it('tells a stale version as a conflict', async () => {
        const { server, sessionId } = buildTestServer({
            ...reads,
            'reporting.v1.engine_settings_execution.put': {
                result: { outcome: 'conflict', code: 'version', message: 'The version moved.' },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/reporting/engine-settings/${SETTINGS_ID}/groups/execution`,
            {
                version: 1,
                values: { n_threads: 1, log_mask: null, gzip_compression_level: null },
                intent: INTENT,
            },
        );
        expect(response.statusCode).toBe(409);
    });

    it('writes the header of a new configuration first and removes it when the row fails', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'reporting.v1.configurations.put': { result: ok },
            'reporting.v1.configurations.delete': { result: ok },
            'reporting.v1.engine_settings.put': {
                result: { outcome: 'invalid', code: 'name', message: 'The name is taken.' },
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/reporting/engine-settings', {
            name: 'HK standard',
            description: '',
            partyId: PARTY_ID,
            intent: INTENT,
        });
        expect(response.statusCode).toBe(400);
        expect(calls.map((call) => call.subject)).toEqual([
            'reporting.v1.configurations.put',
            'reporting.v1.engine_settings.put',
            'reporting.v1.configurations.delete',
        ]);
    });

    it('asks for a party when the session has none and the body names none', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/reporting/engine-settings', {
            name: 'HK standard',
            description: '',
            intent: INTENT,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('refuses to delete engine settings that a report binds', async () => {
        const { server, sessionId, calls } = buildTestServer(reads);
        const response = await send(
            server,
            sessionId,
            'DELETE',
            `/api/reporting/engine-settings/${SETTINGS_ID}`,
            { intent: INTENT },
        );
        expect(response.statusCode).toBe(409);
        expect(calls.some((call) => call.subject.endsWith('.delete'))).toBe(false);
    });

    describe('from a party that does not own the configuration', () => {
        const GROUP_PARTY = { id: '0b0b0b0b-1111-4222-8333-555555555555', name: 'Acme Group' };

        it.each([
            [
                'reads its history',
                'GET',
                `/api/reporting/engine-settings/${SETTINGS_ID}/history`,
                undefined,
            ],
            [
                'saves a group',
                'PUT',
                `/api/reporting/engine-settings/${SETTINGS_ID}/groups/execution`,
                {
                    version: 2,
                    values: { n_threads: 2, log_mask: null, gzip_compression_level: null },
                    intent: INTENT,
                },
            ],
            [
                'renames it',
                'PUT',
                `/api/reporting/engine-settings/${SETTINGS_ID}`,
                { name: 'Renamed', description: '', version: 4, intent: INTENT },
            ],
            [
                'deletes it',
                'DELETE',
                `/api/reporting/engine-settings/${SETTINGS_ID}`,
                { intent: INTENT },
            ],
        ] as const)(
            'refuses a person who %s and names the owner',
            async (_name, method, url, body) => {
                const { server, sessionId, calls } = buildTestServer(reads, GROUP_PARTY);
                const response = await send(server, sessionId, method, url, body);
                expect(response.statusCode).toBe(409);
                expect(response.json()).toEqual({
                    code: 'other-party',
                    message:
                        'This record belongs to ACME Corporation HK. Switch to that party to change it.',
                });
                expect(
                    calls.filter(
                        (call) => call.subject.endsWith('.put') || call.subject.endsWith('.delete'),
                    ),
                ).toEqual([]);
            },
        );

        it('lets the owning party write as before', async () => {
            const { server, sessionId } = buildTestServer(
                { ...reads, 'reporting.v1.engine_settings_execution.put': { result: ok } },
                { id: HK_PARTY.id, name: HK_PARTY.name },
            );
            const response = await send(
                server,
                sessionId,
                'PUT',
                `/api/reporting/engine-settings/${SETTINGS_ID}/groups/execution`,
                {
                    version: 2,
                    values: { n_threads: 8, log_mask: null, gzip_compression_level: 6 },
                    intent: INTENT,
                },
            );
            expect(response.statusCode).toBe(200);
        });
    });
});
