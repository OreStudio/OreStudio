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

import { describe, expect, it } from 'vitest';
import { OresClient } from './client.js';
import { z } from 'zod';
import { NotAuthenticatedError, OperationFailedError, SessionExpiredError } from './errors.js';
import { WireCodec } from './codec.js';
import type { MessageEnvelope, Reply, RequestHeaders, Transport } from './transport.js';

interface RecordedCall {
    readonly subject: string;
    readonly body: Uint8Array;
    readonly headers: RequestHeaders;
}

interface ScriptedReply {
    readonly body?: unknown;
    readonly headers?: Record<string, string>;
    readonly error?: unknown;
}

/** A transport that answers from a script and records what it received. */
class ScriptedTransport implements Transport {
    readonly calls: RecordedCall[] = [];
    readonly #script: Map<string, ScriptedReply[]>;
    readonly #codec = new WireCodec('msgpack');
    closed = false;

    constructor(script: Record<string, ScriptedReply[]>) {
        this.#script = new Map(Object.entries(script));
    }

    async request(
        subject: string,
        body: Uint8Array,
        headers: RequestHeaders,
        _timeoutMs: number,
    ): Promise<Reply> {
        this.calls.push({ subject, body, headers });

        const queue = this.#script.get(subject);
        const next = queue?.shift();
        if (next === undefined) {
            throw new Error(`no scripted reply left for ${subject}`);
        }
        if (next.error !== undefined) {
            throw next.error;
        }
        // Responses travel in the same encoding a real server would use, so the
        // client's own decode path runs in the test.
        const encoded = next.body === undefined ? new Uint8Array() : this.#codec.encode(next.body);
        return { subject, body: encoded, headers: next.headers ?? {} };
    }

    async close(): Promise<void> {
        this.closed = true;
    }

    /** Decodes a recorded request body for assertion. */
    decodeCall(index: number): unknown {
        const call = this.calls[index];
        if (call === undefined) {
            throw new Error(`no call at index ${index}`);
        }
        return this.#codec.decode(call.body);
    }
}

function loginReply(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        success: true,
        account_id: '11111111-1111-1111-1111-111111111111',
        tenant_id: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenant_name: 'System',
        username: 'probe',
        email: 'probe@ores.web.test',
        password_reset_required: false,
        tenant_bootstrap_mode: false,
        party_setup_required: false,
        party_setup_warning: '',
        token: 'token-one',
        error_message: '',
        message: '',
        selected_party_id: '22222222-2222-2222-2222-222222222222',
        available_parties: [
            {
                id: '22222222-2222-2222-2222-222222222222',
                name: 'System Party',
                party_category: 'System',
                business_center_code: 'GBLO',
            },
        ],
        default_party_id: '',
        access_lifetime_s: 1800,
        session_id: '33333333-3333-3333-3333-333333333333',
        ...overrides,
    };
}

describe('OresClient bootstrap status', () => {
    it('reports the deployment as unprovisioned before any credential is used', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.bootstrap_status': [{ body: { is_in_bootstrap_mode: true, message: '' } }],
        });
        const client = new OresClient({ transport });

        const status = await client.bootstrapStatus();

        expect(status.isInBootstrapMode).toBe(true);
        expect(transport.calls[0]?.headers).toEqual({});
    });

    it('reads a provisioned deployment as one a login may proceed against', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.bootstrap_status': [{ body: { is_in_bootstrap_mode: false, message: '' } }],
        });
        const client = new OresClient({ transport });

        expect((await client.bootstrapStatus()).isInBootstrapMode).toBe(false);
    });
});

describe('OresClient system onboarding', () => {
    it('records the finish under the op subject, with no request body', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.ops.complete_system_onboarding': [
                { body: { result: { outcome: 'ok', code: '', message: '' } } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.completeSystemOnboarding();

        expect(transport.calls[1]?.subject).toBe('variability.v1.ops.complete_system_onboarding');
        expect(transport.decodeCall(1)).toEqual({});
    });

    it('refuses a finish the server did not record', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.ops.complete_system_onboarding': [
                {
                    body: {
                        result: {
                            outcome: 'failed',
                            code: 'operation_failed',
                            message: 'no write',
                        },
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.completeSystemOnboarding()).rejects.toBeInstanceOf(
            OperationFailedError,
        );
    });

    it('reads the wizard flag by name through the settings read', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.system_settings.get': [
                {
                    body: {
                        result: { outcome: 'ok', code: '', message: '' },
                        system_setting: { value: 'true' },
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        expect(await client.onboardingSystemComplete()).toBe(true);
        expect(transport.decodeCall(1)).toEqual({ key: { name: 'onboarding.system' } });
    });

    it('reads a missing flag as unfinished rather than as an error', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.system_settings.get': [
                {
                    body: {
                        result: { outcome: 'missing', code: 'not_found', message: '' },
                        system_setting: null,
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        expect(await client.onboardingSystemComplete()).toBe(false);
    });

    it('reads the tenant wizard flag by name through the same settings read', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.system_settings.get': [
                {
                    body: {
                        result: { outcome: 'ok', code: '', message: '' },
                        system_setting: { value: 'true' },
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        expect(await client.onboardingTenantComplete()).toBe(true);
        expect(transport.decodeCall(1)).toEqual({ key: { name: 'onboarding.tenant' } });
    });

    it('reads a tenant that never ran its setup as unfinished rather than as an error', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'variability.v1.system_settings.get': [
                {
                    body: {
                        result: { outcome: 'missing', code: 'not_found', message: '' },
                        system_setting: null,
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        expect(await client.onboardingTenantComplete()).toBe(false);
    });
});

describe('OresClient login', () => {
    it('sends the credential in the principal field', async () => {
        const transport = new ScriptedTransport({ 'iam.v1.ops.login': [{ body: loginReply() }] });
        const client = new OresClient({ transport });

        await client.login({ principal: 'probe', password: 'secret' });

        expect(transport.decodeCall(0)).toEqual({ principal: 'probe', password: 'secret' });
    });

    it('sends no headers on the unauthenticated login call', async () => {
        const transport = new ScriptedTransport({ 'iam.v1.ops.login': [{ body: loginReply() }] });
        const client = new OresClient({ transport });

        await client.login({ principal: 'probe', password: 'secret' });

        expect(transport.calls[0]?.headers).toEqual({});
    });

    it('classifies a rejected login without throwing', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [
                {
                    body: loginReply({
                        success: false,
                        token: '',
                        error_message: 'Invalid username or password',
                    }),
                },
            ],
        });
        const client = new OresClient({ transport });

        const outcome = await client.login({ principal: 'probe', password: 'wrong' });

        expect(outcome).toMatchObject({
            kind: 'rejected',
            message: 'Invalid username or password',
        });
        expect(client.hasToken).toBe(false);
    });

    it('asks for a party when the server selected none', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply({ selected_party_id: '' }) }],
        });
        const client = new OresClient({ transport });

        const outcome = await client.login({ principal: 'probe', password: 'secret' });

        expect(outcome.kind).toBe('party-selection-required');
        expect(client.hasToken).toBe(true);
    });

    it('activates the session when the server already selected a party', async () => {
        const transport = new ScriptedTransport({ 'iam.v1.ops.login': [{ body: loginReply() }] });
        const client = new OresClient({ transport });

        const outcome = await client.login({ principal: 'probe', password: 'secret' });

        expect(outcome.kind).toBe('active');
        if (outcome.kind === 'active') {
            expect(outcome.party.name).toBe('System Party');
            expect(outcome.sessionId).toBe('33333333-3333-3333-3333-333333333333');
        }
    });
});

describe('OresClient authenticated calls', () => {
    const accountsReply = {
        accounts: [],
        total_available_count: 0,
    };

    it('refuses an authenticated call before login', async () => {
        const transport = new ScriptedTransport({});
        const client = new OresClient({ transport });

        await expect(client.listAccounts()).rejects.toBeInstanceOf(NotAuthenticatedError);
        expect(transport.calls).toHaveLength(0);
    });

    it('carries the bearer token and a correlation id on each call', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.accounts.list': [{ body: accountsReply }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.listAccounts({ limit: 5 });

        const headers = transport.calls[1]?.headers ?? {};
        expect(headers['Authorization']).toBe('Bearer token-one');
        expect(headers['Nats-Session-Id']).toBe('33333333-3333-3333-3333-333333333333');
        expect(headers['Nats-Correlation-Id']).toMatch(/^[0-9a-f-]{36}$/);
    });

    it('sends offset, limit and an order even when the caller omits them', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.accounts.list': [{ body: accountsReply }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.listAccounts();

        // The server does not apply defaults on a missing field, so the client
        // must always write every declared field. An empty order field means the
        // key, which is what makes a page of an unordered set reproducible.
        expect(transport.decodeCall(1)).toEqual({
            offset: 0,
            limit: 100,
            order: { field: '', descending: false },
            as_of: null,
            filter: null,
        });
    });

    it('reads the services roster, and sends the empty request it declares', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.ops.get_service_roster': [
                {
                    body: {
                        success: true,
                        message: '',
                        slots: [
                            {
                                service_name: 'ores.iam.service',
                                state: 'running',
                                slot: 1,
                                instance_id: '91b0f33d-4b32-4f65-8809-2d3e4f506172',
                                version: 'v0.0.25',
                                sampled_at: '2026-10-04 14:32:00Z',
                            },
                        ],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const slots = await client.serviceRoster();

        expect(transport.calls[1]?.subject).toBe('telemetry.v1.ops.get_service_roster');
        expect(transport.decodeCall(1)).toEqual({});
        // A field the reply left out is stated as nothing rather than invented,
        // so a slot no instance fills cannot read as a reporting one.
        expect(slots).toEqual([
            {
                service_name: 'ores.iam.service',
                display_name: '',
                description: '',
                service_account: null,
                slot: 1,
                state: 'running',
                instance_id: '91b0f33d-4b32-4f65-8809-2d3e4f506172',
                host_id: null,
                version: 'v0.0.25',
                sampled_at: '2026-10-04 14:32:00Z',
            },
        ]);
    });

    it('refuses a roster the server did not read, rather than reading it as empty', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.ops.get_service_roster': [
                { body: { success: false, message: 'denied', slots: [] } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.serviceRoster()).rejects.toBeInstanceOf(OperationFailedError);
    });

    it('reads the grid summary and the node rows, and sends the empty request it declares', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'compute.v1.ops.get_grid_stats': [
                {
                    body: {
                        success: true,
                        message: '',
                        total_hosts: 2,
                        online_hosts: 1,
                        idle_hosts: 0,
                        total_workunits: 3,
                        total_batches: 1,
                        active_batches: 1,
                        outcomes_success: 4,
                        outcomes_client_error: 1,
                        outcomes_no_reply: 0,
                        sampled_at: '2026-10-04 14:31:02Z',
                        node_summaries: [
                            {
                                host_id: '9e0f33aa-0000-4000-8000-000000000001',
                                tasks_completed: 1284,
                                tasks_failed: 2,
                                tasks_since_last: 12,
                                avg_task_duration_ms: 42_000,
                                max_task_duration_ms: 51_000,
                                input_bytes_fetched: 1_288_490_188,
                                output_bytes_uploaded: 230_686_720,
                                seconds_since_hb: 8,
                            },
                        ],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const stats = await client.gridStats();

        expect(transport.calls[1]?.subject).toBe('compute.v1.ops.get_grid_stats');
        expect(transport.decodeCall(1)).toEqual({});
        // A counter the reply left out states zero rather than a made-up count.
        expect(stats.results_done).toBe(0);
        expect(stats.sampled_at).toBe('2026-10-04 14:31:02Z');
        // The failures and the slowest task travel on the node summary now.
        expect(stats.node_summaries[0]?.tasks_failed).toBe(2);
        expect(stats.node_summaries[0]?.max_task_duration_ms).toBe(51_000);
    });

    it('refuses a grid read the server did not produce, rather than reading it as idle', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'compute.v1.ops.get_grid_stats': [{ body: { success: false, message: 'denied' } }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.gridStats()).rejects.toBeInstanceOf(OperationFailedError);
    });

    it('reads the host registry page, and names the nodes by their external id', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'compute.v1.hosts.list': [
                {
                    body: {
                        result: { outcome: 'ok', code: '', message: '', fields: [] },
                        hosts: [
                            {
                                version: 1,
                                tenant_id: 'ffffffff-ffff-4fff-8fff-ffffffffffff',
                                id: '9e0f33aa-0000-4000-8000-000000000001',
                                external_id: 'grid-01.example.com',
                                display_name: 'Grid 01',
                                location: 'ldn',
                                cpu_count: 8,
                                ram_mb: 32768,
                                gpu_type: '',
                                credit_total: 100,
                                modified_by: 'probe',
                                performed_by: 'probe',
                                change_reason_code: 'new',
                                change_commentary: '',
                                recorded_at: '2026-10-04 14:31:02Z',
                            },
                        ],
                        total: 1,
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const hosts = await client.listHosts();

        expect(transport.calls[1]?.subject).toBe('compute.v1.hosts.list');
        expect(transport.decodeCall(1)).toEqual({
            offset: 0,
            limit: 1000,
            order: { field: '', descending: false },
            filter: null,
            as_of: null,
        });
        expect(hosts.map((host) => host.external_id)).toEqual(['grid-01.example.com']);
    });

    it('refuses a host page that did not end ok, rather than reading it as no hosts', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'compute.v1.hosts.list': [
                {
                    body: {
                        result: {
                            outcome: 'denied',
                            code: 'forbidden',
                            message: 'denied',
                            fields: [],
                        },
                        hosts: [],
                        total: 0,
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.listHosts()).rejects.toBeInstanceOf(OperationFailedError);
    });

    it('reads the NATS server samples, and sends both bounds of the range', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.nats_server_samples.list': [
                {
                    body: {
                        success: true,
                        message: '',
                        samples: [
                            {
                                sampled_at: '2026-10-04 14:31:45Z',
                                in_msgs: 1_240_512,
                                out_msgs: 3_410_882,
                                in_bytes: 220_200_960,
                                out_bytes: 1_181_167_616,
                                connections: 23,
                                mem_bytes: 88_080_384,
                                slow_consumers: 0,
                            },
                        ],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const samples = await client.natsServerSamples({
            startTime: '2026-10-04 13:31:45Z',
            endTime: '2026-10-04 14:31:45Z',
        });

        expect(transport.calls[1]?.subject).toBe('telemetry.v1.nats_server_samples.list');
        expect(transport.decodeCall(1)).toEqual({
            query: {
                start_time: '2026-10-04 13:31:45Z',
                end_time: '2026-10-04 14:31:45Z',
                limit: 1000,
            },
        });
        // A counter the reply left out states zero rather than a made-up count.
        expect(samples[0]?.in_msgs).toBe(1_240_512);
        expect(samples[0]?.out_bytes).toBe(1_181_167_616);
    });

    it('refuses a server sample read the server did not produce', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.nats_server_samples.list': [
                { body: { success: false, message: 'denied', samples: [] } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(
            client.natsServerSamples({
                startTime: '2026-10-04 13:31:45Z',
                endTime: '2026-10-04 14:31:45Z',
            }),
        ).rejects.toBeInstanceOf(OperationFailedError);
    });

    it('reads one stream’s samples, naming the stream and the range', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.nats_stream_samples.list': [
                {
                    body: {
                        success: true,
                        message: '',
                        samples: [
                            {
                                sampled_at: '2026-10-04 14:31:45Z',
                                stream_name: 'ores_dev_test_workflow',
                                messages: 12_004,
                                bytes: 88_080_384,
                                consumer_count: 2,
                            },
                        ],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const samples = await client.natsStreamSamples({
            streamName: 'ores_dev_test_workflow',
            startTime: '2026-10-04 13:31:45Z',
            endTime: '2026-10-04 14:31:45Z',
            limit: 100,
        });

        expect(transport.calls[1]?.subject).toBe('telemetry.v1.nats_stream_samples.list');
        expect(transport.decodeCall(1)).toEqual({
            query: {
                stream_name: 'ores_dev_test_workflow',
                start_time: '2026-10-04 13:31:45Z',
                end_time: '2026-10-04 14:31:45Z',
                limit: 100,
            },
        });
        expect(samples[0]?.stream_name).toBe('ores_dev_test_workflow');
        expect(samples[0]?.consumer_count).toBe(2);
    });

    it('refuses a stream sample read the server did not produce', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.nats_stream_samples.list': [
                { body: { success: false, message: 'denied', samples: [] } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(
            client.natsStreamSamples({
                streamName: 'ores_dev_test_workflow',
                startTime: '2026-10-04 13:31:45Z',
                endTime: '2026-10-04 14:31:45Z',
            }),
        ).rejects.toBeInstanceOf(OperationFailedError);
    });

    /** One log entry as the read answers it. */
    const logEntry = {
        id: '11111111-1111-4111-8111-111111111111',
        timestamp: '2026-10-04 14:31:02Z',
        source: 'server',
        source_name: 'ores.compute.service',
        session_id: null,
        account_id: null,
        level: 'error',
        component: 'ores.compute.poller',
        message: 'fetch failed, retrying',
        tag: 'compute.fetch',
        recorded_at: '2026-10-04 14:31:03Z',
    };

    it('reads the log entries a filter selects, and sends every query field', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.logs.list': [
                { body: { success: true, message: '', entries: [logEntry], total_count: 2431 } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const page = await client.listLogs({
            startTime: '2026-10-04 13:31:02Z',
            endTime: '2026-10-04 14:31:02Z',
            source: 'server',
            level: 'error',
            component: 'ores.compute.poller',
            tag: 'compute.fetch',
            messageContains: 'fetch',
            offset: 100,
            limit: 50,
        });

        expect(transport.calls[1]?.subject).toBe('telemetry.v1.logs.list');
        // The filters combine with AND in one request, and every declared field
        // is written: the server applies no defaults to a field left out.
        expect(transport.decodeCall(1)).toEqual({
            query: {
                start_time: '2026-10-04 13:31:02Z',
                end_time: '2026-10-04 14:31:02Z',
                source: 'server',
                source_name: null,
                session_id: null,
                account_id: null,
                level: 'error',
                min_level: null,
                component: 'ores.compute.poller',
                tag: 'compute.fetch',
                message_contains: 'fetch',
                limit: 50,
                offset: 100,
            },
        });
        // The total is the whole set, not the page.
        expect(page.totalCount).toBe(2431);
        expect(page.entries).toEqual([logEntry]);
    });

    it('turns a filter the caller left off into nothing rather than an empty match', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.logs.list': [
                { body: { success: true, message: '', entries: [], total_count: 0 } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.listLogs({
            startTime: '2026-10-04 14:16:02Z',
            endTime: '2026-10-04 14:31:02Z',
        });

        expect(transport.decodeCall(1)).toEqual({
            query: {
                start_time: '2026-10-04 14:16:02Z',
                end_time: '2026-10-04 14:31:02Z',
                source: null,
                source_name: null,
                session_id: null,
                account_id: null,
                level: null,
                min_level: null,
                component: null,
                tag: null,
                message_contains: null,
                limit: 100,
                offset: 0,
            },
        });
    });

    it('refuses a logs reply the server did not read', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'telemetry.v1.logs.list': [
                { body: { success: false, message: 'denied', entries: [], total_count: 0 } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(
            client.listLogs({
                startTime: '2026-10-04 14:16:02Z',
                endTime: '2026-10-04 14:31:02Z',
            }),
        ).rejects.toBeInstanceOf(OperationFailedError);
    });

    it('refreshes once and retries when the server reports an expired token', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.accounts.list': [
                { headers: { 'X-Error': 'token_expired' } },
                { body: accountsReply },
            ],
            'iam.v1.ops.refresh': [
                {
                    body: {
                        success: true,
                        token: 'token-two',
                        message: '',
                        access_lifetime_s: 1800,
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.listAccounts();

        const subjects = transport.calls.map((call) => call.subject);
        expect(subjects).toEqual([
            'iam.v1.ops.login',
            'iam.v1.accounts.list',
            'iam.v1.ops.refresh',
            'iam.v1.accounts.list',
        ]);
        // The retry must use the fresh token, not the expired one.
        expect(transport.calls[3]?.headers['Authorization']).toBe('Bearer token-two');
        const refreshHeaders = transport.calls[2]?.headers ?? {};
        expect(refreshHeaders['Authorization']).toBe('Bearer token-one');
    });

    it('fails the call when the refresh is refused', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.accounts.list': [{ headers: { 'X-Error': 'token_expired' } }],
            'iam.v1.ops.refresh': [
                { body: { success: false, token: '', message: 'max_session_exceeded' } },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.listAccounts()).rejects.toMatchObject({
            name: 'SessionExpiredError',
            code: 'max_session_exceeded',
        });
    });
});

/**
 * Entering a tenant swaps the session's token, and every way back restores the
 * token it entered from, so the session is never stranded inside a tenant.
 */
describe('OresClient reading inside a tenant', () => {
    const accountsReply = { accounts: [], total_available_count: 0 };
    const ACME = '44444444-4444-4444-4444-444444444444';
    const entered = {
        success: true,
        message: '',
        token: 'tenant-token',
        tenant_id: ACME,
        tenant_code: 'acme_corporation',
        tenant_name: 'Acme Corporation',
        party_id: '66666666-6666-6666-6666-666666666666',
        party_name: 'System Party',
        access_lifetime_s: 900,
    };
    const left = { success: true, message: '' };
    const anything = z.unknown();

    it('reads with the tenant token, leaves with it, and keeps its own', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.enter_tenant': [{ body: entered }],
            'iam.v1.accounts.list': [{ body: accountsReply }],
            'iam.v1.ops.leave_tenant': [{ body: left }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.readInsideTenant(ACME, (caller) =>
            caller.callAuthenticated('iam.v1.accounts.list', {}, anything),
        );

        expect(transport.calls.map((call) => call.subject)).toEqual([
            'iam.v1.ops.login',
            'iam.v1.ops.enter_tenant',
            'iam.v1.accounts.list',
            'iam.v1.ops.leave_tenant',
        ]);
        expect(transport.decodeCall(1)).toEqual({ tenant_id: ACME });
        expect(transport.calls[1]?.headers['Authorization']).toBe('Bearer token-one');
        expect(transport.calls[2]?.headers['Authorization']).toBe('Bearer tenant-token');
        expect(transport.calls[3]?.headers['Authorization']).toBe('Bearer tenant-token');
        expect(client.token).toBe('token-one');
    });

    /*
     * The read holds the tenant token itself, so a call the session makes while
     * the read is under way is still the session's own.
     */
    it('leaves a call the session makes meanwhile with its own token', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.enter_tenant': [{ body: entered }],
            'iam.v1.accounts.list': [{ body: accountsReply }],
            'iam.v1.tenants.list': [{ body: {} }],
            'iam.v1.ops.leave_tenant': [{ body: left }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await client.readInsideTenant(ACME, async (caller) => {
            await client.callAuthenticated('iam.v1.tenants.list', {}, anything);
            return caller.callAuthenticated('iam.v1.accounts.list', {}, anything);
        });

        const outside = transport.calls.find((call) => call.subject === 'iam.v1.tenants.list');
        expect(outside?.headers['Authorization']).toBe('Bearer token-one');
    });

    it('reads nothing and leaves nothing when the server refuses the entry', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.enter_tenant': [
                {
                    body: {
                        ...entered,
                        success: false,
                        token: '',
                        message: 'No tenant has this id.',
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });
        let read = false;

        await expect(
            client.readInsideTenant(ACME, async () => {
                read = true;
            }),
        ).rejects.toThrow(OperationFailedError);
        expect(read).toBe(false);
        expect(transport.calls.map((call) => call.subject)).not.toContain(
            'iam.v1.ops.leave_tenant',
        );
    });

    it('leaves the tenant when the read fails', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.enter_tenant': [{ body: entered }],
            'iam.v1.ops.leave_tenant': [{ body: left }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(
            client.readInsideTenant(ACME, () => Promise.reject(new Error('read failed'))),
        ).rejects.toThrow('read failed');
        expect(transport.calls.at(-1)?.subject).toBe('iam.v1.ops.leave_tenant');
        expect(client.token).toBe('token-one');
    });

    /*
     * A tenant session is never refreshed. A read that outlives it fails as an
     * ordinary failed read, not as an expired session, so the session it was
     * entered from is neither renewed nor ended on its account. The exit that
     * cannot be recorded is reported.
     */
    it('fails a read whose tenant session lapsed, without refreshing', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.ops.login': [{ body: loginReply() }],
            'iam.v1.ops.enter_tenant': [{ body: entered }],
            'iam.v1.accounts.list': [{ headers: { 'X-Error': 'token_expired' } }],
            'iam.v1.ops.leave_tenant': [{ headers: { 'X-Error': 'token_expired' } }],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        const exitFailures: unknown[] = [];
        const read = client.readInsideTenant(
            ACME,
            (caller) => caller.callAuthenticated('iam.v1.accounts.list', {}, anything),
            (error) => exitFailures.push(error),
        );

        await expect(read).rejects.toThrow(OperationFailedError);
        await expect(read).rejects.not.toThrow(SessionExpiredError);
        expect(exitFailures).toHaveLength(1);
        expect(transport.calls.map((call) => call.subject)).not.toContain('iam.v1.ops.refresh');
        expect(client.token).toBe('token-one');
    });
});

describe('the service heartbeat', () => {
    it('publishes the message on the heartbeat subject, encoded as every other body is', () => {
        const published: { relative: string; body: Uint8Array }[] = [];
        const transport: Transport = {
            request: () => Promise.reject(new Error('a heartbeat is not asked for')),
            close: () => Promise.resolve(),
            publish: (relative, body) => {
                published.push({ relative, body });
            },
        };
        const message = {
            service_name: 'ores.web.service',
            instance_id: '0197d2a1-0000-7000-8000-000000000001',
            host_id: '',
            version: '0.0.27',
        };

        new OresClient({ transport }).publishServiceHeartbeat(message);

        expect(published).toHaveLength(1);
        expect(published[0]?.relative).toBe('telemetry.v1.ops.service_heartbeat');
        expect(new WireCodec('msgpack').decode(published[0]?.body ?? new Uint8Array())).toEqual(
            message,
        );
    });

    it('does nothing on a transport that cannot publish', () => {
        const transport: Transport = {
            request: () => Promise.reject(new Error('unused')),
            close: () => Promise.resolve(),
        };

        expect(() =>
            new OresClient({ transport }).publishServiceHeartbeat({
                service_name: 'ores.web.service',
                instance_id: 'x',
                host_id: '',
                version: '0.0.27',
            }),
        ).not.toThrow();
    });
});

describe('listening to published events', () => {
    it('hands on the time of the change with the tenancy the envelope names', () => {
        const heard: unknown[] = [];
        let deliver: ((payload: Uint8Array, envelope: MessageEnvelope) => void) | undefined;
        const transport = {
            async request(): Promise<Reply> {
                throw new Error('not used');
            },
            async close(): Promise<void> {
                return undefined;
            },
            subscribe(
                _relative: string,
                onMessage: (payload: Uint8Array, envelope: MessageEnvelope) => void,
            ) {
                deliver = onMessage;
                return () => undefined;
            },
        } as unknown as Transport;
        const client = new OresClient({ transport });

        client.subscribeToEvents('iam.v1.accounts_events.>', (change) => heard.push(change));
        const payload = new WireCodec('msgpack').encode({
            event_id: '44444444-4444-4444-4444-444444444444',
            action: 'updated',
            occurred_at: '2026-10-10T12:00:03Z',
        });
        deliver?.(payload, { tenantId: 't1', partyId: 'p1' });
        deliver?.(payload, { tenantId: undefined, partyId: undefined });

        expect(heard).toEqual([
            { at: '2026-10-10T12:00:03Z', tenantId: 't1', partyId: 'p1' },
            { at: '2026-10-10T12:00:03Z', tenantId: undefined, partyId: undefined },
        ]);
    });
});
