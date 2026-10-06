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
import { WireCodec } from './codec.js';
import { OperationFailedError } from './errors.js';
import type { Reply, RequestHeaders, Transport } from './transport.js';

interface RecordedCall {
    readonly subject: string;
    readonly body: Uint8Array;
}

interface ScriptedReply {
    readonly body?: unknown;
}

/** A transport that answers from a script and records what it received. */
class ScriptedTransport implements Transport {
    readonly calls: RecordedCall[] = [];
    readonly #script: Map<string, ScriptedReply[]>;
    readonly #codec = new WireCodec('msgpack');

    constructor(script: Record<string, ScriptedReply[]>) {
        this.#script = new Map(Object.entries(script));
    }

    async request(
        subject: string,
        body: Uint8Array,
        _headers: RequestHeaders,
        _timeoutMs: number,
    ): Promise<Reply> {
        this.calls.push({ subject, body });

        const queue = this.#script.get(subject);
        const next = queue?.shift();
        if (next === undefined) {
            throw new Error(`no scripted reply left for ${subject}`);
        }
        const encoded = next.body === undefined ? new Uint8Array() : this.#codec.encode(next.body);
        return { subject, body: encoded, headers: {} };
    }

    async close(): Promise<void> {
        return Promise.resolve();
    }

    decodeCall(index: number): unknown {
        const call = this.calls[index];
        if (call === undefined) {
            throw new Error(`no call at index ${index}`);
        }
        return this.#codec.decode(call.body);
    }
}

function loginReply(): Record<string, unknown> {
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
    };
}

const ok = { outcome: 'ok', code: '', message: '' };

const emptyOperationalId = '1bf0711f-ea4d-4465-87f3-54601f68ee4c';
const acmeDemoId = '650550c6-8c4f-4b14-bef7-c78a42bc4d19';

/**
 * The two seeded profiles answer the profile subject in key order, which is
 * not the order the cards are offered in: that is the point of the sort.
 * Their copy is the accepted prototype's.
 */
const profilesReply = {
    result: ok,
    total: 2,
    seed_profiles: [
        {
            id: acmeDemoId,
            code: 'acme_demo',
            name: 'ACME demo',
            summary: 'Pre-configured sandbox',
            audience: 'For demos and testing',
            bullets_json: JSON.stringify([
                '4 legal entities, books and desks',
                '45 staff to sign in as',
                'Live synthetic market data',
            ]),
            tenant_name: 'Acme Corporation',
            tenant_code: 'acme_corporation',
            tenant_hostname: 'acme_corporation',
            admin_username: 'tenant_admin',
            admin_email: 'admin@acme_corporation.com',
            inherits_admin_password: true,
            force_password_change: false,
            display_order: 20,
        },
        {
            id: emptyOperationalId,
            code: 'empty_operational',
            name: 'Operational',
            summary: 'Production-ready setup',
            audience: 'For real use',
            bullets_json: JSON.stringify([
                'Standard reference data and counterparties',
                'Your legal entities, from their LEI',
                'No test data',
            ]),
            tenant_name: '',
            tenant_code: '',
            tenant_hostname: '',
            admin_username: '',
            admin_email: '',
            inherits_admin_password: false,
            force_password_change: true,
            display_order: 10,
        },
    ],
};

const acmeStepsReply = {
    result: ok,
    total: 2,
    seed_profile_steps: [
        { step_kind: 'publish_bundle', display_order: 10 },
        { step_kind: 'load_staff', display_order: 40 },
    ],
};

const acmeParametersReply = {
    result: ok,
    total: 0,
    seed_profile_parameters: [],
};

const emptyStepsReply = {
    result: ok,
    total: 1,
    seed_profile_steps: [{ step_kind: 'publish_bundle', display_order: 10 }],
};

const emptyParametersReply = {
    result: ok,
    total: 2,
    seed_profile_parameters: [
        {
            name: 'root_lei',
            label: 'Root LEI',
            data_type: 'string',
            // A nullable jsonb column reaches the wire as an empty string.
            choices_json: '',
            default_value: '',
            is_required: true,
            description:
                "The LEI of the top legal entity. Its GLEIF hierarchy becomes the tenant's parties.",
            display_order: 10,
        },
        {
            name: 'counterparty_size',
            label: 'Counterparty set',
            data_type: 'choice',
            choices_json: JSON.stringify(['small', 'large']),
            default_value: 'small',
            is_required: true,
            description: 'small is about 13k GLEIF counterparties; large is about 500k.',
            display_order: 20,
        },
    ],
};

function scriptedClient(): { client: OresClient; transport: ScriptedTransport } {
    const transport = new ScriptedTransport({
        'iam.v1.auth.login': [{ body: loginReply() }],
        'iam.v1.seed_profiles.list': [{ body: profilesReply }],
        'iam.v1.seed_profile_steps.list_by_seed_profile_id': [
            { body: acmeStepsReply },
            { body: emptyStepsReply },
        ],
        'iam.v1.seed_profile_parameters.list_by_seed_profile_id': [
            { body: acmeParametersReply },
            { body: emptyParametersReply },
        ],
    });
    return { client: new OresClient({ transport }), transport };
}

describe('OresClient seed profiles', () => {
    it('joins the three reads and orders the answer by the declared order', async () => {
        const { client, transport } = scriptedClient();
        await client.login({ principal: 'probe', password: 'secret' });

        const profiles = await client.seedProfiles();

        expect(profiles).toEqual([
            {
                code: 'empty_operational',
                name: 'Operational',
                summary: 'Production-ready setup',
                audience: 'For real use',
                bullets: [
                    'Standard reference data and counterparties',
                    'Your legal entities, from their LEI',
                    'No test data',
                ],
                tenant: {
                    name: '',
                    code: '',
                    hostname: '',
                    adminUsername: '',
                    adminEmail: '',
                },
                inheritsAdminPassword: false,
                forcePasswordChange: true,
                order: 10,
                steps: [{ kind: 'publish_bundle', order: 10 }],
                parameters: [
                    {
                        name: 'root_lei',
                        label: 'Root LEI',
                        dataType: 'string',
                        choices: [],
                        defaultValue: '',
                        required: true,
                        hint: "The LEI of the top legal entity. Its GLEIF hierarchy becomes the tenant's parties.",
                        order: 10,
                    },
                    {
                        name: 'counterparty_size',
                        label: 'Counterparty set',
                        dataType: 'choice',
                        choices: ['small', 'large'],
                        defaultValue: 'small',
                        required: true,
                        hint: 'small is about 13k GLEIF counterparties; large is about 500k.',
                        order: 20,
                    },
                ],
            },
            {
                code: 'acme_demo',
                name: 'ACME demo',
                summary: 'Pre-configured sandbox',
                audience: 'For demos and testing',
                bullets: [
                    '4 legal entities, books and desks',
                    '45 staff to sign in as',
                    'Live synthetic market data',
                ],
                tenant: {
                    name: 'Acme Corporation',
                    code: 'acme_corporation',
                    hostname: 'acme_corporation',
                    adminUsername: 'tenant_admin',
                    adminEmail: 'admin@acme_corporation.com',
                },
                inheritsAdminPassword: true,
                forcePasswordChange: false,
                order: 20,
                steps: [
                    { kind: 'publish_bundle', order: 10 },
                    { kind: 'load_staff', order: 40 },
                ],
                parameters: [],
            },
        ]);

        expect(transport.calls.map((call) => call.subject)).toEqual([
            'iam.v1.auth.login',
            'iam.v1.seed_profiles.list',
            'iam.v1.seed_profile_steps.list_by_seed_profile_id',
            'iam.v1.seed_profile_parameters.list_by_seed_profile_id',
            'iam.v1.seed_profile_steps.list_by_seed_profile_id',
            'iam.v1.seed_profile_parameters.list_by_seed_profile_id',
        ]);
    });

    it('writes every field the two request shapes declare', async () => {
        const { client, transport } = scriptedClient();
        await client.login({ principal: 'probe', password: 'secret' });

        await client.seedProfiles();

        expect(transport.decodeCall(1)).toEqual({
            offset: 0,
            limit: 50,
            order: { field: '', descending: false },
            filter: null,
            as_of: null,
        });
        // The child read names the profile the answer listed first, which is
        // the key order the server returned rather than the card order.
        expect(transport.decodeCall(2)).toEqual({
            seed_profile_id: acmeDemoId,
            scope: 'direct',
            offset: 0,
            limit: 200,
            order: { field: '', descending: false },
            filter: null,
        });
        expect(transport.decodeCall(4)).toEqual({
            seed_profile_id: emptyOperationalId,
            scope: 'direct',
            offset: 0,
            limit: 200,
            order: { field: '', descending: false },
            filter: null,
        });
    });

    it('refuses a bullet list the row does not hold as a list of strings', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [{ body: loginReply() }],
            'iam.v1.seed_profiles.list': [
                {
                    body: {
                        result: ok,
                        total: 1,
                        seed_profiles: [
                            {
                                id: emptyOperationalId,
                                code: 'empty_operational',
                                name: 'Operational',
                                summary: 'Production-ready setup',
                                audience: 'For real use',
                                bullets_json: JSON.stringify({ not: 'a list' }),
                                tenant_name: '',
                                tenant_code: '',
                                tenant_hostname: '',
                                admin_username: '',
                                admin_email: '',
                                inherits_admin_password: false,
                                force_password_change: true,
                                display_order: 10,
                            },
                        ],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.seedProfiles()).rejects.toThrow();
    });

    it('fails rather than answering with the rows a refused read carried', async () => {
        const transport = new ScriptedTransport({
            'iam.v1.auth.login': [{ body: loginReply() }],
            'iam.v1.seed_profiles.list': [
                {
                    body: {
                        result: { outcome: 'denied', code: 'forbidden', message: 'Not permitted' },
                        total: 0,
                        seed_profiles: [],
                    },
                },
            ],
        });
        const client = new OresClient({ transport });
        await client.login({ principal: 'probe', password: 'secret' });

        await expect(client.seedProfiles()).rejects.toBeInstanceOf(OperationFailedError);
    });
});
